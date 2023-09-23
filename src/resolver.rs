use std::collections::HashMap;

use crate::{
    ast::{Expression, Identifier, Statement},
    interpreter::Interpreter,
    visitor::Visitor,
};

pub struct Resolver<'a, T: std::io::Write> {
    interpreter: &'a mut Interpreter<T>,
    scopes: Vec<HashMap<String, bool>>,
}

impl<'a, T: std::io::Write> Resolver<'a, T> {
    pub fn new(interpreter: &'a mut Interpreter<T>) -> Self {
        Self {
            scopes: vec![],
            interpreter,
        }
    }
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, ident: Identifier) -> Result<(), ResolverError> {
        if let Some(current_scope) = self.scopes.last_mut() {
            if current_scope.contains_key(&ident.to_string()) {
                return Err(ResolverError::Redeclaration);
            }
            current_scope.insert(ident.to_string(), false);
        }
        Ok(())
    }

    fn define(&mut self, ident: Identifier) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(ident.to_string(), true);
        }
    }

    fn resolve_local(&mut self, ident: Identifier) {
        for (n, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&ident.to_string()) {
                self.interpreter.resolve(ident, self.scopes.len() - n - 1);
                return;
            }
        }
    }

    fn resolve_function(&mut self, fun: Statement) -> Result<(), ResolverError> {
        self.begin_scope();
        if let Statement::Function(_, args, body) = fun {
            for arg in args.into_iter() {
                self.declare(arg.clone())?;
                self.define(arg);
            }
            self.visit_statement(*body)?;
        }
        self.end_scope();
        Ok(())
    }

    pub fn resolve(&mut self, stmts: Vec<Statement>) -> Result<(), ResolverError> {
        for stmt in stmts {
            self.visit_statement(stmt)?;
        }
        Ok(())
    }
}

#[derive(Eq, PartialEq, Debug, thiserror::Error)]
pub enum ResolverError {
    #[error("Can't read local variable in its own initializer.")]
    SelfInitialize,
    #[error("Already a variable with this name in this scope.")]
    Redeclaration,
}

#[allow(unused_variables)]
impl<T: std::io::Write> Visitor<Result<(), ResolverError>> for Resolver<'_, T> {
    fn visit_statement(&mut self, stmt: Statement) -> Result<(), ResolverError> {
        // TODO: avoid clone
        match stmt.clone() {
            Statement::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts.into_iter() {
                    self.visit_statement(stmt)?;
                }
                self.end_scope();
            }
            Statement::Variable(ident, init) => {
                self.declare(ident.clone())?;
                if let Some(init) = init {
                    self.visit_expression(init)?;
                }
                self.define(ident);
            }
            Statement::Function(ident, _, body) => {
                self.declare(ident.clone())?;
                self.define(ident.clone());
                self.resolve_function(stmt)?;
            }
            Statement::Expression(expr) => {
                self.visit_expression(expr)?;
            }
            Statement::If(cond, then_branch, else_branch) => {
                self.visit_expression(cond)?;
                self.visit_statement(*then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.visit_statement(*else_branch)?;
                }
            }
            Statement::Print(expr) => {
                self.visit_expression(expr)?;
            }
            Statement::While(cond, body) => {
                self.visit_expression(cond)?;
                self.visit_statement(*body)?;
            }
            Statement::Return(Some(expr)) => {
                self.visit_expression(expr)?;
            }
            _ => {}
        };
        Ok(())
    }

    fn visit_expression(&mut self, expr: Expression) -> Result<(), ResolverError> {
        match &expr {
            Expression::Assign(ident, init) => {
                self.visit_expression(*init.clone())?;
                self.resolve_local(ident.clone());
            }
            Expression::Binary(left, _, right) => {
                self.visit_expression(*left.clone())?;
                self.visit_expression(*right.clone())?;
            }
            Expression::Grouping(expr) => {
                self.visit_expression(*expr.clone())?;
            }
            Expression::Logical(left, _, right) => {
                self.visit_expression(*left.clone())?;
                self.visit_expression(*right.clone())?;
            }
            Expression::Unary(_, expr) => {
                self.visit_expression(*expr.clone())?;
            }
            Expression::Variable(ident) => {
                if let Some(current_scope) = self.scopes.last() {
                    if let Some(initialized) = current_scope.get(&ident.to_string()) {
                        if !initialized {
                            return Err(ResolverError::SelfInitialize);
                        }
                    }
                }
                self.resolve_local(ident.clone());
            }
            Expression::Call(f, _) => {
                self.visit_expression(*f.clone())?;
            }
            _ => {}
        };
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::IdentifierKey, interpreter::Interpreter, parse, resolver::ResolverError};

    use super::Resolver;

    #[test]
    fn test_resolver() {
        let program = parse!(
            "var a = \"outer\";
            {
              var c = a;
              {
                  var b = 1;
                  {
                      var a = c;
                      var x = a;
                  }
                  var e = a;
                  var f = c;
              }
            }
            "
        );
        let mut interpreter = Interpreter::new();
        let mut resolver = Resolver::new(&mut interpreter);
        resolver.resolve(program).unwrap();
        let locals = interpreter.locals;
        assert_eq!(locals.get(&IdentifierKey("a@3".into())), None);
        assert_eq!(locals.get(&IdentifierKey("c@7".into())), Some(&2));
        assert_eq!(locals.get(&IdentifierKey("a@8".into())), Some(&0));
        assert_eq!(locals.get(&IdentifierKey("a@10".into())), None);
        assert_eq!(locals.get(&IdentifierKey("c@11".into())), Some(&1));
    }

    #[test]
    fn test_self_initialize() {
        let program = parse!(
            "
            var a = 69;
            {
                var a = a;
            }"
        );
        let mut interpreter = Interpreter::new();
        let mut resolver = Resolver::new(&mut interpreter);
        let error = resolver.resolve(program).unwrap_err();
        assert_eq!(error, ResolverError::SelfInitialize)
    }

    #[test]
    fn test_global_redeclaration() {
        let program = parse!(
            "
            // Global scope is allowed
            var a = 69;
            var a = 70;
            "
        );
        let mut interpreter = Interpreter::new();
        let mut resolver = Resolver::new(&mut interpreter);
        assert!(resolver.resolve(program).is_ok(), "Global redeclartions are allowed.");
    }

    #[test]
    fn test_local_redeclaration() {
        let program = parse!(
            "
            {
                var a = 70;
                var a = 69;
            }
            "
        );
        let mut interpreter = Interpreter::new();
        let mut resolver = Resolver::new(&mut interpreter);
        let error = resolver.resolve(program).unwrap_err();
        assert_eq!(error, ResolverError::Redeclaration)
    }
}
