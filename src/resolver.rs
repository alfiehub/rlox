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
            scopes: vec![HashMap::new()],
            interpreter,
        }
    }
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, ident: Identifier) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(ident.to_string(), false);
        }
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

    fn resolve_function(&mut self, fun: Statement) -> Result<(), String> {
        self.begin_scope();
        if let Statement::Function(_, args, body) = fun {
            for arg in args.into_iter() {
                self.declare(arg.clone());
                self.define(arg);
            }
            self.visit_statement(*body)?;
        }
        self.end_scope();
        Ok(())
    }

    pub fn resolve(&mut self, stmts: Vec<Statement>) {
        for stmt in stmts {
            self.visit_statement(stmt).unwrap();
        }
    }
}

#[allow(unused_variables)]
impl<T: std::io::Write> Visitor<Result<(), String>> for Resolver<'_, T> {
    fn visit_statement(&mut self, stmt: Statement) -> Result<(), String> {
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
                self.declare(ident.clone());
                if let Some(init) = init {
                    self.visit_expression(init)?;
                }
                self.define(ident);
            }
            Statement::Function(ident, _, body) => {
                self.declare(ident.clone());
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
            _ => {}
        };
        Ok(())
    }

    fn visit_expression(&mut self, expr: Expression) -> Result<(), String> {
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
                            return Err("Can't read local variable in its own initializer.".to_string())
                        }
                    }
                }
                self.resolve_local(ident.clone());
            }
            _ => {}
        };
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{interpreter::Interpreter, parse, ast::IdentifierKey};

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
                  }
                  var e = a;
                  var f = c;
              }
            }
            "
        );
        let mut interpreter = Interpreter::new();
        let mut resolver = Resolver::new(&mut interpreter);
        resolver.resolve(program);
        assert_eq!(resolver.interpreter.locals.get(&IdentifierKey("a@3".into())), Some(&1));
        assert_eq!(resolver.interpreter.locals.get(&IdentifierKey("c@7".into())), Some(&2));
        assert_eq!(resolver.interpreter.locals.get(&IdentifierKey("a@9".into())), Some(&2));
        assert_eq!(resolver.interpreter.locals.get(&IdentifierKey("c@10".into())), Some(&1));
    }
}
