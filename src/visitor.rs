use crate::ast::{
    Declaration, Expression, Identifier, Literal, Operator, Statement, UnaryOperator,
};

pub trait ExpressionVisitor<R: Default = (), E = ()> {
    fn visit_expression(&mut self, expr: Expression) -> Result<R, E> {
        match expr {
            Expression::Unary(op, expr) => self.unary(op, *expr),
            Expression::Binary(left_expr, op, right_expr) => {
                self.binary(*left_expr, op, *right_expr)
            }
            Expression::Logical(left_expr, op, right_expr) => {
                self.logical(*left_expr, op, *right_expr)
            }
            Expression::Grouping(expr) => self.grouping(*expr),
            Expression::Literal(lit) => self.literal(lit),
            Expression::Assignment(ident, expr) => self.assignment(ident, *expr),
            Expression::Call(f, args) => self.call(*f, args),
        }
    }

    fn unary(&mut self, _op: UnaryOperator, _expr: Expression) -> Result<R, E> {
        Ok(R::default())
    }
    fn binary(
        &mut self,
        _left_expr: Expression,
        _op: Operator,
        _right_expr: Expression,
    ) -> Result<R, E> {
        Ok(R::default())
    }
    fn logical(
        &mut self,
        _left_expr: Expression,
        _op: Operator,
        _right_expr: Expression,
    ) -> Result<R, E> {
        Ok(R::default())
    }
    fn grouping(&mut self, _expr: Expression) -> Result<R, E> {
        Ok(R::default())
    }
    fn literal(&mut self, _lit: Literal) -> Result<R, E> {
        Ok(R::default())
    }
    fn assignment(&mut self, _ident: Identifier, _expr: Expression) -> Result<R, E> {
        Ok(R::default())
    }
    fn call(&mut self, _f: Expression, _args: Vec<Expression>) -> Result<R, E> {
        Ok(R::default())
    }
}

pub trait StatementVisitor<R: Default = (), E = ()> {
    fn visit_statement(&mut self, stmt: Statement) -> Result<R, E> {
        match stmt {
            Statement::Expression(expr) => self.expression(expr),
            Statement::Print(expr) => self.print(expr),
            Statement::If(expr, stmt, else_stmt) => {
                self.if_statement(expr, *stmt, else_stmt.map(|s| *s))
            }
            Statement::Block(decls) => self.block(decls),
            Statement::While(expr, stmt) => self.while_statement(expr, *stmt),
            Statement::Function(ident, arg_idents, stmt) => self.function(ident, arg_idents, *stmt),
            Statement::Return(expr) => self.return_statement(expr),
        }
    }

    fn expression(&mut self, _expr: Expression) -> Result<R, E> {
        Ok(R::default())
    }

    fn print(&mut self, _expr: Expression) -> Result<R, E> {
        Ok(R::default())
    }

    fn if_statement(
        &mut self,
        _condition: Expression,
        _then_branch: Statement,
        _else_branch: Option<Statement>,
    ) -> Result<R, E> {
        Ok(R::default())
    }

    fn block(&mut self, _decls: Vec<Declaration>) -> Result<R, E> {
        Ok(R::default())
    }

    fn while_statement(&mut self, _expr: Expression, _stmt: Statement) -> Result<R, E> {
        Ok(R::default())
    }

    fn function(
        &mut self,
        _ident: Identifier,
        _arg_idents: Vec<Identifier>,
        _stmt: Statement,
    ) -> Result<R, E> {
        Ok(R::default())
    }

    fn return_statement(&mut self, _expr: Option<Expression>) -> Result<R, E> {
        Ok(R::default())
    }
}

pub trait DeclarationVisitor<R: Default = (), E = ()> {
    fn visit_declaration(&mut self, decl: Declaration) -> Result<R, E> {
        match decl {
            Declaration::Variable(ident, expr) => self.variable(ident, expr),
            Declaration::Statement(stmt) => self.statement(stmt),
        }
    }

    fn variable(&mut self, _ident: Identifier, _expr: Option<Expression>) -> Result<R, E> {
        Ok(R::default())
    }

    fn statement(&mut self, _stmt: Statement) -> Result<R, E> {
        Ok(R::default())
    }
}
