use crate::ast::{Declaration, Expression, Statement};

pub trait Visitor<Return> {
    fn visit_declaration(&mut self, decl: Declaration) -> Return;
    fn visit_statement(&mut self, stmt: Statement) -> Return;
    fn visit_expression(&mut self, expr: Expression) -> Return;
}
