use crate::ast::{Expression, Statement};

pub trait Visitor<Return> {
    fn visit_statement(&mut self, stmt: Statement) -> Return;
    fn visit_expression(&mut self, expr: Expression) -> Return;
}
