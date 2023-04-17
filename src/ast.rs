use std::fmt::Display;

use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct Program(pub Statement);

#[derive(Debug, Clone)]
pub struct Literal(pub TokenType);

#[derive(Debug, Clone)]
pub struct Identifier(pub TokenType);

#[derive(Debug, Clone)]
pub struct UnaryOperator(pub TokenType);

#[derive(Debug, Clone)]
pub struct Operator(pub TokenType);

#[derive(Debug, Clone)]
pub enum Expression {
    Unary(UnaryOperator, Box<Expression>),
    Binary(Box<Expression>, Operator, Box<Expression>),
    Logical(Box<Expression>, Operator, Box<Expression>),
    Grouping(Box<Expression>),
    Literal(Literal),
    Assignment(Identifier, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>)
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Print(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Block(Vec<Declaration>),
    While(Expression, Box<Statement>),
    Function(Identifier, Vec<Identifier>, Box<Statement>)
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Variable(Identifier, Option<Expression>),
    Statement(Statement),
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Variable(identifier, expression) => write!(
                f,
                "(var {} {})",
                identifier.0,
                expression
                    .as_ref()
                    .map_or("None".to_string(), |f| f.to_string())
            ),
            Declaration::Statement(statement) => write!(f, "{}", statement),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expression(expression) => write!(f, "{}", expression),
            Statement::Print(expression) => write!(f, "(print {})", expression),
            Statement::Block(statements) => write!(f, "{{ {:?} }}", statements),
            _ => write!(f, "Not implemented"),
        }
    }
}

impl From<Statement> for Declaration {
    fn from(value: Statement) -> Self {
        Declaration::Statement(value)
    }
}

impl From<Expression> for Statement {
    fn from(value: Expression) -> Self {
        Statement::Expression(value)
    }
}

impl From<Expression> for Declaration {
    fn from(value: Expression) -> Self {
        Declaration::Statement(value.into())
    }
}

fn parenthesize(name: &str, expressions: &[&Expression]) -> String {
    let mut result = String::new();
    result.push_str("(");
    result.push_str(name);
    for expression in expressions {
        result.push_str(" ");
        result.push_str(&expression.to_string());
    }
    result.push_str(")");
    result
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expression::Unary(operator, expression) => {
                    parenthesize(&operator.0.to_string(), &[expression])
                }
                Expression::Binary(left, operator, right) => {
                    parenthesize(&operator.0.to_string(), &[left, right])
                }
                Expression::Logical(left, operator, right) => {
                    parenthesize(&operator.0.to_string(), &[left, right])
                }
                Expression::Grouping(expression) => parenthesize("group", &[expression]),
                Expression::Literal(literal) => literal.0.to_string(),
                Expression::Assignment(identifier, expression) =>
                    parenthesize(&identifier.0.to_string(), &[expression]),
                _ => todo!("Not implemented")
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_unary() {
        let expression = Expression::Unary(
            UnaryOperator(TokenType::Minus),
            Expression::Literal(Literal(TokenType::Number(1.0))).into(),
        );
        assert_eq!(expression.to_string(), "(- 1)");
    }

    #[test]
    fn test_display_binary() {
        let expression = Expression::Binary(
            Expression::Literal(Literal(TokenType::Number(1.0))).into(),
            Operator(TokenType::Plus),
            Expression::Literal(Literal(TokenType::Number(2.0))).into(),
        );
        assert_eq!(expression.to_string(), "(+ 1 2)");
    }

    #[test]
    fn test_display_grouping() {
        let expression =
            Expression::Grouping(Expression::Literal(Literal(TokenType::Number(1.0))).into());
        assert_eq!(expression.to_string(), "(group 1)");
    }

    #[test]
    fn test_display_literal() {
        let expression = Expression::Literal(Literal(TokenType::Number(1.0)));
        assert_eq!(expression.to_string(), "1");
    }

    #[test]
    fn test_display_complex() {
        let expression = Expression::Binary(
            Expression::Unary(
                UnaryOperator(TokenType::Minus),
                Expression::Literal(Literal(TokenType::Number(123.0))).into(),
            )
            .into(),
            Operator(TokenType::Star),
            Expression::Grouping(Expression::Literal(Literal(TokenType::Number(45.67))).into())
                .into(),
        );
        assert_eq!(expression.to_string(), "(* (- 123) (group 45.67))");
    }
}
