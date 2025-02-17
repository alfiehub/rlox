use std::fmt::Display;

use crate::token::Token;

#[allow(dead_code)]
#[derive(Debug)]
pub struct Program(pub Statement);

macro_rules! single_token_display {
    ($type:ty) => {
        impl std::fmt::Display for $type {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };
}

#[derive(Debug, Clone)]
pub struct Literal(pub Token);
single_token_display!(Literal);

#[derive(Debug, Clone)]
pub struct Identifier(pub Token);
single_token_display!(Identifier);

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct IdentifierKey(pub String);
impl From<Identifier> for IdentifierKey {
    fn from(value: Identifier) -> Self {
        IdentifierKey(format!("{}@{}", value, value.0.line))
    }
}
impl From<&Identifier> for IdentifierKey {
    fn from(value: &Identifier) -> Self {
        IdentifierKey(format!("{}@{}", value, value.0.line))
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOperator(pub Token);
single_token_display!(UnaryOperator);

#[derive(Debug, Clone)]
pub struct Operator(pub Token);
single_token_display!(Operator);

#[derive(Debug, Clone)]
pub enum Expression {
    Assign(Identifier, Box<Expression>),
    Binary(Box<Expression>, Operator, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Grouping(Box<Expression>),
    Literal(Literal),
    Logical(Box<Expression>, Operator, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Variable(Identifier),
    Get(Box<Expression>, Identifier),
    Set(Box<Expression>, Identifier, Box<Expression>),
    Super(Identifier, Identifier),
    This(Identifier),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(Vec<Statement>),
    Class(Identifier, Option<Expression>, Vec<Statement>),
    Expression(Expression),
    Function(Identifier, Vec<Identifier>, Box<Statement>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Print(Expression),
    Return(Option<Expression>),
    While(Expression, Box<Statement>),
    Variable(Identifier, Option<Expression>),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expression(expression) => write!(f, "{}", expression),
            Statement::Print(expression) => write!(f, "(print {})", expression),
            Statement::Block(statements) => write!(f, "{{ {:?} }}", statements),

            Statement::Variable(identifier, expression) => write!(
                f,
                "(var {} {})",
                identifier.0,
                expression
                    .as_ref()
                    .map_or("None".to_string(), |f| f.to_string())
            ),
            _ => write!(f, "Not implemented"),
        }
    }
}

impl From<Expression> for Statement {
    fn from(value: Expression) -> Self {
        Statement::Expression(value)
    }
}

fn parenthesize(name: &str, expressions: &[&Expression]) -> String {
    let mut result = String::new();
    result.push('(');
    result.push_str(name);
    for expression in expressions {
        result.push(' ');
        result.push_str(&expression.to_string());
    }
    result.push(')');
    result
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expression::Unary(operator, expression) => {
                    parenthesize(&operator.0.token_type.to_string(), &[expression])
                }
                Expression::Binary(left, operator, right) => {
                    parenthesize(&operator.0.token_type.to_string(), &[left, right])
                }
                Expression::Logical(left, operator, right) => {
                    parenthesize(&operator.0.token_type.to_string(), &[left, right])
                }
                Expression::Grouping(expression) => parenthesize("group", &[expression]),
                Expression::Literal(literal) => literal.0.token_type.to_string(),
                Expression::Assign(identifier, expression) =>
                    parenthesize(&identifier.0.token_type.to_string(), &[expression]),
                Expression::This(ident) => ident.to_string(),
                _ => todo!("Not implemented"),
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::token::TokenType;

    use super::*;

    #[test]
    fn test_display_unary() {
        let expression = Expression::Unary(
            UnaryOperator(Token::new_empty(TokenType::Minus, 0)),
            Expression::Literal(Literal(Token::new_empty(TokenType::Number(1.0), 0))).into(),
        );
        assert_eq!(expression.to_string(), "(- 1)");
    }

    #[test]
    fn test_display_binary() {
        let expression = Expression::Binary(
            Expression::Literal(Literal(Token::new_empty(TokenType::Number(1.0), 0))).into(),
            Operator(Token::new_empty(TokenType::Plus, 0)),
            Expression::Literal(Literal(Token::new_empty(TokenType::Number(2.0), 0))).into(),
        );
        assert_eq!(expression.to_string(), "(+ 1 2)");
    }

    #[test]
    fn test_display_grouping() {
        let expression = Expression::Grouping(
            Expression::Literal(Literal(Token::new_empty(TokenType::Number(1.0), 0))).into(),
        );
        assert_eq!(expression.to_string(), "(group 1)");
    }

    #[test]
    fn test_display_literal() {
        let expression = Expression::Literal(Literal(Token::new_empty(TokenType::Number(1.0), 0)));
        assert_eq!(expression.to_string(), "1");
    }

    #[test]
    fn test_display_complex() {
        let expression = Expression::Binary(
            Expression::Unary(
                UnaryOperator(Token::new_empty(TokenType::Minus, 0)),
                Expression::Literal(Literal(Token::new_empty(TokenType::Number(123.0), 0))).into(),
            )
            .into(),
            Operator(Token::new_empty(TokenType::Star, 0)),
            Expression::Grouping(
                Expression::Literal(Literal(Token::new_empty(TokenType::Number(45.67), 0))).into(),
            )
            .into(),
        );
        assert_eq!(expression.to_string(), "(* (- 123) (group 45.67))");
    }
}
