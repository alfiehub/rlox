use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::{ast::Statement, interpreter::Environment};

#[derive(Default, Debug, Clone)]
pub enum LoxType {
    Number(f64),
    String(String),
    Boolean(bool),
    #[default]
    Nil,
    Function(Statement, Rc<RefCell<Environment>>),
    NativeFunction {
        name: String,
        arity: usize,
        func: fn(Vec<LoxType>) -> LoxType,
    },
    Class {
        name: String,
    },
}

#[derive(Debug)]
pub struct LoxTypeError(pub String);

macro_rules! lox_type_bail {
    ($msg:expr) => {
        return Err(LoxTypeError($msg.to_string()))
    };
}

type LoxTypeResult<T> = Result<T, LoxTypeError>;

impl LoxType {
    pub fn is_truthy(&self) -> bool {
        match self {
            LoxType::Nil => false,
            LoxType::Boolean(b) => *b,
            _ => true,
        }
    }

    pub fn eq(&self, other: &Self) -> LoxTypeResult<bool> {
        Ok(match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::Nil, Self::Nil) => true,
            _ => lox_type_bail!(format!("== not defined for {} {}", self, other)),
        }
        .into())
    }

    pub fn neq(&self, other: &Self) -> LoxTypeResult<bool> {
        let result = self.eq(other)?;
        Ok(!result)
    }

    pub fn gt(&self, other: &Self) -> LoxTypeResult<bool> {
        match (self, other) {
            (LoxType::Number(l), LoxType::Number(r)) => Ok(l > r),
            _ => lox_type_bail!("> only defined for Number."),
        }
    }

    pub fn gte(&self, other: &Self) -> LoxTypeResult<bool> {
        match (self, other) {
            (LoxType::Number(l), LoxType::Number(r)) => Ok(l >= r),
            _ => lox_type_bail!(">= only defined for Number."),
        }
    }

    pub fn lt(&self, other: &Self) -> LoxTypeResult<bool> {
        match (self, other) {
            (LoxType::Number(l), LoxType::Number(r)) => Ok(l < r),
            _ => lox_type_bail!("< only defined for Number."),
        }
    }

    pub fn lte(&self, other: &Self) -> LoxTypeResult<bool> {
        match (self, other) {
            (LoxType::Number(l), LoxType::Number(r)) => Ok(l <= r),
            _ => lox_type_bail!("<= only defined for Number."),
        }
    }
}

impl std::ops::Add for LoxType {
    type Output = LoxTypeResult<Self>;

    fn add(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (LoxType::Number(l), LoxType::Number(r)) => LoxType::Number(l + r),
            (LoxType::String(l), LoxType::String(r)) => LoxType::String(format!("{}{}", l, r)),
            _ => lox_type_bail!("Invalid addition on LoxType."),
        })
    }
}

impl std::ops::Sub for LoxType {
    type Output = LoxTypeResult<Self>;

    fn sub(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (LoxType::Number(l), LoxType::Number(r)) => LoxType::Number(l - r),
            _ => lox_type_bail!("Invalid addition on LoxType."),
        })
    }
}

impl std::ops::Mul for LoxType {
    type Output = LoxTypeResult<Self>;

    fn mul(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (LoxType::Number(l), LoxType::Number(r)) => LoxType::Number(l * r),
            _ => lox_type_bail!("Invalid addition on LoxType."),
        })
    }
}

impl std::ops::Div for LoxType {
    type Output = LoxTypeResult<Self>;

    fn div(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (LoxType::Number(l), LoxType::Number(r)) => LoxType::Number(l / r),
            _ => lox_type_bail!("Invalid addition on LoxType."),
        })
    }
}

impl Display for LoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxType::Number(n) => write!(f, "{n}"),
            LoxType::String(s) => write!(f, "{s}"),
            LoxType::Boolean(b) => write!(f, "{b}"),
            LoxType::Nil => write!(f, "nil"),
            LoxType::Class { name, .. } => write!(f, "{name}"),
            _ => todo!("Not implemented"),
        }
    }
}

impl From<f64> for LoxType {
    fn from(value: f64) -> Self {
        LoxType::Number(value)
    }
}

impl From<bool> for LoxType {
    fn from(value: bool) -> Self {
        LoxType::Boolean(value)
    }
}
