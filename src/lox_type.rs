use std::fmt::Display;

use anyhow::{bail, Result};

use crate::ast::Statement;

#[derive(Debug, Clone)]
pub enum LoxType {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Function(Statement),
    NativeFunction {
        name: String,
        arity: usize,
        func: fn(Vec<LoxType>) -> LoxType,
    },
}

impl LoxType {
    pub fn is_truthy(&self) -> bool {
        match self {
            LoxType::Nil => false,
            LoxType::Boolean(b) => *b,
            _ => true,
        }
    }

    pub fn eq(&self, other: &Self) -> Result<bool> {
        Ok(match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::Nil, Self::Nil) => true,
            _ => bail!("== not defined for {} {}", self, other),
        }
        .into())
    }

    pub fn neq(&self, other: &Self) -> Result<bool> {
        let result = self.eq(other)?;
        Ok(!result)
    }

    pub fn gt(&self, other: &Self) -> Result<bool> {
        match (self, other) {
            (LoxType::Number(l), LoxType::Number(r)) => Ok(l > r),
            _ => bail!("> only defined for Number."),
        }
    }

    pub fn gte(&self, other: &Self) -> Result<bool> {
        match (self, other) {
            (LoxType::Number(l), LoxType::Number(r)) => Ok(l >= r),
            _ => bail!(">= only defined for Number."),
        }
    }

    pub fn lt(&self, other: &Self) -> Result<bool> {
        match (self, other) {
            (LoxType::Number(l), LoxType::Number(r)) => Ok(l < r),
            _ => bail!("< only defined for Number."),
        }
    }

    pub fn lte(&self, other: &Self) -> Result<bool> {
        match (self, other) {
            (LoxType::Number(l), LoxType::Number(r)) => Ok(l <= r),
            _ => bail!("<= only defined for Number."),
        }
    }
}

impl std::ops::Add for LoxType {
    type Output = Result<Self>;

    fn add(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (LoxType::Number(l), LoxType::Number(r)) => LoxType::Number(l + r),
            (LoxType::String(l), LoxType::String(r)) => LoxType::String(format!("{}{}", l, r)),
            _ => bail!("Invalid addition on LoxType."),
        })
    }
}

impl std::ops::Sub for LoxType {
    type Output = Result<Self>;

    fn sub(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (LoxType::Number(l), LoxType::Number(r)) => LoxType::Number(l - r),
            _ => bail!("Invalid addition on LoxType."),
        })
    }
}

impl std::ops::Mul for LoxType {
    type Output = Result<Self>;

    fn mul(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (LoxType::Number(l), LoxType::Number(r)) => LoxType::Number(l * r),
            _ => bail!("Invalid addition on LoxType."),
        })
    }
}

impl std::ops::Div for LoxType {
    type Output = Result<Self>;

    fn div(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (LoxType::Number(l), LoxType::Number(r)) => LoxType::Number(l / r),
            _ => bail!("Invalid addition on LoxType."),
        })
    }
}

impl Display for LoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxType::Number(n) => write!(f, "{}", n),
            LoxType::String(s) => write!(f, "{}", s),
            LoxType::Boolean(b) => write!(f, "{}", b),
            LoxType::Nil => write!(f, "nil"),
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
