use std::convert::TryFrom;

#[allow(non_camel_case_types)]
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    OP_CONSTANT,
    OP_RETURN,
}

impl OpCode {
    pub fn to_byte(self) -> u8 {
        self as u8
    }
}

impl TryFrom<u8> for OpCode {
    type Error = String;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(OpCode::OP_CONSTANT),
            1 => Ok(OpCode::OP_RETURN),
            _ => Err(format!("Invalid OpCode value: {}", value)),
        }
    }
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_string() {
        assert_eq!("OP_RETURN".to_string(), OpCode::OP_RETURN.to_string());
    }
}
