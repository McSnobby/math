use std::error::Error;
use std::fmt;
use std::num::ParseFloatError;

#[derive(Debug)]
pub struct DivByZeroError {
    description: String,
}

impl DivByZeroError {
    pub fn new() -> Self {
        Self {
            description: "Can not divide by zero".to_string(),
        }
    }
}

impl fmt::Display for DivByZeroError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Cant divide by zero")
    }
}

impl Error for DivByZeroError {
    fn description(&self) -> &str {
        &self.description
    }
}

#[derive(Debug)]
pub struct EvalError {
    details: String,
}

impl EvalError {
    pub fn new(msg: &str) -> Self {
        Self {
            details: msg.to_string(),
        }
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.details)
    }
}

impl Error for EvalError {
    fn cause(&self) -> Option<&dyn Error> {
        Some(self)
    }
    fn description(&self) -> &str {
        &self.details
    }
}

impl From<DivByZeroError> for EvalError {
    fn from(value: DivByZeroError) -> Self {
        Self::new(&value.to_string())
    }
}

#[derive(Debug)]
pub struct ParseError {
    details: String,
}

impl ParseError {
    pub fn new(msg: &str) -> Self {
        Self {
            details: msg.to_string(),
        }
    }
    pub fn with_format(args: fmt::Arguments<'_>) -> Self {
        Self {
            details: fmt::format(args),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.details)
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        &self.details
    }
}

#[derive(Debug)]
pub struct TokenizeError {
    details: String,
}

impl TokenizeError {
    pub fn new(msg: &str) -> Self {
        Self {
            details: msg.to_string(),
        }
    }
}

impl fmt::Display for TokenizeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.details)
    }
}

impl Error for TokenizeError {
    fn cause(&self) -> Option<&dyn Error> {
        None
    }
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
    fn description(&self) -> &str {
        &self.details
    }
}

impl From<ParseFloatError> for TokenizeError {
    fn from(value: ParseFloatError) -> Self {
        Self::new(&value.to_string())
    }
}
