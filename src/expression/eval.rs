use super::*;
use crate::errors::{DivByZeroError, EvalError};
use crate::util;
use std::collections::hash_map;
use std::ops::Sub;

impl Expression {
    fn eval_symbolic(
        &self,
        known: &hash_map::HashMap<String, f64>,
    ) -> Result<Self, DivByZeroError> {
        match self {
            Self::Variable(x) => match known.get(x) {
                Some(v) => Ok(Self::Number(*v)),
                None => Ok(self.clone()),
            },
            Self::Constant(a) => match a.as_str() {
                "pi" => Ok(Self::Number(PI)),
                "e" => Ok(Self::Number(E)),
                other => panic!("Unknown constant {}", other),
            },
            Self::Number(_) => Ok(self.clone()),
            Self::Add(a, b) => {
                let a = a.eval_symbolic(&known)?;
                let b = b.eval_symbolic(&known)?;
                match (&a, &b) {
                    (Self::Number(x), Self::Number(y)) => Ok(Self::Number(x + y)),
                    _ => Ok(Self::Add(Box::new(a), Box::new(b))),
                }
            }
            Self::Sub(a, b) => {
                let a = a.eval_symbolic(&known)?;
                let b = b.eval_symbolic(&known)?;
                match (&a, &b) {
                    (Self::Number(x), Self::Number(y)) => Ok(Self::Number(x - y)),
                    _ => Ok(Self::Sub(Box::new(a), Box::new(b))),
                }
            }
            Self::Mul(a, b) => {
                let a = a.eval_symbolic(&known)?;
                let b = b.eval_symbolic(&known)?;
                match (&a, &b) {
                    (Self::Number(x), Self::Number(y)) => Ok(Self::Number(x * y)),
                    _ => Ok(Self::Mul(Box::new(a), Box::new(b))),
                }
            }
            Self::Div(a, b) => {
                let a = a.eval_symbolic(&known)?;
                let b = b.eval_symbolic(&known)?;
                match (&a, &b) {
                    (Self::Number(x), Self::Number(y)) => {
                        if *y == 0.0 {
                            Err(DivByZeroError::new())
                        } else {
                            Ok(Self::Number(x / y))
                        }
                    }
                    _ => Ok(Self::Add(Box::new(a), Box::new(b))),
                }
            }
            Self::Pow(a, b) => {
                let a = a.eval_symbolic(&known)?;
                let b = b.eval_symbolic(&known)?;

                match (&a, &b) {
                    (Self::Number(x), Self::Number(y)) => Ok(Self::Number(x.powf(*y))),
                    (_, Self::Number(0.0)) => Ok(Self::Number(1.0)),
                    (_, Self::Number(1.0)) => Ok(a),
                    _ => Ok(Self::Pow(Box::new(a), Box::new(b))),
                }
            }
            Self::Sin(a) => {
                let a = a.eval_symbolic(&known)?;
                match a {
                    Self::Number(x) => Ok(Self::Number(x.sin())),
                    _ => Ok(Self::Sin(Box::new(a))),
                }
            }
            Self::Cos(a) => {
                let a = a.eval_symbolic(&known)?;
                match a {
                    Self::Number(x) => Ok(Self::Number(x.cos())),
                    _ => Ok(Self::Cos(Box::new(a))),
                }
            }
            Self::Tan(a) => {
                let a = a.eval_symbolic(&known)?;
                match a {
                    Self::Number(x) => Ok(Self::Number(x.tan())),
                    _ => Ok(Self::Tan(Box::new(a))),
                }
            }
            Self::Ln(a) => {
                let a = a.eval_symbolic(&known)?;
                match a {
                    Self::Number(x) => Ok(Self::Number(x.ln())),
                    _ => Ok(Self::Ln(Box::new(a))),
                }
            }
            Self::Log(a, b) => {
                let b = b.eval_symbolic(&known)?;
                match b {
                    Self::Number(x) => Ok(Self::Number(x.log(*a))),
                    _ => Ok(Self::Log(*a, Box::new(b))),
                }
            }
        }
    }

    pub fn eval_numeric(&self, known: &hash_map::HashMap<String, f64>) -> Result<f64, EvalError> {
        match self.eval_symbolic(&known)? {
            Self::Number(x) => Ok(x),
            _ => Err(EvalError::new("Could find value")),
        }
    }

    pub fn find_root(&self, respects_to: &str, guess: f64) -> Result<f64, EvalError> {
        // Newton-Raphson method
        let derivative = self.derivative(respects_to);
        let mut x: f64 = guess;
        let mut y: f64;
        let mut x_known = hash_map::HashMap::new();
        x_known.insert(respects_to.to_string(), x);

        loop {
            y = self.eval_numeric(&x_known)?;
            x = x - y / derivative.eval_numeric(&x_known)?;

            if -1e-31 < y && y < 1e-31 {
                break;
            }

            x_known.insert(respects_to.to_string(), x);
        }

        x_known.clear();

        Ok(util::round_dp(x, 10))
    }

    // Wrapper for find_root
    pub fn solve(&self, rhs: &Self, respects_to: &str, guess: f64) -> Result<f64, EvalError> {
        (self.clone() - rhs.clone()).find_root(respects_to, guess)
    }
}

impl Sub for Expression {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Expression::Sub(Box::new(self), Box::new(rhs))
    }
}

mod tests {
    use super::*;
    use crate::parse::parse_tokens;
    use crate::token::tokenize;

    fn parse_expr(input: &str) -> Expression {
        let tokens = tokenize(input).unwrap();
        parse_tokens(tokens).unwrap()
    }

    #[test]
    fn test_eval() {
        let expr = parse_expr("(x + 2)^2");
        let mut known = hash_map::HashMap::new();
        known.insert("x".to_string(), 3.0);
        assert_eq!(
            expr.eval_symbolic(&known).unwrap(),
            Expression::Number(25.0)
        );

        let sin = parse_expr("sin(pi)");
        let result = sin.eval_numeric(&hash_map::HashMap::new()).unwrap();
        assert!(-0.0000001 <= result && result < 0.000000001);
    }

    #[test]
    fn test_solve() {
        let expr = parse_expr("x+2");
        assert_eq!(expr.find_root("x", 1.0).unwrap(), -2.0);

        let expr2 = parse_expr("x^2 - sin(x)");
        assert_eq!(expr2.find_root("x", -1.0).unwrap(), 0.0);

        // Test negative guess
        let expr3 = parse_expr("x^3");
        assert_eq!(expr3.find_root("x", -1.0).unwrap(), 0.0);
    }
}
