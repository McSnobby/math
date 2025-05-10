use super::*;
use crate::errors::DivByZeroError;
use crate::util;

macro_rules! simplify_one_argument_function {
    ($constructor:expr, $e:expr, $evaluate_div:expr) => {
        Ok($constructor(Box::new($e.simplify($evaluate_div)?)))
    };
}

impl Expression {
    pub fn simplify(&self, evaluate_div: bool) -> Result<Self, DivByZeroError> {
        match self {
            Self::Add(a, b) => {
                let a = a.simplify(evaluate_div)?;
                let b = b.simplify(evaluate_div)?;

                match (&a, &b) {
                    (Self::Number(0.0), e) | (e, Self::Number(0.0)) => Ok(e.clone()),
                    (Self::Number(x), Self::Number(y)) => Ok(Self::Number(x + y)),
                    _ => Ok(Expression::Add(Box::new(a), Box::new(b))),
                }
            }
            Self::Sub(a, b) => {
                let a = a.simplify(evaluate_div)?;
                let b = b.simplify(evaluate_div)?;
                match (&a, &b) {
                    (Self::Number(x), Self::Number(y)) => Ok(Self::Number(x - y)),
                    _ => Ok(Expression::Add(Box::new(a), Box::new(b))),
                }
            }
            Self::Mul(a, b) => {
                let a = a.simplify(evaluate_div)?;
                let b = b.simplify(evaluate_div)?;

                match (&a, &b) {
                    (_, Self::Number(0.0)) | (Self::Number(0.0), _) => Ok(Self::Number(0.0)),
                    (_, Self::Number(1.0)) => Ok(a),
                    (Self::Number(1.0), _) => Ok(b),
                    (Self::Number(x), Self::Number(y)) => Ok(Self::Number(x * y)),
                    (Self::Number(x), Self::Mul(y, z)) | (Self::Mul(y, z), Self::Number(x)) => {
                        match (&**y, &**z) {
                            (_, Self::Number(c)) => {
                                Ok(Self::Mul(Box::new(Self::Number(*c * x)), y.clone()))
                            }
                            (Self::Number(c), _) => {
                                Ok(Self::Mul(Box::new(Self::Number(*c * x)), z.clone()))
                            }
                            _ => Ok(Self::Mul(Box::new(a), Box::new(b))),
                        }
                    }
                    _ => Ok(Expression::Mul(Box::new(a), Box::new(b))),
                }
            }
            Self::Div(a, b) => {
                let a = a.simplify(evaluate_div)?;
                let b = b.simplify(evaluate_div)?;

                match (&a, &b) {
                    (_, Self::Number(1.0)) => Ok(a),
                    (Self::Number(x), Self::Number(y)) => {
                        if evaluate_div {
                            if *y == 0.0 {
                                Err(DivByZeroError::new())
                            } else {
                                Ok(Self::Number(x / y))
                            }
                        } else {
                            let gcd = util::gcd(x, y);
                            Ok(Self::Div(
                                Box::new(Self::Number(x / gcd)),
                                Box::new(Self::Number(y / gcd)),
                            ))
                        }
                    }
                    _ => Ok(Self::Div(Box::new(a), Box::new(b))),
                }
            }
            Self::Pow(a, b) => {
                let a = a.simplify(evaluate_div)?;
                let b = b.simplify(evaluate_div)?;

                match (&a, &b) {
                    (_, Self::Number(0.0)) => Ok(Self::Number(1.0)),
                    (_, Self::Number(1.0)) => Ok(a),
                    _ => Ok(Expression::Pow(Box::new(a), Box::new(b))),
                }
            }
            Self::Ln(e) => {
                if **e == Self::Constant("e".to_string()) {
                    Ok(Self::Number(1.0))
                } else {
                    Ok(Self::Ln(Box::new(e.simplify(evaluate_div)?)))
                }
            }
            Self::Sin(e) => simplify_one_argument_function!(Self::Sin, e, evaluate_div),
            Self::Cos(e) => simplify_one_argument_function!(Self::Cos, e, evaluate_div),
            Self::Tan(e) => simplify_one_argument_function!(Self::Tan, e, evaluate_div),
            Self::Log(base, e) => match (base, &**e) {
                (x, Self::Number(y)) => {
                    if x == y {
                        Ok(Self::Number(1.0))
                    } else {
                        Ok(self.clone())
                    }
                }
                _ => Ok(Self::Log(*base, Box::new(e.simplify(evaluate_div)?))),
            },

            _ => Ok(self.clone()),
        }
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
    fn test_simplify() {
        let expr1 = parse_expr("1 + 2(3*8)");
        assert_eq!(expr1.simplify(true).unwrap(), Expression::Number(49.0));

        let expr2 = parse_expr("x^(2+1)");
        let expr3 = parse_expr("x^3");
        assert_eq!(expr2.simplify(true).unwrap(), expr3);

        let div = parse_expr("6/4");
        let simp_div = parse_expr("3/2");
        assert_eq!(div.simplify(false).unwrap(), simp_div);
    }
}
