use super::*;

impl Expression {
    pub fn derivative(&self, respects_to: &str) -> Self {
        match self {
            Self::Variable(x) => {
                if x == respects_to {
                    Self::Number(1.0)
                } else {
                    Self::Number(0.0)
                }
            }
            Self::Constant(_) => Self::Number(0.0),
            Self::Number(_) => Self::Number(0.0),
            Self::Add(a, b) => Self::Add(
                Box::new(a.derivative(respects_to)),
                Box::new(b.derivative(respects_to)),
            ),
            Self::Sub(a, b) => Self::Sub(
                Box::new(a.derivative(respects_to)),
                Box::new(b.derivative(respects_to)),
            ),
            Self::Mul(a, b) => match (&**a, &**b) {
                (Self::Number(_), Self::Number(_)) => Self::Number(0.0),
                (x, Self::Number(y)) | (Self::Number(y), x) => Self::Mul(
                    Box::new(Self::Number(*y)),
                    Box::new(x.derivative(respects_to)),
                ),
                _ => Self::Add(
                    Box::new(Self::Mul(Box::new(a.derivative(respects_to)), b.clone())),
                    Box::new(Self::Mul(a.clone(), Box::new(b.derivative(respects_to)))),
                ),
            },
            Self::Div(a, b) => match (&**a, &**b) {
                (Self::Number(_), Self::Number(_)) => self.clone(),
                _ => {
                    let numerator_term1 =
                        Box::new(Self::Mul(Box::new(a.derivative(respects_to)), b.clone()));
                    let numerator_term2 =
                        Box::new(Self::Mul(a.clone(), Box::new(b.derivative(respects_to))));
                    let denominator = Box::new(Self::Pow(b.clone(), Box::new(Self::Number(2.0))));

                    Self::Div(
                        Box::new(Self::Sub(numerator_term1, numerator_term2)),
                        denominator,
                    )
                }
            },
            Self::Pow(a, b) => match (&**a, &**b) {
                (_, Self::Number(x)) => Self::Mul(
                    Box::new(Self::Mul(b.clone(), Box::new(a.derivative(respects_to)))),
                    Box::new(Self::Pow(a.clone(), Box::new(Self::Number(x - 1.0)))),
                ),
                (x, y) => Self::Mul(
                    Box::new(self.clone()),
                    Box::new(
                        Self::Mul(Box::new(y.clone()), Box::new(Self::Ln(Box::new(x.clone()))))
                            .derivative(respects_to),
                    ),
                ),
            },
            Self::Ln(x) => Self::Div(Box::new(x.derivative(respects_to)), x.clone()),
            Self::Log(x, a) => Self::Div(
                Box::new(a.derivative(respects_to)),
                Box::new(Self::Mul(
                    Box::new(Self::Ln(Box::new(Self::Number(*x)))),
                    a.clone(),
                )),
            ),
            Self::Sin(x) => Self::Mul(
                Box::new(x.derivative(respects_to)),
                Box::new(Self::Cos(x.clone())),
            ),
            Self::Cos(x) => Self::Mul(
                Box::new(x.derivative(respects_to)),
                Box::new(Self::Mul(
                    Box::new(Self::Number(-1.0)),
                    Box::new(Self::Sin(x.clone())),
                )),
            ),
            Self::Tan(x) => Self::Div(
                Box::new(x.derivative(respects_to)),
                Box::new(Self::Pow(
                    Box::new(Self::Cos(x.clone())),
                    Box::new(Self::Number(2.0)),
                )),
            ),
        }
    }

    fn primitive_function(&self, respects_to: &str) -> Self {
        match self {
            Self::Number(_) | Self::Constant(_) => Self::Mul(
                Box::new(self.clone()),
                Box::new(Self::Variable(respects_to.to_string())),
            ),
            Self::Variable(x) => {
                if x == respects_to {
                    Self::Div(
                        Box::new(Self::Pow(
                            Box::new(self.clone()),
                            Box::new(Self::Number(2.0)),
                        )),
                        Box::new(Self::Number(2.0)),
                    )
                } else {
                    Self::Mul(
                        Box::new(self.clone()),
                        Box::new(Self::Variable(respects_to.to_string())),
                    )
                }
            }
            Self::Add(a, b) => Self::Add(
                Box::new(a.primitive_function(respects_to)),
                Box::new(b.primitive_function(respects_to)),
            ),
            Self::Sub(a, b) => Self::Sub(
                Box::new(a.primitive_function(respects_to)),
                Box::new(b.primitive_function(respects_to)),
            ),
            _ => todo!(),
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
    fn test_derivate() {
        let expr = parse_expr("(x+1)^2");
        let d = expr.derivative("x").simplify(false).unwrap();

        assert_eq!(d, parse_expr("2(x+1)"));

        let expr1 = parse_expr("log10(sin(x+1))");
        let d1 = expr1.derivative("x").simplify(false).unwrap();
        let test = parse_expr("cos(x+1)/(ln(10)*sin(x+1))");
        println!("{:?}", test);
        assert_eq!(d1, test);
    }
}
