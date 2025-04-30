use crate::errors::ParseError;
use crate::expression::Expression;
use crate::token::Token;
use std::fmt;

type ParseResult = Result<Expression, ParseError>;

// Wrapper for the parser to remove the unintuative mut requirement
pub fn parse_tokens(tokens: Vec<Token>) -> ParseResult {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

// to be used in parsing functions
// for code simplicity
macro_rules! one_argument_function {
    ($self:expr, $constructor:expr) => {
        if $self.advance() == Some(Token::LParen) {
            Ok($constructor(Box::new($self.parse_paren()?)))
        } else {
            Err(ParseError::new("must close functions"))
        }
    };
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<Token> {
        if self.pos >= self.tokens.len() {
            None
        } else {
            let tok = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(tok)
        }
    }

    pub fn parse(&mut self) -> ParseResult {
        let expr = self.parse_add_sub();
        if self.peek().is_some() {
            panic!("Unexpected token: {:?}", self.peek());
        }

        expr
    }

    fn parse_add_sub(&mut self) -> ParseResult {
        let mut node = self.parse_mul_and_div()?;

        while let Some(tok) = self.peek() {
            match tok {
                Token::Plus => {
                    self.advance();
                    node = Expression::Add(Box::new(node), Box::new(self.parse_mul_and_div()?));
                }
                Token::Minus => {
                    self.advance();
                    node = Expression::Sub(Box::new(node), Box::new(self.parse_mul_and_div()?));
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_mul_and_div(&mut self) -> ParseResult {
        let mut node = self.parse_pow()?;

        while let Some(tok) = self.peek() {
            match tok {
                Token::Star => {
                    self.advance();
                    node = Expression::Mul(Box::new(node), Box::new(self.parse_pow()?));
                }
                Token::Variable(_) | Token::Number(_) | Token::LParen => {
                    // implicit multiplication
                    node = Expression::Mul(Box::new(node), Box::new(self.parse_pow()?));
                }
                Token::Slash => {
                    self.advance();
                    node = Expression::Div(Box::new(node), Box::new(self.parse_pow()?))
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn parse_pow(&mut self) -> ParseResult {
        let base = self.parse_atom()?;

        if let Some(Token::Caret) = self.peek() {
            self.advance();
            let exponent = self.parse_pow()?; // right-associative
            Ok(Expression::Pow(Box::new(base), Box::new(exponent)))
        } else {
            Ok(base)
        }
    }

    fn parse_atom(&mut self) -> ParseResult {
        match self.advance() {
            Some(Token::Number(n)) => Ok(Expression::Number(n)),
            Some(Token::Constant(c)) => Ok(Expression::Constant(c)),
            Some(Token::Variable(name)) => Ok(Expression::Variable(name)),
            Some(Token::LParen) => self.parse_paren(),
            Some(Token::Minus) => Ok(Expression::Mul(
                Box::new(Expression::Number(-1.0)),
                Box::new(self.parse_atom()?),
            )),
            Some(Token::Function(variant)) => self.parse_function(&variant),
            other => Err(ParseError::new(&fmt::format(format_args!(
                "Unexpected token: {:?}",
                other
            )))),
        }
    }

    fn parse_paren(&mut self) -> ParseResult {
        let expr = self.parse_add_sub()?;
        match self.advance() {
            Some(Token::RParen) => Ok(expr),
            _ => Err(ParseError::new("Expected ')'")),
        }
    }

    fn parse_function(&mut self, variant: &str) -> ParseResult {
        match variant {
            "sin" => one_argument_function!(self, Expression::Sin),
            "cos" => one_argument_function!(self, Expression::Cos),
            "tan" => one_argument_function!(self, Expression::Tan),
            "ln" => one_argument_function!(self, Expression::Ln),
            "log" => {
                if let Some(Token::Number(x)) = self.advance() {
                    if x.is_sign_positive() {
                        if self.advance() == Some(Token::LParen) {
                            Ok(Expression::Log(x, Box::new(self.parse_paren()?)))
                        } else {
                            Err(ParseError::new("must close functions"))
                        }
                    } else {
                        Err(ParseError::new("invalid base"))
                    }
                } else {
                    Err(ParseError::new("need to have a base"))
                }
            }
            "sqrt" => {
                if self.advance() == Some(Token::LParen) {
                    Ok(Expression::Pow(
                        Box::new(self.parse_paren()?),
                        Box::new(Expression::Number(0.5)),
                    ))
                } else {
                    Err(ParseError::new("must close functions"))
                }
            }

            other => Err(ParseError::with_format(format_args!(
                "unknown function: {}",
                other
            ))),
        }
    }
}

mod tests {
    use super::*;
    use crate::token::tokenize;

    fn parse_expr(input: &str) -> Expression {
        let tokens = tokenize(input).unwrap();
        parse_tokens(tokens).unwrap()
    }

    #[test]
    fn test_simple_addition() {
        let expr = parse_expr("3 + 2");
        assert!(matches!(expr, Expression::Add(_, _)));
    }

    #[test]
    fn test_variable_multiplication() {
        let expr = parse_expr("3x");
        match expr {
            Expression::Mul(a, b) => {
                assert_eq!(*a, Expression::Number(3.0));
                assert_eq!(*b, Expression::Variable("x".to_string()));
            }
            _ => panic!("Expected Mul"),
        }
    }

    #[test]
    fn test_parenthesized_expression() {
        let expr = parse_expr("(3 + 2)");
        match expr {
            Expression::Add(a, b) => {
                assert_eq!(*a, Expression::Number(3.0));
                assert_eq!(*b, Expression::Number(2.0));
            }
            _ => panic!("Expected Add inside parentheses"),
        }
    }

    #[test]
    fn test_nested_expression() {
        let expr = parse_expr("3x + 2 + (3x * -1)");
        match expr {
            Expression::Add(_, right) => match *right {
                Expression::Mul(_, _) => {} // inner (3x * -1)
                _ => panic!("Expected Mul at the end"),
            },
            _ => panic!("Expected top-level Add"),
        }
    }

    #[test]
    fn test_negative_number() {
        let expr = parse_expr("-3");
        println!("{:?}", expr);
        assert_eq!(expr, Expression::Number(-3.0));
    }

    #[test]
    fn test_implicit_mul_with_parens() {
        let expr = parse_expr("2(3 + 4)");
        match expr {
            Expression::Mul(a, b) => {
                assert_eq!(*a, Expression::Number(2.0));
                match &*b {
                    Expression::Add(_, _) => {}
                    _ => panic!("Expected Add inside parens"),
                }
            }
            _ => panic!("Expected Mul"),
        }
    }

    #[test]
    fn test_div() {
        let expr = parse_expr("1/2");
        assert_eq!(
            expr,
            Expression::Div(
                Box::new(Expression::Number(1.0)),
                Box::new(Expression::Number(2.0))
            )
        );
    }

    #[test]
    fn test_multiple_variables() {
        let expr = parse_expr("xy");
        match expr {
            Expression::Mul(a, b) => {
                assert_eq!(*a, Expression::Variable("x".to_string()));
                assert_eq!(*b, Expression::Variable("y".to_string()));
            }
            Expression::Variable(var) => panic!("got single variable: {}", var),
            other => panic!("got: {:?}", other),
        }
    }

    #[test]
    fn test_functions() {
        let sin = parse_expr("sin(x)");
        assert_eq!(
            sin,
            Expression::Sin(Box::new(Expression::Variable("x".to_string())))
        );
        let log = parse_expr("log10(x)");
        assert_eq!(
            log,
            Expression::Log(10.0, Box::new(Expression::Variable("x".to_string())))
        );
        let sqrt = parse_expr("sqrt(x)");
        assert_eq!(
            sqrt,
            Expression::Pow(
                Box::new(Expression::Variable("x".to_string())),
                Box::new(Expression::Number(0.5))
            )
        );
    }
}
