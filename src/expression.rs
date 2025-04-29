#![allow(dead_code)]

use crate::errors::{DivByZeroError, EvalError, ParseError};
use crate::token::Token;
use std::collections::hash_map;
use std::fmt;
use std::ops::Sub;

const PI: f64 = 3.14159265359;
const E: f64 = 2.71828182846;

#[derive(Clone, PartialEq)]
pub enum Expression {
    Number(f64),
    Variable(String),
    Constant(String), // Possible constants can be found in src/token.rs
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Pow(Box<Expression>, Box<Expression>),
    Sin(Box<Expression>),
    Cos(Box<Expression>),
    Tan(Box<Expression>),
    Ln(Box<Expression>),
    Log(f64, Box<Expression>),
}

macro_rules! simplify_one_argument_function {
    ($constructor:expr, $e:expr, $evaluate_div:expr) => {
        Ok($constructor(Box::new($e.simplify($evaluate_div)?)))
    };
}

impl Expression {
    fn gcd(a: &f64, b: &f64) -> f64 {
        let mut r = a.rem_euclid(*b);
        let mut result: f64 = b.clone();

        while r != 0.0 {
            let temp = r;
            r = result.rem_euclid(r);
            result = temp;
        }
        result
    }

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
                    (e, Self::Number(1.0)) => Ok(e.clone()),
                    (Self::Number(1.0), e) => Ok(e.clone()),
                    (Self::Number(x), Self::Number(y)) => Ok(Self::Number(x * y)),
                    _ => Ok(Expression::Mul(Box::new(a), Box::new(b))),
                }
            }
            Self::Div(a, b) => {
                let a = a.simplify(evaluate_div)?;
                let b = b.simplify(evaluate_div)?;

                match (&a, &b) {
                    (e, Self::Number(1.0)) => Ok(e.clone()),
                    (Self::Number(x), Self::Number(y)) => {
                        if evaluate_div {
                            if *y == 0.0 {
                                Err(DivByZeroError::new())
                            } else {
                                Ok(Self::Number(x / y))
                            }
                        } else {
                            let gcd = Self::gcd(x, y);
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
                    (e, Self::Number(1.0)) => Ok(e.clone()),
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
                _ => Ok(Self::Log(base.clone(), Box::new(e.simplify(evaluate_div)?))),
            },

            _ => Ok(self.clone()),
        }
    }

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
            x_known.insert(respects_to.to_string(), x);
            if y < 0.00000001 {
                break;
            }
        }

        Ok(x)
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
            Self::Mul(a, b) => Self::Add(
                Box::new(Self::Mul(Box::new(a.derivative(respects_to)), b.clone())),
                Box::new(Self::Mul(a.clone(), Box::new(b.derivative(respects_to)))),
            ),
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
impl Expression {
    fn precedence(&self) -> u8 {
        match self {
            Expression::Add(_, _) | Expression::Sub(_, _) => 1,
            Expression::Mul(_, _) | Expression::Div(_, _) => 2,
            Expression::Pow(_, _) => 3,
            Expression::Sin(_)
            | Expression::Cos(_)
            | Expression::Tan(_)
            | Expression::Ln(_)
            | Expression::Log(_, _) => 4,
            Expression::Number(_) | Expression::Variable(_) | Expression::Constant(_) => 5,
        }
    }

    fn to_string_prec(&self, parent_prec: u8) -> String {
        let my_prec = self.precedence();
        let s = match self {
            Expression::Number(a) => a.to_string(),
            Expression::Constant(a) => a.clone(),
            Expression::Variable(a) => a.clone(),
            Expression::Add(a, b) => format!(
                "{} + {}",
                a.to_string_prec(my_prec),
                b.to_string_prec(my_prec)
            ),
            Expression::Sub(a, b) => format!(
                "{} - {}",
                a.to_string_prec(my_prec),
                b.to_string_prec(my_prec)
            ),
            Expression::Mul(a, b) => {
                let a_needs_parens = matches!(**a, Expression::Add(_, _) | Expression::Sub(_, _));
                let b_needs_parens = matches!(**b, Expression::Add(_, _) | Expression::Sub(_, _));

                match (&**a, &**b) {
                    // 3x, 2e etc: number or constant directly next to variable/constant
                    (Expression::Number(_), Expression::Variable(_))
                    | (Expression::Number(_), Expression::Constant(_))
                    | (Expression::Constant(_), Expression::Variable(_)) => {
                        format!("{}{}", a.to_string_prec(my_prec), b.to_string_prec(my_prec))
                    }
                    // (x+1)(x-1) and similar
                    _ if a_needs_parens && b_needs_parens => {
                        format!("({})({})", a.to_string_prec(0), b.to_string_prec(0))
                    }
                    _ if a_needs_parens => {
                        format!("({}){}", a.to_string_prec(0), b.to_string_prec(my_prec))
                    }
                    _ if b_needs_parens => {
                        format!("{}({})", a.to_string_prec(my_prec), b.to_string_prec(0))
                    }
                    // fallback: normal multiplication
                    _ => format!(
                        "{} * {}",
                        a.to_string_prec(my_prec),
                        b.to_string_prec(my_prec)
                    ),
                }
            }
            Expression::Div(a, b) => format!(
                "{} / {}",
                a.to_string_prec(my_prec),
                b.to_string_prec(my_prec)
            ),
            Expression::Pow(a, b) => format!(
                "{}^{}",
                a.to_string_prec(my_prec),
                b.to_string_prec(my_prec)
            ),
            Expression::Ln(a) => format!("ln({})", a.to_string_prec(0)),
            Expression::Sin(a) => format!("sin({})", a.to_string_prec(0)),
            Expression::Cos(a) => format!("cos({})", a.to_string_prec(0)),
            Expression::Tan(a) => format!("tan({})", a.to_string_prec(0)),
            Expression::Log(base, arg) => format!("log{}({})", base, arg.to_string_prec(0)),
        };
        if my_prec < parent_prec {
            format!("({})", s)
        } else {
            s
        }
    }
    pub fn to_string(&self) -> String {
        self.to_string_prec(0)
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

fn format_expr_helper(
    f: &mut fmt::Formatter<'_>,
    lhs: &Expression,
    rhs: &Expression,
    indent: &String,
    name: &str,
    is_last: bool,
) -> fmt::Result {
    writeln!(f, "{}", name)?;
    let new_indent = indent.clone() + if is_last { "    " } else { "│   " };
    format_expr_debug(lhs, f, new_indent.clone(), false)?;
    format_expr_debug(rhs, f, new_indent, true)?;
    Ok(())
}
fn format_expr_debug(
    expr: &Expression,
    f: &mut fmt::Formatter<'_>,
    indent: String,
    is_last: bool,
) -> fmt::Result {
    let branch = if is_last { "└── " } else { "├── " };
    write!(f, "{}{}", indent, branch)?;

    match expr {
        Expression::Constant(c) => writeln!(f, "Constant({})", c)?,
        Expression::Number(n) => writeln!(f, "Number({})", n)?,
        Expression::Variable(name) => writeln!(f, "Variable({})", name)?,
        Expression::Add(lhs, rhs) => format_expr_helper(f, &lhs, &rhs, &indent, "Add", is_last)?,
        Expression::Sub(lhs, rhs) => format_expr_helper(f, &lhs, &rhs, &indent, "Sub", is_last)?,
        Expression::Mul(lhs, rhs) => format_expr_helper(f, &lhs, &rhs, &indent, "Mul", is_last)?,
        Expression::Div(lhs, rhs) => format_expr_helper(f, &lhs, &rhs, &indent, "Div", is_last)?,
        Expression::Pow(lhs, rhs) => format_expr_helper(f, &lhs, &rhs, &indent, "Pow", is_last)?,
        Expression::Ln(inside) => {
            writeln!(f, "Ln")?;
            let new_indent = indent.clone() + if is_last { "    " } else { "│   " };
            format_expr_debug(&inside, f, new_indent, true)?;
        }
        Expression::Sin(inside) => {
            writeln!(f, "Sin")?;
            let new_indent = indent.clone() + if is_last { "    " } else { "│   " };
            format_expr_debug(&inside, f, new_indent, true)?;
        }
        Expression::Cos(inside) => {
            writeln!(f, "Cos")?;
            let new_indent = indent.clone() + if is_last { "    " } else { "│   " };
            format_expr_debug(&inside, f, new_indent, true)?;
        }
        Expression::Tan(inside) => {
            writeln!(f, "Tan")?;
            let new_indent = indent.clone() + if is_last { "    " } else { "│   " };
            format_expr_debug(&inside, f, new_indent, true)?;
        }
        Expression::Log(base, inside) => {
            writeln!(f, "Log")?;
            let new_indent = indent.clone() + if is_last { "    " } else { "│   " };
            writeln!(f, "{}Base({})", new_indent, base)?;
            format_expr_debug(&inside, f, new_indent, true)?;
        }
    }
    Ok(())
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_expr_debug(self, f, "".to_string(), true)
    }
}

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::tokenize;

    fn parse_expr(input: &str) -> Expression {
        let tokens = tokenize(input).unwrap();
        let mut parser = Parser::new(tokens);
        parser.parse().unwrap()
    }

    fn parse_expr_with_print(input: &str) -> Expression {
        let tokens = tokenize(input).unwrap();
        println!("{:?}", tokens);
        let mut parser = Parser::new(tokens);
        parser.parse().unwrap()
    }

    #[test]
    fn test_to_string() {
        let expr = parse_expr("3x/(1+1)");
        assert_eq!(expr.to_string(), "3x / (1 + 1)")
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

    #[test]
    fn test_gcd() {
        assert_eq!(Expression::gcd(&6.0, &4.0), 2.0);
    }

    #[test]
    fn test_functions() {
        let sin = parse_expr("sin(x)");
        assert_eq!(
            sin,
            Expression::Sin(Box::new(Expression::Variable("x".to_string())))
        );
        let log = parse_expr_with_print("log10(x)");
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
        assert_eq!(
            sin.eval_symbolic(&hash_map::HashMap::new()).unwrap(),
            Expression::Number(0.0)
        );
    }
}
