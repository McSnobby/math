#![allow(dead_code)]

mod algebra;
mod eval;
mod simplify;

use std::fmt;

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse_tokens;
    use crate::token::tokenize;

    fn parse_expr(input: &str) -> Expression {
        let tokens = tokenize(input).unwrap();
        parse_tokens(tokens).unwrap()
    }

    #[test]
    fn test_to_string() {
        let expr = parse_expr("3x/(1+1)");
        assert_eq!(expr.to_string(), "3x / (1 + 1)")
    }
}
