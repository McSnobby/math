use crate::errors::TokenizeError;
use std::fmt;
const KNOWN_CONSTANTS: &[&str] = &["pi", "e"];
const KNOWN_FUNCTIONS: &[&str] = &["sin", "cos", "tan", "ln", "log", "sqrt"];

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Number(f64),
    Variable(String),
    Constant(String),
    Function(String),
    Plus,
    Minus,
    Slash,
    Star,
    Caret,
    LParen,
    RParen,
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' => {
                chars.next();
            }
            '+' => {
                tokens.push(Token::Plus);
                chars.next();
            }
            '/' => {
                tokens.push(Token::Slash);
                chars.next();
            }
            '-' => {
                chars.next();

                // Lookahead for negative number
                if let Some(&next_ch) = chars.peek() {
                    if next_ch.is_ascii_digit() {
                        let mut num = String::from("-");
                        while let Some(&c) = chars.peek() {
                            if c.is_ascii_digit() || c == '.' {
                                num.push(c);
                                chars.next();
                            } else {
                                break;
                            }
                        }
                        tokens.push(Token::Number(num.parse().unwrap()));
                    } else {
                        tokens.push(Token::Minus);
                    }
                } else {
                    tokens.push(Token::Minus);
                }
            }
            '*' => {
                tokens.push(Token::Star);
                chars.next();
            }
            '^' => {
                tokens.push(Token::Caret);
                chars.next();
            }
            '(' => {
                tokens.push(Token::LParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::RParen);
                chars.next();
            }
            '0'..='9' | '.' => {
                let mut num = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() || c == '.' {
                        num.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Number(num.parse()?))
            }
            'π' => {
                tokens.push(Token::Constant("pi".to_string()));
                chars.next();
            }
            'a'..='z' | 'A'..='Z' => {
                let mut ident = String::new();

                while let Some(&d) = chars.peek() {
                    if d.is_alphabetic() {
                        ident.push(d);
                        chars.next();
                    } else {
                        break;
                    }
                }

                if KNOWN_CONSTANTS.contains(&ident.as_str()) {
                    tokens.push(Token::Constant(ident));
                } else if KNOWN_FUNCTIONS.contains(&ident.as_str()) || chars.peek() == Some(&'(') {
                    tokens.push(Token::Function(ident));
                } else {
                    for c in ident.chars() {
                        tokens.push(Token::Variable(c.to_string()));
                    }
                }
            }
            _ => {
                return Err(TokenizeError::new(&fmt::format(format_args!(
                    "Unrecongized charachter: {}",
                    ch
                ))));
            }
        }
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function() {
        assert_eq!(
            tokenize("sin(x)").unwrap(),
            vec![
                Token::Function("sin".to_string()),
                Token::LParen,
                Token::Variable("x".to_string()),
                Token::RParen
            ]
        );
    }
}
