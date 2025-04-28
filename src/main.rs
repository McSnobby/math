mod errors;
mod expression;
mod token;

use std::env;

fn main() {
    let mut args = env::args().peekable();
    args.next();

    let input = match args.next() {
        Some(input) => input,
        None => panic!("No input provided"),
    };

    let tokens = match token::tokenize(&input) {
        Ok(tok) => tok,
        Err(e) => panic!("{}", e),
    };
    let mut parser = expression::Parser::new(tokens);
    let mut expr = match parser.parse() {
        Ok(expr) => expr,
        Err(e) => panic!("{}", e),
    };

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--derivative" | "-d" => {
                let respects_to = match args.next() {
                    Some(x) => x,
                    None => panic!("No respects to provided"),
                };
                // Unwrap is safe because an error can only occur if we evaluate div
                expr = expr.derivative(&respects_to).simplify(false).unwrap();
                args.next();
            }
            other => {
                panic!("Unknown command {}", other);
            }
        }
    }
    println!("{}", expr);
}
