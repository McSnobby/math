mod errors;
mod expression;
mod parse;
mod token;
mod util;

use std::{env, process::exit};

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
    let mut expr = match parse::parse_tokens(tokens) {
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
            }
            "--root" | "-R" => {
                let respects_to = match args.next() {
                    Some(x) => x,
                    None => panic!("Must provide respects to"),
                };
                match expr.find_root(&respects_to, 1.0) {
                    Ok(r) => println!("{} = {}", respects_to, r),
                    Err(_) => println!("{} = undefined", respects_to),
                }
                exit(0);
            }
            "--simplify" | "-sp" => {
                let evaluate_div = match args.next() {
                    Some(s) => {
                        if s == "true" {
                            true
                        } else if s == "false" {
                            false
                        } else {
                            panic!("Invalid parameter, valid are true/false")
                        }
                    }
                    None => panic!(
                        "Must provide provide if it should evaluate div\n Valid parameters are: true/false "
                    ),
                };
                match expr.simplify(evaluate_div) {
                    Ok(r) => expr = r,
                    Err(_) => panic!("Cannot divide by zero"),
                }
            }

            "--solve" | "-S" => {
                let rhs = match args.next() {
                    Some(x) => x,
                    None => panic!("Must provide right hand side"),
                };
                let tokens = match token::tokenize(&rhs) {
                    Ok(tok) => tok,
                    Err(e) => panic!("{}", e),
                };
                let expr2 = match parse::parse_tokens(tokens) {
                    Ok(ex) => ex,
                    Err(e) => panic!("{}", e),
                };
                let respects_to = match args.next() {
                    Some(x) => x,
                    None => panic!("Mst provide respects to"),
                };
                match expr.solve(&expr2, &respects_to, 1.0) {
                    Ok(r) => println!("{} = {}", respects_to, r),
                    Err(_) => println!("{} = undefined", respects_to),
                }
                exit(0);
            }
            other => {
                panic!("Unknown command {}", other);
            }
        }
    }
    println!("{}", expr);
}
