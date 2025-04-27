mod errors;
mod expression;
mod token;

fn main() {
    let input = "3x + 2(x + 1)";
    let tokens = token::tokenize(input).unwrap();
    let mut parser = expression::Parser::new(tokens);
    let expr = parser.parse().unwrap();

    println!("{}", expr.derivative("x").simplify(false).unwrap());
}
