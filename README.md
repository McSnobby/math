# math
A free matematica alternative. (Better name comming soon...)

## Currently in development
This code is only left here as proof of concept.

Terribly optimised with excessive cloning.

## Usage
Clone it and run\n
`cargo run <expression> <arguments>`

### Expressions

#### Operands
+, -, *, /, ^
#### Constants
- pi
- e
#### Functions
- Sin
- Cos
- Tan
- Ln
- Log(base), Example `Log10(x)`
- sqrt, Alias for ^(1/2)
#### Variables
All other a-z and A-Z characters will be treated as unknowns

### Flags
- --derive / -d <Respects to>
  - Example: `cargo run x^3 -d x`, Output: `3 * x^2`
- --root / -R <Respects to>
  - Example: `cargo run x^3 -R x`, Output: `x = 0`
- --solve / -S <Right hand side> <Respects to>
  - Example: `cargo run 2x -S 4 x`, Output: `x = 2`

You can chain the flags to do multiple operations on the expression.
## Features

- Symbolic derivatives
- Simplifying expressions (For expressions with only numbers, this also means calculating it) (No flag for this yet)
- Solving
- Evaluating

