# Computor V2

Computor V2 is an exploration in building interpreters with focus on implementing functional programming features.

## Running the project
Computor V2 is a `cabal`-backed project.
Running it is as simple as running `cabal run`.
The method was tested with `cabal 3.2.0.0`.

## Features

### Arithmetic Operations

Computor V2 supports the following arithmetic operations, listed in descending order of precedence:
- `-` - negation, prefix operator
- `%` - modulo
- `^` - exponentiation
- `/` - division
- `**` - matrix multiplication
- `*` - multiplication
- `+` - addition
- `-` - subtraction

All operators except negation are infix and left-associative.

### Supported Types

- Integers (`0`, `11`, `123`, `-256`)
- Rationals (`1.1`, `0.2`, `-0.771`, `0.0`)
- Complex Numbers (`2.0 * i`, `3 * i`, `1 + 4 * i`)
- Matrices (`[[1, 2];[3, 4]]`, `[[1]]`, `[]`)
- Functions (`() -> 0`, `(a, b) -> a + b`, `(fn, val) -> fn(val)`)

### Variables

Variables are alphanumeric identifiers that can hold value of any type:
```
$> num = 3
3
$> float = 2.4
2.4
$> num + float
5.4
$> square = (n) -> n ^ 2
(n) -> (n ^ 2)
$> square(num * 3)
81
```

__Important__: `x` is a reserved keyword that is used for an unspoken feature of this project..

### Function definition

Functions can be defined in 2 different ways:
```
$> add = (a, b) -> a + b
```
and
```
$> add(a, b) = a + b
```

Both are equivalent as the latter option is the syntactic sugar for the former.

### Curried functions

Functions in Computor V2 are curried by default.
If the function of `N` arguments is applied to `M` arguments (where `M < N`), it returns a function of `N - M` arguments that remembered previously applied `M` arguments.
Let's leave this explanation...

The feature is best explained with an example:
```
$> add = (a, b) -> a + b
(a, b) -> (a + b)
$> add2 = add(2)
(a: 2, b) -> (a + b)
$> add2(3)
5
```

Here, `add2` is an _unsaturated_ version of `add` with a single argument applied.
It means that whenever we will apply `add2` to an argument `z` (to _saturate_ the function), it will execute the body of `add` with `a` substituted with the previously applied `2` and `b` substituted with the value of `z`.

### Anonymous functions

In Computor V2, functions can be defined and used in-place without the need for a name.
```
$> ((a, b) -> a + b)(2, 3)
5
```
Anonymous functions look a little bit more useful together with the next feature.

### Higher-order functions

Computor V2 supports higher-order functions, meaning that functions can be used as function arguments and return values.

Function as an argument:
```
$> apply = (func, value) -> func(value)
(func, value) -> func(value)
$> apply((a) -> a ^ 2, 3)
9
```

Function as both an argument and a return value:
```
$> apply(add, 3)
(a: 3, b) -> (a + b)
```

### Builtins

Builtins are means of executing special commands in the Computor V2 shell.
All builtins start with `@` and can be listed using the `@help` builtin.

Supported builtins:
- `@help` - print help information.
- `@dump` - dump the list of defined variables.
- `@exit` - exit the program.

## Easter eggs

As the legend goes, Computor V2 supports Church booleans and can solve quadratic equations...
