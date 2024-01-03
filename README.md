# IgnisScript

This is a simple programming language with some inspiration in Rust and C.
This compiles first to X86_64 assembly and then to an executable.

**This will only run on linux with NASM installed**

## Features

- [x] - Types: INT, CHAR, BOOL;

- [x] - Arithmetic operations: +, -, *, /;

- [x] - Boolean operations: &&, ||, !;

- [x] - Comparators: ==, !=, >, <, >=, <=;

- [x] - Unitary operations: -,!;

- [x] - Scopes: define by {...}

- [x] - Variables with strong type checking;

- [x] - While Loops;

- [x] - IF, ELSE IF, ELSE;


## Example

Here is an simple example of a program in IgnisScript:

```
let fib : int = 0;
let a : int = 0;
let b : int = 1;
let n : int = 10;

while (n > 0) {
    println(a);
    let next : int = a + b;
    a = b;
    b = next;
    n = n - 1;
}
```

## Compiling

Simply run ```cargo build --release``` to compile the project.

## Running

Simply run ```cargo run --release``` to compile and run the project.

## License

This project is under the GNU General Public License v3.0 license(GPLv3).

Check the license in the [LICENSE](LICENSE) file.

