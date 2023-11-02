# RustyC

RustyC is a compiler written in Rust that supports a subset of the 
C programming language. It is capable of producing x86_64 native
assembly and LLVM IR.

## Example Usage

An example of what RustyC can compile is shown below.

```c
// fib.c

int fib_recursive(int n) {
    if (n <= 1) return n;
    return fib_recursive(n - 1) + fib_recursive(n - 2);
}

int fib_iterative(int n) {
    int a = 0;
    int b = 1;
    for (int i = 0; i < n; i = i + 1) {
        int c = a;
        a = b;
        b = c + b;
    }
    return a;
}

int main() {
    return !(fib_recursive(10) == fib_iterative(10));
}
```

```bash

$ cargo b --release
$ ./target/release/rustyc --help

Usage: rustyc.exe [OPTIONS] [INPUT_SOURCE_FILES]...

Arguments:
  [INPUT_SOURCE_FILES]...

Options:
  -l, --emit-llvm
  -c, --emit-object
  -s, --emit-asm
  -o, --exe-filename <EXE_FILENAME>
  -h, --help

$ ./target/release/rustyc fib.c -o fib
$ ./fib; echo $?
0
```

## Pre-requisites

- Rust 1.70.0 or later
- LLVM 15.0.x or later
