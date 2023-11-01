// CASE Basic function call
// RETURNS 1

int f(int x) {
    return x - 1;
}

int main() {
    return f(2);
}

// CASE Recursive Fibonacci
// RETURNS 3

int fib(int n) {
    if (n <= 1) return n;
    return fib(n-1) + fib(n-2);
}

int main() {
    return fib(4);
}

// CASE Print to stdout
// RETURNS 0
// Outputs Hello

int putchar(int c);

int main() {
    putchar(72);
    putchar(101);
    putchar(108);
    putchar(108);
    putchar(111);
    return 0;
}
