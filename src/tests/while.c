// CASE Basic while loop
// RETURNS 5

int main() {
    int x = 0;
    int y = 5;
    while (y > 0) {
        y = y - 1;
        x = x + 1;
    }
    return x;
}

// CASE 45th Fibonacci
// RETURNS 1836311903

int main() {
    int x = 45;
    int a = 0;
    int b = 1;
    int c;
    while (x) {
       c = a + b;
       a = b;
       b = c;
       x = x - 1;
    }
    return c;
}