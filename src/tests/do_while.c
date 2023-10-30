// CASE Basic do while loop
// RETURNS 5

int main() {
    int x = 0;
    int y = 5;
    do {
        y = y - 1;
        x = x + 1;
    } while (y > 0);
    return x;
}

// CASE 45th Fibonacci
// RETURNS 1836311903

int main() {
    int x = 45;
    int a = 0;
    int b = 1;
    int c;
    do {
       c = a + b;
       a = b;
       b = c;
       x = x - 1;
    } while (x);
    return c;
}