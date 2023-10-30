// CASE Basic for loop
// RETURNS 55

int main() {
    int res = 0;
    for (int i = 0; i <= 5; i = i + 1) {
        res = res + i * i;
    }
    return res;
}

// CASE 45th Fibonacci
// RETURNS 1836311903

int main() {
    int a = 0;
    int b = 1;
    int c;
    for (int i = 0; i <= 44; i = i + 1) {
        c = b + a;
        a = b;
        b = c;
    }
    return c;
}