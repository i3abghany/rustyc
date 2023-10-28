// CASE Basic assignment
// RETURNS 30

int main() {
    int x = 5;
    int y = 6;
    return x * y;
}

// CASE Chain assignment
// RETURNS 25

int main() {
    int x = 0;
    int y = x = 5;
    return x * y;
}

// CASE Global variable assignment
// RETURNS 6

int x = 5;

int main() {
    x = 6;
    return x;
}
