// CASE Binary operator +
// RETURNS 3

int main() {
    return 1 + 2;
}

// CASE Binary operator -
// RETURNS -1

int main() {
    return 1 - 2;
}

// CASE Binary operator *
// RETURNS 10

int main() {
    return 5 * 2;
}

// CASE Binary operator *
// RETURNS 4

int main() {
    return 9 / 2;
}

// CASE Binary arithmetic operators combined
// RETURNS 9697

int main() {
    int x = 312;
    int y = 99;
    int z;
    z = 2 * x / 3 + y * y;
    return z - x;
}

// CASE Binary operator ||
// RETURNS 1

int main() {
    int false = 0; int true = 123; int y = true || false; return y;
}

// CASE Binary operator &&
// RETURNS 0

int main() {
    int false = 0; int true = 123; int y = true && false; return y;
}

// CASE Binary operator ^
// RETURNS 123

int main() {
    int false = 0; int true = 123; return true ^ false;
}

// CASE Unary operator ! 1
// RETURNS 1

int main() {
    int x = !0;
    return x;
}

// CASE Unary operator ! 2
// RETURNS 0

int main() {
    int x = !1;
    return x;
}

// CASE Unary +-
// RETURNS 144

int main() {
    int x = -----++++----+------12; return x * x;
}

// CASE Binary, unary and parenthesized expressions
// RETURNS -1

int main() {
    int x = 3;
    int y = 4;
    int z = 5;
    return (-x + y) * (z - x) / (-y + z) - x;
}
