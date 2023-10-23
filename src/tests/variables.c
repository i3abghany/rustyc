// CASE Return Local
// RETURNS 1

int main() {
    int x = 1;
    return x;
}

// CASE Return Global
// RETURNS 2

int d = 2;
int main() {
    return d;
}

// CASE Multiple Global Declarations
// RETURNS 3

int d;
int d;
int d = 3;

int main() {
    return d;
}

// CASE Global and Local Declarations & Definitions
// RETURNS 4

int d = 3;

int main() {
    int d = 4;
    return d;
}
