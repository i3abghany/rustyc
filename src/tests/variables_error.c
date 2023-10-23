// CASE Multiple Global Definitions
// RETURNS 2

int d = 1;
int d = 2;

int main() {
    return d;
}

// CASE Multiple Local Definitions
// RETURNS 2

int main() {
    int d = 1;
    int d = 2;
    return d;
}

// CASE Multiple Local Declarations
// RETURNS 2

int main() {
    int d;
    int d;
    return d;
}

// CASE Local Declaration and Definition
// RETURNS 2

int main() {
    int d;
    int d = 2;
    return d;
}
