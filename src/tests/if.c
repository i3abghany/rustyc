// CASE Basic if 1
// RETURNS 5

int main() {
    if (1) { return 5; }
    else {return 6; }
    return 0;
}

// CASE Basic if 2
// RETURNS 5

int main() {
    if (1) { return 5; }
    return 6;
}

// CASE if, else if
// RETURNS 5

int main() {
    int x = 5;
    if (x < 3)
        return 6;
    else if (x >= 5)
        return 5;
    return 7;
}

// CASE nested if, else if
// RETURNS 11

int main() {
    int y = 54;
    if (y < 22)
        return 6;
    else if (y > 20) {
        if (y < 30)
            return 15;
        else if (y > 30) {
            return 11;
        } else {
            return 17;
        }
    }
    return 7;
}
