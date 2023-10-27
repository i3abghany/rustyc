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
    if (0)
        return 6;
    else if (1)
        return 5;
    return 7;
}

// CASE nested if, else if
// RETURNS 11

int main() {
    if (0)
        return 6;
    else if (1) {
        if (0)
            return 15;
        else if (1) {
            return 11;
        } else {
            return 17;
        }
    }
    return 7;
}
