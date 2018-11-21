fact(n) {
        if (n < 1) 1;
        else fact(n-1)*n;
}

main() {
        fact(5);
}
