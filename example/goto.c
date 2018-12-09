loop(n, end) {
        start:
        print(n);
        n = n + 1;
        if (n <= end) {
                goto start;
        }
}

main() {
        loop(1, 5);
}
