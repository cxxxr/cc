loop(n, end) {
        start:
        log(n);
        n = n + 1;
        if (n <= end) {
                goto start;
        }
}

main() {
        loop(1, 5);
}
