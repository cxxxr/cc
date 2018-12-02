loop(n) {
  start:
  f(n);
  n = n + 1;
  if (n < 10) {
    goto start;
  }
}
