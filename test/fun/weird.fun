fun(b) {
  if (b) {
    print(97);
    return 97;
  }
  else {
    print(98);
    return 98;
  }
  return a;
}

main() {
  fun(true);
  fun(false);
  exit;
}