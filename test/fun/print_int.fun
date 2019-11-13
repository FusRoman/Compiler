println() {
  print(10);
  return 0;
}

println_char(c) {
  print(c);
  print(10);
  return 0;
}

println_int(v) {
  print_int(v);
  print(10);
  return 0;
}

print_int(v) {
  if (v == 0)
    print(48);
  else if (v < 0) {
    print(45);
    print_int_rec(-v);
  }
  else
    print_int_rec(v);

  return 0;
}

print_int_rec(v) {
  if (v > 0) {
    print_int_rec(v / 10);
    print(v % 10 + 48);
  }

  return 0;
}

main() {
  println_char(97);
  println_int(97);
  println_int(65535);
  println_int(-10);
  exit;
}