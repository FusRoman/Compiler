print_int_rec(v) {
  if (v > 0) {
    print_int_rec(v / 10);
    print(v % 10 + 48);
  }

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

println_int(v) {
  print_int(v);
  print(10);
  return 0;
}

main() {
  print(100); print(10);
  println_int(memory_break@);
  print(10);
  var a := 0;
  while (true) {
    print_int(a);
    print(32);
    print(47);
    print(32);
    println_int(memory_break@);
    malloc(200);
    a++;
  }
  exit;
}