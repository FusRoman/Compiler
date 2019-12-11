print_int(v) {
  print(10);
  if (v == 0)
    print(48);
  else if (v < 0) {
    print(45);
    print_int_rec(-v);
  }
  else
    print_int_rec(v);

  print(10);
  return 0;
}

print_int_rec(v) {
  if (v > 0) {
    print_int_rec(v / 10);
    print(v % 10 + 48);
  }

  return 0;
}

add(x, y) {
  return x + y;
}

test1(f, x, y) {
  *f(x, y);
  return function_result;
}

test2(&f, x, y) {
  f(x, y);
  return function_result;
}

main() {
  print(97);

  # Fonctionne
  tmp := &add;
  *tmp(48, 50);
  print(function_result);

  # Fonctionne pas lol
  test1(&add, 49, 50);
  print(function_result);

  # Non plus mdr
  test2(&add, 50, 50);
  print(function_result);

  # Il faut que j'aille dormir
  print(101);

  print(10);
  exit;
}

.data
  tmp: 0
