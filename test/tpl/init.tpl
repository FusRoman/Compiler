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

  print(10);
  return 0;
}

print_size(size) {
  print(115);
  print(105);
  print(122);
  print(101);
  print(58);
  print(32);
  print_int(size);
  return 0;
}

main() {
  print_int(memory_break@);
  var size := *(argv - 1);
  print_size(size);
  print(10);

  for (var i := 0; i < size; i++) {
    var arg := *(argv + i);
    var size := *(arg - 1);
    print_size(size);
    print_int(argv + i);
    print_int(arg);

    var j := 0;
    for (; j < size; j++)
      print(*(arg + j));
    print(10);
    print_int(arg + j);
    print(10);
  }

  exit;
}