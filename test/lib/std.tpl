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

parse_int(s) {
  var size := *(s - 1);
  var sum := 0;
  for (var i := 0; i < size; i++) {
    var c := *(s + i);
    if (c < 48 || c > 57) {
      print(69);
      print(82);
      print(82);
      print(79);
      print(82);
      print(10);
      exit;
    }
    sum := sum * 10 + (*(s + i) - 48);
  }
  return sum;
}