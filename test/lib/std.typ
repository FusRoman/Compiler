type string := int[];

int print_char(v: char) {
  print(v);
  return 0;
}

int println_char(v: char) {
  print(v);
  print('\n');
  return 0;
}

int print_int_rec(v: int) {
  if (v > 0) {
    print_int_rec(v / 10);
    print(v % 10 + '0');
  }
  return 0;
}

int print_int(v: int) {
  if (v == 0)
    print('0');
  else if (v < 0) {
    print('-');
    print_int_rec(-v);
  }
  else
    print_int_rec(v);
  return 0;
}

int println_int(v: int) {
  print_int(v);
  print('\n');
  return 0;
}

int print_string(s: string) {
  for (var i: int := 0; i < s.size; i++)
    print(s[i]);
  return 0;
}

int println_string(s: string) {
  print_string(s);
  print('\n');
  return 0;
}

int parse_int(s: string) {
  var sum: int := 0;
  for (var i: int := 0; i < s.size; i++) {
    var c: char := s[0];
    if (c < '0' || c > '9') {
      println_string("ERROR");
      exit;
    }
    sum := sum * 10 + (s[i] - '0');
  }
  return sum;
}