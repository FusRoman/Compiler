int print_string(s: string) {
  for (var i: int := 0; i < s.size; i++)
    print(s[i]);
  print(10);
  return 0;
}

int main() {
  print_string("coucou, je suis tres en retard");
  for (var i: int := 0; i < argc; i++)
    print_string(argv[i]);
  exit;
}