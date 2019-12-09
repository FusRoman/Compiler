main() {
  # Copie des arguments du main
  var argv_copy := malloc(argc + 1);
  *argv_copy := argc;
  argv_copy++;
  for (var i := 0; i < argc; i++) {
    var arg := *(argv + i);
    var size := *(arg - 1);
    var arg_copy := malloc(size + 1);
    *(argv_copy + i) := arg_copy;
    *(arg_copy - 1) := size;
    for (var j := 0; j < size; j++)
      *(arg_copy + j) := *(arg + j);
  }

  # Affichage
  for (var i := 0; i < *(argv_copy - 1); i++) {
    var arg := *(argv_copy + i);
    for (var j := 0; j < *(arg - 1); j++)
      print(*(arg + j));
    print(10);
  }

  exit;
}