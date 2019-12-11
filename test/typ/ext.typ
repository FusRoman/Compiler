type r1 := {a: int; b: int};

type r2 extends r1 {c: int};

int do_something(r: r1) {
  print(r.a);
  print(r.b);
  print(10);
  return 0;
}

int do_something_the_sequel(r: r2) {
  do_something(r);
  print(r.c);
  print(10);
  return 0;
}

int main() {
  var a: r2 := {<r2> a := 'a'; b := 'b'; c := 'c'};
  do_something(a);
  do_something_the_sequel(a);

  var b: r1 := {<r2> a := 'd'; b := 'e'; c := 'f'};
  do_something(b);
  #do_something_the_sequel(b);

  var c: r1 := {<r1> a := 'g'; b := 'h'};
  do_something(c);
  #do_something_the_sequel(c);

  c := a;
  do_something(c);

  exit; 
}