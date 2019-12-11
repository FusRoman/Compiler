
type re := {a:int; b:int};

int print_result(test: bool) {
    if (test) {
        print('o');
        print('k');
    } else {
        print('n');
        print('o');
        print('n');
    }
    print(10);
    return test;
}

int main() {
    var f: (int,int) := (1,2);

    var c: ((int,int),int) := ((1, 2), 3);
    var d: ((int,int),int) := ((1,2), 3);
    var g: ((int,int),int) := (f, 3);
    var e: ((int,int),int) := ((1, 3), 3);

    #print_result(c = g);
    #print_result(c <> e);

   #int[][] tab1 := [['a';'b'];['c';'d';'e']];

    var tab: int[] := [0;1;2;3];
    var tab2: int[] := [0;1;2;3];
    var tab3: int[] := [0;1;2;4];

    var tab1: int[][] := [[0;1]; [2;3]];
    var tab4: int[][] := [[0;1]; [2;3]];
    var tab5: int[][] := [[0;1]; [2;4]];

    print_result(tab1 = tab4);
    print_result(tab1 <> tab5);

    var t: char[] := ['a';'b'];

    var t_a1: (int[], int) := (t, 'c');
    var t_a2: (int[], int) := (t, 'c');
    var t_a3: (int[], int) := (['a';'d'], 'c');

    print_result(t_a1 = t_a2);
    print_result(t_a1 <> t_a3);

    var a_t1: (int, int)[] := [('a', 'b'); ('c', 'd'); ('e', 'f')];
    var a_t2: (int, int)[] := [('a', 'b'); ('c', 'd'); ('e', 'f')];
    var a_t3: (int, int)[] := [('a', 'd'); ('c', 'd'); ('e', 'f')];
 
    print_result(a_t1 = a_t2);
    print_result(a_t1 <> a_t3);

    var t: (int,int)[][][] := [[[(1,2)]; [(1,5)]]; [[(3,4)]]];
    var t1: (int,int)[][][] := [[[(1,2)]; [(1,5)]]; [[(3,4)]]];
    var t2: (int,int)[][][] := [[[(1,4)]; [(1,5)]]; [[(3,4)]]];

    var r1: {a:(int,int)[][][]; b:int} := {<{a:(int,int)[][][]; b:int}> a := t; b := 0};
    var r2: {a:(int,int)[][][]; b:int} := {<{a:(int,int)[][][]; b:int}> a := t; b := 0};
    var r3: {a:(int,int)[][][]; b:int} := {<{a:(int,int)[][][]; b:int}> a := t2; b := 0};

    print_result(r1 = r2);
    print_result(r1 <> r3);

    var r1: re[] := [{<re> a:= 'a'; b:= 'b'}; {<re> a:= 'c'; b:= 'd'}];
    var r2: re[] := [{<re> a:= 'a'; b:= 'b'}; {<re> a:= 'c'; b:= 'd'}];
    var r3: re[] := [{<re> a:= 'a'; b:= 'g'}; {<re> a:= 'c'; b:= 'd'}];

    print_result(r1 = r2);
    print_result(r1 <> r3);

    var i1: int := 97;
    var i2: int := 98;

    var p1: int* := &i1;
    var p2: int* := &i1;
    var p3: int* := &i2;

    print_result(p1 = p2);
    print_result(p1 <> p3);

    var p1: int* := null;
    var p2: int* := null;
    var p3: int* := &i2;

    print_result(p1 = p2);
    print_result(p1 = null);
    print_result(p1 == null);
    print_result(p3 <> null);
    print_result(p1 <> p3);

    exit;
}