
type re := {a:int; b:int};

int kkk (g : int){
    return 96 + g;
}

int print_result(test: bool) {
    if (test) {
        print('o');
        print('k');
        print(10);
    } else {
        print('n');
        print('o');
        print('n');
        print(10);
    }
    return test;
}

int main() {
    var f: (int,int) := (1,2);

    var c: ((int,int),int) := ((1, 2), 3);
    var d: ((int,int),int) := ((1,2), 3);
    var g: ((int,int),int) := (f, 3);
    var e: ((int,int),int) := ((1, 3), 3);


    var h : int  := kkk(2);

    print(kkk(c = g));

    kkk(c=g);

    print_result(c = g);

    if (c = g) {
        print('o');
        print('k');
        print(10);
    } else {
        print('n');
        print('o');
        print('n');
        print(10);
    }

   #int[][] tab1 := [['a';'b'];['c';'d';'e']];

    var tab: int[] := [0;1;2;3];
    var tab2: int[] := [0;1;2;3];
    var tab3: int[] := [0;1;2;4];

    var tab1: int[][] := [[0;1]; [2;3]];
    var tab4: int[][] := [[0;1]; [2;3]];
    var tab5: int[][] := [[0;1]; [2;4]];
    if (tab1 = tab4) {
        print('o');
        print('k');
        print(10);
    } else {
        print('n');
        print('o');
        print('n');
        print(10);
    }

    var t: char[] := ['a';'b'];

    var t_a1: (int[], int) := (t, 'c');
    var t_a2: (int[], int) := (t, 'c');
    var t_a3: (int[], int) := (['a';'d'], 'c');

    if (t_a1 = t_a2) {
        print('o');
        print('k');
        print(10);
    } else {
        print('n');
        print('o');
        print('n');
        print(10);
    }

    var a_t1: (int, int)[] := [('a', 'b'); ('c', 'd'); ('e', 'f')];
    var a_t2: (int, int)[] := [('a', 'b'); ('c', 'd'); ('e', 'f')];
    var a_t3: (int, int)[] := [('a', 'd'); ('c', 'd'); ('e', 'f')];
 
    if (a_t1 = a_t2) {
        print('o');
        print('k');
        print(10);
    } else {
        print('n');
        print('o');
        print('n');
        print(10);
    }

    var t: (int,int)[][][] := [[[(1,2)]; [(1,5)]]; [[(3,4)]]];
    var t1: (int,int)[][][] := [[[(1,2)]; [(1,5)]]; [[(3,4)]]];
    var t2: (int,int)[][][] := [[[(1,4)]; [(1,5)]]; [[(3,4)]]];

    var r1: {a:(int,int)[][][]; b:int} := {<{a:(int,int)[][][]; b:int}> a := t; b := 0};
    var r2: {a:(int,int)[][][]; b:int} := {<{a:(int,int)[][][]; b:int}> a := t; b := 0};
    var r3: {a:(int,int)[][][]; b:int} := {<{a:(int,int)[][][]; b:int}> a := t2; b := 0};

    if (r1 = r2) {
        print('o');
        print('k');
        print(10);
    } else {
        print('n');
        print('o');
        print('n');
        print(10);
    }

    var r1: re[] := [{<re> a:= 'a'; b:= 'b'}; {<re> a:= 'c'; b:= 'd'}];
    var r2: re[] := [{<re> a:= 'a'; b:= 'b'}; {<re> a:= 'c'; b:= 'd'}];
    var r3: re[] := [{<re> a:= 'a'; b:= 'g'}; {<re> a:= 'c'; b:= 'd'}];

    if (r1 = r2) {
        print('o');
        print('k');
        print(10);
    } else {
        print('n');
        print('o');
        print('n');
        print(10);
    }

    exit;
}