var t: int := 0;

int f(t: int) {
    return t + 2;
}

int h(tab: int[]) {
    return tab[1] + 1;
}

int[] init_tab(size: int, elt: int) {
    return [size | elt];
}

(int,int,int) fun_tuple(b: int, c: int) {
    return (97, b, 97 + b + c);
}

int arg_tuple(t: (int,int)) {
    return t.(0) + t.(1);
}

int arg_record(r: {a:int;b:int}) {
    return r.a;
}

{a:int; b:int} return_record(a: int, b: int) {
    return {<{a:int; b:int}> a := a; b := b};
}

mdr fait_des_chose(g: mdr) {
    return {<mdr> a := g.b; b := g.a};
}

type lol := {a: int; b: int};
type mdr := lol;

type mild_clusterfuck := {c: int[]; d: fun (int, int -> int)};
type ultimate_clusterfuck := {a: (mild_clusterfuck, int); b: int};

int add(x: int, y: int) {
    return x + y;
}

int main() {
    /*
        int j := f(97);
        print(j);
        (int,fun (int -> int),int) tuple1 := <(97, &f, 100)>;
        int gg := tuple1.(2);
        print(gg);
    */

    /*
        {a:int;b:int} r1 := {<{a:int;b:int}> b:= 98; a := f(96) };
        print(r1.b);
        print(arg_tuple(<(97,2)>));
        print(f(<(97,98,98)>.(0)));
        print(arg_record({<{a:int;b:int}> a:= 97; b := 98}));
        print((return_record(97,98)).a);
    */

    /*
        <mdr> r2 := {<<mdr>> b:= 98; a:= 97};
        print(r2.a);
        print((fait_des_chose(r2)).a);
    */

    var fuck: ultimate_clusterfuck := {<ultimate_clusterfuck> 
        b := 'd';
        a := ({<mild_clusterfuck>
                c := ['a';'b';'c'];
                d := &add
             }, 102)
    };
    print(fuck.b);
    var tmp1: (mild_clusterfuck, int) := fuck.a;
    var tmp2: mild_clusterfuck := tmp1.(0);
    var tmp3: fun (int, int -> int) := tmp2.d;
    print(*tmp3(47, 50));

    print(*(fuck.a.(0).d)(47, 50));
    #print(((fuck.a).c));

    /*
        #fun (int->int)[] tab_fun := [&f];
        #int tmp := (*(tab_fun[0]))(97);
        #int t := (*tuple1.(1))(97);
        #print(t);
        int size := 10;
        int[] tab1 := [size | 97];
        print(tab1[9]);
        print(10);
        print(10);
        for(int i := 0; i < 10; i++){
            print(tab1[i]);
            print(10);
        }
        int[] tab2 := [100;tab1[5];102];
        for(int i := 0; i < 3; i++){
            print(tab2[i]);
            print(10);
        }
        print(10);
        print(h(tab2));
        print(10);
        int[] tab3 := init_tab(20, 99);
        for(int i := 0; i < 20; i++){
            print(tab3[i]);
            print(10);
        }
        print(10);
        print(10);
        int a := 97;
        int b := 2;
        print(f([f(a+b);98;99;100][0]));
        print([97;98;99;100][2] + 3 + b);
        print(10);
        print(10);
        print(<(97,a+b,99)>.(1));
        print((fun_tuple(2,1)).(2));
    */

    print(10);
    exit;
}