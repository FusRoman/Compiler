
type re := {a:int;b:int};

int main(){
    (int,int) f := <(1,2)>;

    ((int,int),int) c := <(<(1,2)>,3)>;
    ((int,int),int) d := <(<(1,2)>,3)>;
    ((int,int),int) g := <(f,3)>;
    ((int,int),int) e := <(<(1,3)>,3)>;

    if (c = g){
        print('o');
        print('k');
        print(10);
    }
    else{
        print('n');
        print('o');
        print('n');
        print(10);
    }

   #int[][] tab1 := [['a';'b'];['c';'d';'e']];

    int[] tab := [0;1;2;3];
    int[] tab2 := [0;1;2;3];
    int[] tab3 := [0;1;2;4];

    int[][] tab1 := [[0;1];[2;3]];
    int[][] tab4 := [[0;1];[2;3]];
    int[][] tab5 := [[0;1];[2;4]];
    if (tab1 = tab4){
        print('o');
        print('k');
        print(10);
    }
    else{
        print('n');
        print('o');
        print('n');
        print(10);
    }

    int[] t := ['a';'b'];

    (int[], int) t_a1 := <(t, 'c')>;
    (int[], int) t_a2 := <(t, 'c')>;
    (int[], int) t_a3 := <(['a';'d'], 'c')>;

    if (t_a1 = t_a2){
        print('o');
        print('k');
        print(10);
    }
    else{
        print('n');
        print('o');
        print('n');
        print(10);
    }

    (int, int)[] a_t1 := [<('a','b')>;<('c','d')>;<('e','f')>];
    (int, int)[] a_t2 := [<('a','b')>;<('c','d')>;<('e','f')>];
    (int, int)[] a_t3 := [<('a','d')>;<('c','d')>;<('e','f')>];
 
    if (a_t1 = a_t2){
        print('o');
        print('k');
        print(10);
    }
    else{
        print('n');
        print('o');
        print('n');
        print(10);
    }

    (int,int)[][][] t  := [[[<(1,2)>];[<(1,5)>]];[[<(3,4)>]]];
    (int,int)[][][] t1 := [[[<(1,2)>];[<(1,5)>]];[[<(3,4)>]]];
    (int,int)[][][] t2 := [[[<(1,4)>];[<(1,5)>]];[[<(3,4)>]]];

    {a:(int,int)[][][];b:int} r1 := {<{a:(int,int)[][][];b:int}> a := t; b := 0};
    {a:(int,int)[][][];b:int} r2 := {<{a:(int,int)[][][];b:int}> a := t; b := 0};
    {a:(int,int)[][][];b:int} r3 := {<{a:(int,int)[][][];b:int}> a := t2; b := 0};

    if (r1 = r2){
        print('o');
        print('k');
        print(10);
    }
    else{
        print('n');
        print('o');
        print('n');
        print(10);
    }

    <re>[] r1 := [{<<re>> a:= 'a'; b:= 'b'};{<<re>> a:= 'c'; b:= 'd'}];
    <re>[] r2 := [{<<re>> a:= 'a'; b:= 'b'};{<<re>> a:= 'c'; b:= 'd'}];
    <re>[] r3 := [{<<re>> a:= 'a'; b:= 'g'};{<<re>> a:= 'c'; b:= 'd'}];

    if (r1 = r2){
        print('o');
        print('k');
        print(10);
    }
    else{
        print('n');
        print('o');
        print('n');
        print(10);
    }

    exit;
}