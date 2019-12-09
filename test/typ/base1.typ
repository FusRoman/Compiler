var t := 0;

int f(int t){
    return t + 2;
}

int main(){
    int j := f(97);
    print(j);
    (int,fun (int -> int),int) tuple1 := <(97, &f, 20)>;
    int gg := tuple1.(0);
    int t := (*(tuple1.(1)))(97);
    print(t);

    int[] tab1 := [10 | 5];

    int[] tab2 := [1;2;3];

    int v := tab1[5];

    int v2 := tab2[1] + 3 - tab1[2];

    int v3 := tuple1.(0);

    return 0;
}