var t := 0;

int f(int t){
    return t + 2;
}

int main(){
    int j := f(97);
    print(j);
    (int,fun (int -> int),int) tuple1 := <(5, &f, 20)>;
    int t := (tuple1.(1))(97);
    print(t);

    int[] tab1 := [10 | 5];

    int[] tab2 := [1;2;3];

    int v := tab1[5];

    return 0;
}