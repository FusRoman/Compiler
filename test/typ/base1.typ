var t := 0;

int f(int t){
    return t + 2;
}

int h(int[] tab){
    return tab[1] + 1;
}

int[] init_tab(int size, int elt){
    return [size | elt];
}

int main(){
    int j := f(97);
    print(j);
    (int,fun (int -> int),int) tuple1 := <(97, &f, 100)>;
    int gg := tuple1.(2);
    print(gg);
    
    #int t := (*(tuple1.(1)))(97);
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
    print(f([a+b;98;99;100][0]));

    exit;
}