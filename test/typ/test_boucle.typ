

int main(){

    int[] tab := ['a';'b';'c';'d';'e';'f';'g';'h'];

    int i := 0;

    int a := 0;

    int[] tab2 := ['i';'j';'k';'l';'m'];

    tab2 := tab;
    if (tab == tab2){
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

    while( [i][0] < 10){
        print(tab[i]);
        a++;
        i:=a;
    }

    a := 0;
    for(int i := [a][0]; [a][0] < 10; i++){
        print(tab[i]);
        a++;
    }

    exit;
}