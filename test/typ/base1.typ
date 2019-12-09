var t := 0;

int f(int t){
    return t + 2;
}

int main(){
    fun (int -> int) j := f(97);
    print(j);
    (int,fun (int -> int),int) tuple1 := <(5, &f, 20)>;
    int t := (tuple1.(1))(97);
    print(t);
    return 0;
}