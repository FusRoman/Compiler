main(){
    var tuple := 0;
    make_tuple(tuple,26);
    var tuple2 := 0;
    make_tuple(tuple2,26);
    print(tuple+97);
    print(tuple2+97);
    var toto := 0;
    var toto2 := 0;
    test(&toto,&toto2);
    print(10);
    print(toto);
    print(toto2);
    exit;
}

test(&a,&b){
    a := 97;
    b := 98;
    return 0;
}