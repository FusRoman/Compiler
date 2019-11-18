make_array(&t, s, e){
    make_tuple(&t, s + 1);
    var length := tuple_access(t, 0);
    *(length) := s;
    for(var i := 1; i <= s; i++){
        var case := tuple_access(t,i);
        *(case) := e;
    }
    t := t + 1;
    return 0;
}

array_get(t, i){
    var length := *(tuple_access(t,-1));
    if( length <= i ){
        print(73);print(110);print(100);print(101);print(120); #Index
        print(65);print(114);print(114);print(97);print(121); #Array
        print(79);print(117);print(116); #Out
        print(79);print(102); #Of
        print(66);print(111);print(117);print(110);print(100);print(115); #Bounds
        print(10);
        exit;
    }
    else{
        return tuple_access(t, i);
    }
}
