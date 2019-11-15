main(){
    var tuple := 0;
    make_tuple(&tuple,26);
    for(var i := 0; i < 26; i++){
        var case := tuple_access(tuple, i);
        *(case) := 97 + i;
    }
    #print_tuple(tuple);
    print(10);
    print(*(tuple_access(tuple, 24)));
    print(10);
    #print_tuple2(tuple);
    print(10);
    print(*(tuple_access(tuple,0)));
    print(10);
    var tuple2 := 0;
    make_tuple(&tuple2, 40);
    print(10);
    print(10);
    print(*(tuple_access(tuple,0)));
    *(tuple_access(tuple,0)) := 98;
    print(*(tuple_access(tuple,0)));
    print(*(tuple_access(tuple,1)));
    exit;
}

# pas besoin de déréférencer tuple_access comme dans le print du main
# comme on passe le tuple par référence, en fait on passe la valeur de la première case du tableau
# donc 97 et donc tuple_access se comporte comme un incrémenteur avec t qui vaut 97.
print_tuple(&t){
    for(var i := 0; i < 26; i++){
        print(tuple_access(t,i));
    }
    return 0;
}

# en revanche, ici, comme le tuple n'est plus passé par référence, on envoie l'adresse du tuple 
# en paramètre et donc il faut déréférencé tuple_access pour accéder à la ième case du tuple
print_tuple2(t){
    for(var i := 0; i < 26; i++){
        print(*(tuple_access(t,i)));
    }
    return 0;
}