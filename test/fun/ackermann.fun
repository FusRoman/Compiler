ackermann(m, n) {
    if (m == 0)
        return(n+1);
    else if (m > 0 && n == 0) {
        ackermann(m-1, 1);
        return(function_result);
    } else {
        a := ackermann(m, n-1);
        ackermann(m-1, a);
        return(function_result);
    }
}

main() {
    print(97);
    print(10);

    for (i := 96; i < 122; i++) {
        ackermann(0, i);
        print(function_result);
    }

    print(10);
    exit;
}

.data
a: 0
i: 0

