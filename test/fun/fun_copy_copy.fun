main() {
	print(97);
	print(10);
	a := (&fact);
	t := *a(5, (&mult));
	print(t);
	print(10);
	t := fact2(5, (&mult));
	print(t);
	print(10);
	exit;
}

fact(n, fun) {
	if ((n <= 1)) {
		return(1);
	} else {
		i := fact((n - 1), fun);
		i := *fun(i, n);
		return(i);
	}
}

fact2(n, &fun) {
	if ((n <= 1)) {
		return(1);
	} else {
		i := fact2((n - 1), (&fun));
		i := fun(i, n);
		return(i);
	}
}

mult(x, y) {
	return((x * y));
}

add(x, y) {
	return((x + y));
}

.data
a: 0
t: 0
i: 0
