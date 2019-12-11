print_int_rec(v) {
	if ((v > 0)) {
		*stack_pointer := (v / 10);
		stack_pointer--;
		print_int_rec();
		stack_pointer += 1;
		print(((v % 10) + 48));
	}
	return 0;
}

print_int(v) {
	if ((v == 0)) {
		print(48);
	} else {
		if ((v < 0)) {
			print(45);
			*stack_pointer := (-v);
			stack_pointer--;
			print_int_rec();
			stack_pointer += 1;
		} else {
			*stack_pointer := v;
			stack_pointer--;
			print_int_rec();
			stack_pointer += 1;
		}
	}
	return 0;
}

parse_int(s) {
	stack_pointer -= 4;
	*(frame_pointer - 2) := (*(s - 1));
	*(frame_pointer - 3) := 0;
	for (*(frame_pointer - 4) := 0; ((*(frame_pointer - 4)) < (*(frame_pointer - 2))); *(frame_pointer - 4)++) {
		*(frame_pointer - 5) := (*(s + (*(frame_pointer - 4))));
		if ((((*(frame_pointer - 5)) < 48) || ((*(frame_pointer - 5)) > 57))) {
			print(69);
			print(82);
			print(82);
			print(79);
			print(82);
			print(10);
			exit;
		}
		*(frame_pointer - 3) := (((*(frame_pointer - 3)) * 10) + ((*(s + (*(frame_pointer - 4)))) - 48));
	}
	return (*(frame_pointer - 3));
}

.data
