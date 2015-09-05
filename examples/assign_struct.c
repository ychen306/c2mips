struct Foo {
	int a;
	int b;
};

void main()
{ 
	struct Foo x;
	x.a = 1;
	x.b = 2;
	struct Foo y = x;
	x.a = 3;
	x.b = 4;
	print_int(y.a); // 1
	print_str("\n");
	print_int(y.b); // 2
	print_str("\n");
	print_int(x.a); // 3
	print_str("\n");
	print_int(x.b); // 4
	print_str("\n");
}
