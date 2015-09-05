struct Foo {
	int a;
	int b;
	char c;
};

void print_foo(struct Foo f)
{
	print_str("a: ");
	print_int(f.a);
	print_str("\nb: ");
	print_int(f.b);
	print_str("\nc: ");
	print_int(f.c);
	print_str("\n");
}

void main()
{
	struct Foo f; 
	f.a = 123;
	f.b = 456;
	f.c = 78;
	print_foo(f);
}
