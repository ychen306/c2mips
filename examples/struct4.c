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

struct Foo make_struct(int a, int b, int c)
{
	struct Foo f; 
	f.a = a;
	f.b = b;
	f.c = c;
	return f;
}

void main()
{
	print_foo(make_struct(123, 456, 78));
}
