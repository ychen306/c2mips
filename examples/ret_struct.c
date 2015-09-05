struct Foo {
	int a;
	int b;
	char c;
};

struct Foo get_foo()
{
	struct Foo f;
	f.a = 10;
	f.b = 20;
	f.c = 30;
	return f;
}

void main()
{
	struct Foo f;
	f = get_foo();
	print_int(f.a + f.b + f.c); // 60
	print_str("\n");
}
