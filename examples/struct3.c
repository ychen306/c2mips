struct Foo  {
	int a; 
	char b;  
	char c; 
	int d;
};

struct Bar {
	char c;
	struct Foo *foo;
};

int sum_foo(struct Foo *foo)
{
	return foo->a + foo->b + foo->c + foo->d;
} 

void main()
{
	struct Bar b;
	struct Foo foo;
	foo.a = 1;
	foo.b = 3;
	foo.c = 5;
	foo.d = 7;
	b.foo = &foo;
	print_int(sum_foo(b.foo)); // 16
	print_str("\n");
}
