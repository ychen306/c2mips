struct Foo  {
	int a; 
	char b;  
	char c; 
	int d;
};

struct Bar {
	char c;
	struct Foo foo;
};

int sum_foo(struct Foo *foo)
{
	return foo->a + foo->b + foo->c + foo->d;
} 

void main()
{
	struct Bar b;
	b.foo.a = 1;
	b.foo.b = 3;
	b.foo.c = 5;
	b.foo.d = 7;
	void *p = &b;
	print_int(sum_foo(p + 4));
	print_str("\n");
}
