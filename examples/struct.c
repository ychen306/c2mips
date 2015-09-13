struct Foo  {
	int a; 
	char b;  
	char c; 
	int d;
};

int sum_foo(struct Foo *foo)
{
	return foo->a + foo->b + foo->c + foo->d;
}

void main()
{
	struct Foo foo;
	int *p;
	foo.a = 1;
	foo.b = 2;
	foo.c = 3;
	p = 3 + &(foo.c);
	*p = 4;
	print_int(sum_foo(&foo)); // 10
	print_str("\n");
}
