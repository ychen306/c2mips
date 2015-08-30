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
	foo.a = 1;
	foo.b = 2;
	foo.c = 3;
	foo.d = 4; 
	print_int(sum_foo(&foo));
	print_str("\n");
}
