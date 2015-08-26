int fib(int n)
{
	if (n < 2) {
		return n;
	} else {
		return fib(n-1) + fib(n-2);
	}
}

int main()
{
	int i;
	for (i = 0; i < 20; i++) {
		print_int(fib(i));
		print_str("\n");
	}
	return 0;
}
