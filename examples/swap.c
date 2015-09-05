void swap(int *a, int *b)
{
	int t = *a;
	*a = *b;
	*b = t;
}

int main()
{
	int a = 42;
	int b = 1;
	swap(&a, &b);
	print_int(a);
	print_str("\n"); // 1
	print_int(b);
	print_str("\n"); // 42
}
