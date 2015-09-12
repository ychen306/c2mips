int odd(int n);

int even(int n)
{
	if (n == 0)
		return 1;
	return !odd(n-1);
}

int odd(int n)
{ 
	if (n == 0)
		return 0; 
	return !even(n-1);
}

void main()
{
	print_int(odd(42)); // 0
	print_str("\n");
	print_int(even(42)); // 1
	print_str("\n");
} 
