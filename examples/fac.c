int fac_helper(int n, int acc)
{
	if (n < 2) return acc;
	else return fac_helper(n-1, n*acc);
}

int fac(int n)
{
	return fac_helper(n, 1);
}

int main()
{
	print_int(fac(5)); // 120
	print_str("\n");
}
