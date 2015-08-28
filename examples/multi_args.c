void inc(char *n)
{
	*n += 1;
}

int add(char a, char b, char c, char d, char e, char f)
{
	inc(&f);
	return a + b + c + d + e + f;
}

int main()
{
	print_int(add(1, 2, 3, 4, 5, 6)); // 22
	print_str("\n");
	return 0;
}
