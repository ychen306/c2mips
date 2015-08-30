void main()
{
	int a[42];
	int *p = &a;
	print_int(p++ == a); // 1
	print_str("\n");
	print_int(p == a); // 0
	print_str("\n"); 
}
