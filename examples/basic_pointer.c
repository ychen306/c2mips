int x;
void set_to_42(int *p)
{
	*p = 42;
}

void main()
{ 
	set_to_42(&x);
	print_int(x);
	print_str("\n");
}
