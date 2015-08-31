int getone()
{
	print_str("getone() got executed\n");
	return 1;
}

void main()
{ 
	int true = 42;
	int false = 0;
	print_int(false && getone());
	print_str("\n");
	print_int(true && getone());
	print_str("\n");
} 
