int getone()
{
	print_str("getone() got executed -- ");
	return 1;
}

void main()
{ 
	int true = 42;
	int false = 0;
	print_str("false && getone():\n");
	print_str("result: ");
	print_int(false && getone());
	print_str("\n");
	print_str("\n");

	print_str("true && getone():\n");
	print_str("result: ");
	print_int(true && getone());
	print_str("\n");
	print_str("\n");

	print_str("true || getone():\n");
	print_str("result: ");
	print_int(true || getone());
	print_str("\n");
	print_str("\n");

	print_str("false || getone():\n");
	print_str("result: ");
	print_int(false || getone());
	print_str("\n");
} 
