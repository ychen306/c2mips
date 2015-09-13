struct foo { 
	int x;
	struct foo *foos[10];
};
void main()
{
	print_int(sizeof 1 + 1); // 5
	print_str("\n");
	print_int(sizeof (1 + 1)); // 4
	print_str("\n");
	int arr[10];
	print_int(sizeof arr); // 40
	print_str("\n");
	struct foo f;
	print_int(sizeof f); // 44
	print_str("\n");
}
