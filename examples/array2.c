void print_arr(int arr[], int size)
{
	int i;
	for (i = 0; i < size; i++) {
		print_int(arr[i]);
		print_str("\n");
	}
}

void swap(int arr[], int a, int b)
{
	int t = arr[a];
	arr[a] = arr[b];
	arr[b] = t;
} 

void main()
{
	int arr[3];
	int *p = arr;
	p[0] = 1; 
	p[1] = 2;
	p[2] = 3;
	print_arr(arr, 3);
	swap(arr, 0, 2);
	print_arr(arr, 3);
}
