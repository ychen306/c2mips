int sum_arr(int arr[], int size)
{
	int i;
	int sum = 0;
	for (i = 0; i < size; i++) {
		sum += arr[i];
	}
	return sum;
}



void main()
{
	int arr[3];
	int *p = &arr;
	p[0] = 1; 
	p[1] = 2;
	p[2] = 3;
	print_int(sum_arr(arr, 3));
	print_str("\n");
}
