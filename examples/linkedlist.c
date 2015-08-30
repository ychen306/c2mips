struct node { 
	int v;
	struct node *next;
};

void print_list(struct node *n)
{
	while (n != NULL) {
		print_int(n->v);
		print_str(", ");
		n = n->next;
	}
}

void main()
{
	struct node *head;
	struct node *snd;
	struct node *trd;
	head = sbrk(8);
	snd = sbrk(8);
	trd = sbrk(8);
	head->v = 1;
	head->next = snd;
	snd->v = 2;
	snd->next = trd;
	trd->v = 3;
	trd->next = NULL;
	print_list(head);
	print_str("\n");
}
