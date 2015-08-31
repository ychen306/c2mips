/*
 * copied from here
 * https://raw.githubusercontent.com/danluu/malloc-tutorial/master/malloc.c
 *
 * made some adaption for this compiler, which only accepts a subset of c
 */

struct block_meta {
	int size;
	struct block_meta *next;
	int free;
};


void *global_base = NULL;

struct block_meta *find_free_block(struct block_meta **last, int size) {
	struct block_meta *current = global_base;
	while (current && !(current->free && current->size >= size)) {
		*last = current;
		current = current->next;
	}
	return current;
}

struct block_meta *request_space(struct block_meta* last, int size) {
	struct block_meta *block;
	block = sbrk(0);
	void *request = sbrk(size + sizeof(*block));
	if (request == (void*) -1) {
		return NULL; // sbrk failed.
	}

	if (last) { // NULL on first request.
		last->next = block;
	}
	block->size = size;
	block->next = NULL;
	block->free = 0;
	return block;
}

// If it's the first ever call, i.e., global_base == NULL, request_space and set global_base.
// Otherwise, if we can find a free block, use it.
// If not, request_space.
void *malloc(int size) {
	struct block_meta *block;

	if (size <= 0) {
		return NULL;
	}

	if (!global_base) { // First call.
		block = request_space(NULL, size);
		if (!block) {
			return NULL;
		}
		global_base = block;
	} else {
		struct block_meta *last = global_base;
		block = find_free_block(&last, size);
		if (!block) { // Failed to find free block.
			block = request_space(last, size);
			if (!block) {
				return NULL;
			}
		} else {      // Found free block
			// TODO: consider splitting block here.
			block->free = 0;
		}
	}

	return(block+1);
}

struct block_meta *get_block_ptr(void *ptr) {
	struct blockmeta *bptr = ptr;
	return bptr - 1;
}

void free(void *ptr) {
	if (!ptr) {
		return;
	}

	struct block_meta* block_ptr;
	block_ptr = get_block_ptr(ptr);
	block_ptr->free = 1;
} 
