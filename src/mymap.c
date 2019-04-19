#include "lib.h"
#include "thunk.h"

struct List *map(struct List *list, struct Thunk *func) {
	struct List *new = makeEmptyList(list->content_type);	// is this right?
	struct Node *curr = list->head;

	while (curr) {
		void *data = (curr->data)->value; // always evaluated?
		struct Thunk *newThunk = apply(func, data);
		struct Node *newNode = makeNode(newThunk);
		appendNode(new, newNode);
		curr = curr->next;
	}
	return new;
}

struct List *filter(struct List *list, struct Thunk *filter) {
	struct List *new = makeEmptyList(list->content_type);
	struct Node *curr = list->head;

	while (curr) {
		struct Thunk *currThunk = curr->data;
		int filled_args = currThunk->filled_args;
		void **currArg = currThunk->args;
		struct Thunk *passesFilter = filter;	

		while (filled_args) {
			passesFilter = apply(passesFilter, currArg);
			currArg++;
			filled_args--;
		}	
		void *value = invoke(passesFilter);
		
		int passed = *(int *)value;
		if (passed) {
			struct Node *newNode = makeNode(curr->data);
			appendNode(new,newNode);
		}
		curr = curr->next;
	}
	return new;
}
