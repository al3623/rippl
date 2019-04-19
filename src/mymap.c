#include "lib.h"
#include "thunk.h"

struct List *map(struct List *list, struct Thunk *func) {
	struct List *new = makeEmptyList(list->content_type);	// is this right?
	struct Node *curr = list->head;

	while (curr) {
		void *data = (curr->data)->value; // always evaluated?
		struct Thunk *newThunk = invoke(func, data);
		struct Node *newNode = makeNode(newThunk);
		appendNode(new, newNode);
		curr = curr->next;
	}
	return new;
}
