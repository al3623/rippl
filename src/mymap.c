#include <string.h>
#include "lib.h"
#include "mymap.h"
#include "thunk.h"

struct List *map(struct List *list, struct Thunk *func) {
	struct List *new = makeEmptyList(list->content_type);	// is this right?
	struct Node *curr = list->head;

	while (curr) {
		struct Thunk *data = (curr->data); // always evaluated?
		struct Thunk *newThunk = apply(func, data);
		struct Node *newNode = malloc(sizeof(struct Node));
		newNode->data = newThunk;
		newNode->next = NULL;
		appendNode(new, newNode);
		curr = curr->next;
	}
	return new;
}

struct List *filter(struct List *list, struct Thunk *filter) {
	struct List *new = makeEmptyList(list->content_type);
	struct Node *curr = list->head;

	while (curr) {
		// thunk inside list
		struct Thunk *currThunk = curr->data;
		// number of args stored inside list
		int filled_args = currThunk->filled_args;

		struct Thunk **args = currThunk->args;
		struct Thunk *passesFilter = filter;	

		while (filled_args) {
			passesFilter = apply(passesFilter, *args);
			args++;
			filled_args--;
		}
		void *value = invoke(passesFilter);
		
		int passed = *(int *)value;

		if (passed) {
			struct Node *newNode = malloc(sizeof(struct Node));
			struct Thunk *newThunk = malloc(sizeof(struct Thunk));
			memcpy(newThunk, curr->data, sizeof(struct Thunk));
			newThunk->args = malloc(sizeof(struct Thunk *) * 
				(curr->data)->num_args);
			memcpy(newThunk->args, (curr->data)->args, 
				sizeof(struct Thunk *) * (curr->data)->num_args);

			appendNode(new,newNode);
		}
		curr = curr->next;
	}
	return new;
}

int *int_mult(int *data1, int *data2) {
	int *result = malloc(sizeof(int));

	int x_ = *data1;
	int y_ = *data2;

	*result = x_ * y_;

	return result;
}

void *eval_int_mult(struct Thunk *t) {
	int *x = ((t->args)[0])->value;
	int *y = ((t->args)[1])->value;

	void *result = int_mult(x,y);
	
	return result;
}

int *int_nequal(int *data1, int *data2) {
	int *result = malloc(sizeof(int));

	int x_ = *data1;
	int y_ = *data2;

	*result = x_ != y_;

	return result;
}

void *eval_int_nequal(struct Thunk *t) {
	int *x = ((t->args)[0])->value;
	int *y = ((t->args)[1])->value;

	void *result = int_mult(x,y);
	
	return result;
}

/*
struct Thunk *int_mult_thunk(struct Thunk *thunk, void*arg) {
	struct Thunk *new_thunk = malloc(sizeof(struct Thunk));
	memcpy(new_thunk, thunk, sizeof(struct Thunk));
	new_thunk->args = malloc(new_thunk->num_args * sizeof(struct Thunk*));
	memcpy(new_thunk->args, thunk->args, 
		new_thunk->num_args * sizeof(struct Thunk *));

	if (new_thunk->filled_args == new_thunk->num_args -1) {

		int x1 = *(int *) ((new_thunk->args)[0]);
		int y1 = *(int *) arg;

		void *res = int_mult(x1,y1);

		new_thunk->value = res;
	} else if (new_thunk->filled_args < new_thunk->num_args - 1) {

		(new_thunk->args)[new_thunk->filled_args] = arg;
		new_thunk->filled_args++;

	} else {
		fprintf(stderr, "not a thunk, can't be applied");
		exit(1);
	}
	return new_thunk;
}

struct Thunk *int_nequal_thunk(struct Thunk *thunk, void*arg) {
	struct Thunk *new_thunk = malloc(sizeof(struct Thunk));
	memcpy(new_thunk, thunk, sizeof(struct Thunk));
	new_thunk->args = malloc(new_thunk->num_args * sizeof(void *));
	memcpy(new_thunk->args, thunk->args, new_thunk->num_args * sizeof(void *));

	if (new_thunk->filled_args == new_thunk->num_args -1) {

		int x1 = *(int *) ((new_thunk->args)[0]);
		int y1 = *(int *) arg;

		void *res = int_nequal(x1,y1);

		new_thunk->value = res;
	} else if (new_thunk->filled_args < new_thunk->num_args - 1) {

		(new_thunk->args)[new_thunk->filled_args] = arg;
		new_thunk->filled_args++;

	} else {
		fprintf(stderr, "not a thunk, can't be applied");
		exit(1);
	}
	return new_thunk;
}

int main() {
	int zero = 0;
	int one = 1;
	int two = 2;
	int ten = 10;

	// (!=)
	struct Thunk *neq = init_thunk(int_nequal_thunk, 2);
	// (*)
	struct Thunk *mult = init_thunk(int_mult_thunk, 2);

	// (!=).0
	struct Thunk *neq0 = apply(neq, &zero);
	// (!=).1
	struct Thunk *neq1 = apply(neq, &one);
	// (*).2
	struct Thunk *mult2 = apply(mult, &two);
	// (*).10
	struct Thunk *mult10 = apply(mult, &ten);

	// unfiltered = [0,1,2,3,4,5]
	struct List *unfiltered = makeRangeList(0,5);
	explodeRangeList(unfiltered);	
	printRangeList(unfiltered);
	printf("\n");

	// not0 = x over unfiltered, x != 0
	struct List *not0 = filter(unfiltered, neq0);
	printf("!= 0 filter: ");
	printPrimList(not0);
	printf("\n");

	// not1 = y over unfiltered, y != 1
	struct List *not1 = filter(unfiltered, neq1);
	printf("!= 1 filter: ");
	printPrimList(not1);
	printf("\n");

	struct List *map_mult2_unfiltered = map(unfiltered, mult2);
	printPrimList(map_mult2_unfiltered);
	printf("\n");

	struct List *map_mult10_not1 = map(not1, mult10);
	printPrimList(map_mult10_not1);
	printf("\n");

	return 0;
}
*/
