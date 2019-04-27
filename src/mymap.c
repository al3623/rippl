#include <string.h>
#include "lib.h"
#include "mymap.h"
#include "thunk.h"

struct List *map(struct List *list, struct Thunk *func) {
	struct List *new = makeEmptyList(list->content_type);	// not right type
	struct Node *curr = list->head;

	while (curr) {
		struct Thunk *data = (curr->data);
		struct Thunk *newThunk = apply(func, data);

		struct Node *newNode = malloc(sizeof(struct Node));
		newNode->data = newThunk;
		newNode->next = NULL;
		appendNode(new, newNode);
		curr = curr->next;
	}
	return new;
}

struct List *map_list(struct List *apps, struct List *vals) {
	struct List *new = makeEmptyList(INT);		// incorrect
	struct Node *curr_app_node = apps->head;

	while (curr_app_node) {
		struct Thunk *curr_app = curr_app_node->data;
		struct List *applied = map(vals, curr_app);
		new = cat(new,applied);
		curr_app_node = curr_app_node->next;
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
			newNode->data = curr->data;
			newNode->next = NULL;
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

void *int_mult_eval(struct Thunk *t) {
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

void *int_nequal_eval(struct Thunk *t) {
	int *x = ((t->args)[0])->value;
	int *y = ((t->args)[1])->value;

	void *result = int_nequal(x,y);
	
	return result;
}

int main() {
	int _2 = 2;

	struct List *_0and1 = makeRangeList(0,1);
	explodeRangeList(_0and1);

	struct List *_2and10 = makeRangeList(10,10);
	explodeRangeList(_2and10);
	_2and10 = cons(&_2, _2and10);

	struct Thunk *zero = (_0and1->head)->data;
	struct Thunk *one = ((tail(_0and1))->head)->data;

	struct Thunk *two = (_2and10->head)->data;
	struct Thunk *ten = ((tail(_2and10))->head)->data;

	// (!=)
	struct Thunk *neq = init_thunk(int_nequal_eval, 2);
	struct List *neq0and1 = map(_0and1,neq);

	// (*)
	struct Thunk *mult = init_thunk(int_mult_eval, 2);
	struct List *mult0and1 = map(_0and1,mult);
	struct List *mult2and10 = map(_2and10,mult);

	// (!=).0
	struct Thunk *neq0 = head(neq0and1);
	// (!=).1
	struct Thunk *neq1 = head(tail(neq0and1));
	// (*).2
	struct Thunk *mult2 = head(mult2and10);
	// (*).10
	struct Thunk *mult10 = head(tail(mult2and10));

	// unfiltered = [0,1,2,3,4,5]
	struct List *unfiltered = makeRangeList(0,5);
	explodeRangeList(unfiltered);
	printf("original: ");	
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

	printf("map *2 original: ");
	struct List *map_mult2_unfiltered = map(unfiltered, mult2);
	printPrimList(map_mult2_unfiltered);
	printf("\n");

	printf("map *10 not1: ");
	struct List *map_mult10_not1 = map(not1, mult10);
	printPrimList(map_mult10_not1);
	printf("\n");

	struct List *maplist_mult = map_list(mult2and10,unfiltered);
	printf("map * [2,10] original: ");
	printPrimList(maplist_mult);
	printf("\n");

	return 0;
}
