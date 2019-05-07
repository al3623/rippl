#include <string.h>
#include "lib.h"
#include "mymap.h"
#include "thunk.h"
#include "natives.h"

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
		struct Thunk *stupid_thunk_list_wrapper_applied = init_thunk_literal(applied);
		struct Thunk *stupid_thunk_list_wrapper_new = init_thunk_literal(new);
		new = cat(stupid_thunk_list_wrapper_new,stupid_thunk_list_wrapper_applied);
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


int main() {
	int _2 = 2;
	struct Thunk *_2_thunk = init_thunk_literal(&_2);

	struct Thunk tail_init_thunk[1];
	init_thunk(tail_init_thunk,tail_eval,1);

	struct Thunk cons_init_thunk[1];
	init_thunk(cons_init_thunk,cons_eval,2);

	struct List *_0and1 = makeRangeList(0,1);
	explodeRangeList(_0and1);
	struct Thunk *_0and1_thunk = init_thunk_literal(_0and1);

	struct List *_10 = makeRangeList(10,10);
	explodeRangeList(_10);
	struct Thunk *_10_thunk = init_thunk_literal(_10);

	struct Thunk *_2and10_thunk = apply(apply(cons_init_thunk,_2_thunk),
		_10_thunk);
/*
	struct Thunk *zero = (_0and1->head)->data;
	struct Thunk *one = ((tail(_0and1_thunk))->head)->data;

	struct Thunk *two = (_2and10->head)->data;
	struct Thunk *ten = ((tail(_2and10))->head)->data;

	// (!=)
	struct Thunk ne[1];
	init_thunk(ne,neq_eval, 2);
	struct List *neq0and1 = map(_0and1,ne);

	// (*)
	struct Thunk mul[1];
	init_thunk(mul,mult_eval, 2);
	struct List *mult0and1 = map(_0and1,mul);
	struct List *mult2and10 = map(_2and10,mul);

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
*/
	return 0;
}
