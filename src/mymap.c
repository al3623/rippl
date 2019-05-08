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

	struct Thunk head_init_thunk[1];
	init_thunk(head_init_thunk,head_eval,1);

	struct Thunk neq_init_thunk[1];
	init_thunk(neq_init_thunk,neq_eval,2);
	
	struct Thunk mult_init_thunk[1];
	init_thunk(mult_init_thunk, mult_eval,2);

	struct List *_0and1 = makeRangeList(0,1);
	explodeRangeList(_0and1);
	struct Thunk *_0and1_thunk = init_thunk_literal(_0and1);

	struct List *_10 = makeRangeList(10,10);
	explodeRangeList(_10);
	struct Thunk *_10_thunk = init_thunk_literal(_10);

	struct Thunk *_2and10_thunk = apply(apply(cons_init_thunk,_2_thunk),
		_10_thunk);
	invoke(_2and10_thunk);
	
	printPrimList(_2and10_thunk->value);
	printf("\n");

	struct Thunk *two_thunk = apply(head_init_thunk,_2and10_thunk);
	int *two = invoke(two_thunk);
	printf("head [2,10]: %d\n", *two);
	struct Thunk *ten_thunk = apply(head_init_thunk,apply(tail_init_thunk,_2and10_thunk));
	int *ten = invoke(ten_thunk);
	printf("head tail [2,10]: %d\n", *ten);

	struct List *neq0and1 = map(_0and1,neq_init_thunk);
	struct Thunk *neq0and1_thunk = init_thunk_literal(neq0and1);
	struct List *mult0and1 = map(_0and1,mult_init_thunk);
	struct Thunk *mult0and1_thunk = init_thunk_literal(mult0and1);
	struct List *mult2and10 = map(_2and10_thunk->value,mult_init_thunk);
	struct Thunk *mult2and10_thunk = init_thunk_literal(mult2and10);

	struct List *unfiltered = makeRangeList(0,5);
	explodeRangeList(unfiltered);
	struct Thunk *unfiltered_thunk = init_thunk_literal(unfiltered);
	printf("original: ");
	printPrimList(unfiltered_thunk->value);
	printf("\n");

	struct Thunk *neq0 = invoke(apply(head_init_thunk,neq0and1_thunk));
	// printf("%p\n",neq_eval);
	// printf("%p\n",neq0->eval);

	struct Thunk *neq1 = invoke(apply(head_init_thunk,apply(tail_init_thunk,neq0and1_thunk)));

	printf("not 0 filter: ");	
	struct List *not0 = filter(unfiltered,neq0);
	printPrimList(not0);
	printf("\n");

	printf("not 1 filter: ");	
	struct List *not1 = filter(unfiltered,neq1);
	printPrimList(not1);
	printf("\n");
/*
	// (!=).1
	struct Thunk *neq1 = head(tail(neq0and1));
	// (*).2
	struct Thunk *mult2 = head(mult2and10);
	// (*).10
	struct Thunk *mult10 = head(tail(mult2and10));

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
