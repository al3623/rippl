#include <string.h>
#include "lib.h"
#include "mymap.h"
#include "thunk.h"
#include "natives.h"

struct Thunk *mapl(struct Thunk *list_thunk, struct Thunk *func) {
	struct List *list = list_thunk->value;

	struct Thunk *new_thunk = makeEmptyList(list->content_type);
	struct List *new = new_thunk->value;

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
	return init_thunk_literal(new);
}

struct Thunk *map_listl(struct Thunk *apps_thunk, struct Thunk *vals) {
	struct List *apps = invoke(apps_thunk);

	struct Thunk *new_thunk = makeEmptyList(INT);		// incorrect
	struct List *new = invoke(new_thunk);

	struct Node *curr_app_node = apps->head;

	while (curr_app_node) {
		struct Thunk *curr_app = curr_app_node->data;
		struct Thunk *applied_thunk = mapl(vals, curr_app);

		struct Thunk *stupid_thunk_list_wrapper_new 
			= init_thunk_literal(new);

		new = cat(stupid_thunk_list_wrapper_new,applied_thunk);
		curr_app_node = curr_app_node->next;
	}
	
	return init_thunk_literal(new);
}

struct Thunk *filterl(struct Thunk *list_thunk, struct Thunk *filter) {
	struct List *list = invoke(list_thunk);

	struct Thunk *new_thunk = makeEmptyList(list->content_type);
	struct List *new = new_thunk->value;

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
	return init_thunk_literal(new);
}
/*
int main() {
	int _0 = 0;
	int _1 = 1;
	int _2 = 2;
	int _5 = 5;
	int _10 = 10;
	struct Thunk *_2_thunk = init_thunk_literal(&_2);

	init_thunk(tail_init_thunk,tail_eval,1);

	init_thunk(cons_init_thunk,cons_eval,2);

	init_thunk(head_init_thunk,head_eval,1);

	init_thunk(neq_init_thunk,neq_eval,2);
	
	init_thunk(mult_init_thunk, mult_eval,2);

	struct Thunk *_0and1_thunk = 
		makeRangeList(init_thunk_literal(&_0),init_thunk_literal(&_1));

	struct Thunk *_10_thunk = 
		makeRangeList(init_thunk_literal(&_10),init_thunk_literal(&_10));

	struct Thunk *_2and10_thunk = apply(apply(cons_init_thunk,_2_thunk),
		_10_thunk);
	invoke(_2and10_thunk);	
	printAnyThunk(_2and10_thunk, 4);
	printf("\n");

	struct Thunk *two_thunk = apply(head_init_thunk,_2and10_thunk);
	int *two = invoke(two_thunk);
	printf("head [2,10]: %d\n", *two);
	struct Thunk *ten_thunk = apply(head_init_thunk,apply(tail_init_thunk,_2and10_thunk));
	int *ten = invoke(ten_thunk);
	printf("head tail [2,10]: %d\n", *ten);

	struct Thunk *neq0and1_thunk = mapl(_0and1_thunk,neq_init_thunk);

	struct Thunk *mult0and1_thunk = mapl(_0and1_thunk,mult_init_thunk);

	struct Thunk *mult2and10_thunk = mapl(_2and10_thunk,mult_init_thunk);

	struct Thunk *unfiltered_thunk = makeRangeList(init_thunk_literal(&_0),init_thunk_literal(&_5));
	printf("original: ");
	printAnyThunk(unfiltered_thunk, 4);
	printf("\n");

	struct Thunk *mult2 = invoke(apply(head_init_thunk,mult2and10_thunk));
	struct Thunk *mult10 = invoke(apply(head_init_thunk,apply(tail_init_thunk,mult2and10_thunk)));
	struct Thunk *neq0 = invoke(apply(head_init_thunk,neq0and1_thunk));
	struct Thunk *neq1 = invoke(apply(head_init_thunk,apply(tail_init_thunk,neq0and1_thunk)));

	printf("not 0 filter: ");	
	struct Thunk *not0_thunk = filterl(unfiltered_thunk,neq0);
	printAnyThunk(not0_thunk, 4);
	printf("\n");

	printf("not 1 filter: ");	
	struct Thunk *not1_thunk = filterl(unfiltered_thunk,neq1);
	printAnyThunk(not1_thunk, 4);
	printf("\n");

	printf("map *2 original: ");
	struct Thunk *map_mult2_unfiltered_thunk = mapl(unfiltered_thunk, mult2);
	printAnyThunk(map_mult2_unfiltered_thunk, 4);
	printf("\n");

	printf("map *10 not1: ");
	struct Thunk *map_mult10_not1_thunk = mapl(not1_thunk, mult10);
	printAnyThunk(map_mult10_not1_thunk, 4);
	printf("\n");

	struct Thunk *maplist_mult_thunk =
		map_listl(mult2and10_thunk,unfiltered_thunk);
	printf("map * [2,10] original: ");
	printAnyThunk(maplist_mult_thunk, 4);
	printf("\n");

	return 0;
}*/
