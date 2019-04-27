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
/*
		fprintf(stderr, "func arg: %d\n",
			*(int *)((func->args)[0]->value));
		fprintf(stderr, "app map on: %d\n",
			*(int *)(data->value));

		fprintf(stderr, "newThunk num_args: %d\n",
			newThunk->num_args);
		fprintf(stderr, "newThunk filled_args: %d\n",
			newThunk->filled_args);
		fprintf(stderr, "newThunk args[0]: %d\n",
			*(int *)((newThunk->args)[0]->value));
		fprintf(stderr, "newThunk args[1]: %d\n",
			*(int *)((newThunk->args)[1]->value));
*/
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
	struct Node *curr_app = apps->head;

	while (curr_app) {

		curr_app = curr_app->next;
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
/*			struct Thunk *newThunk = malloc(sizeof(struct Thunk));
			memcpy(newThunk, curr->data, sizeof(struct Thunk));
			newThunk->args = malloc(sizeof(struct Thunk *) * 
				(curr->data)->num_args);
			memcpy(newThunk->args, (curr->data)->args, 
				sizeof(struct Thunk *) * (curr->data)->num_args);
*/
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
/*
int main() {
	int _0 = 0;
	int _1 = 1;
	int _2 = 2;
	int _10 = 10;

	struct Thunk *zero = init_thunk_literal(&_0);
	struct Thunk *one = init_thunk_literal(&_1);
	struct Thunk *two = init_thunk_literal(&_2);
	struct Thunk *ten = init_thunk_literal(&_10);

	// (!=)
	struct Thunk *neq = init_thunk(int_nequal_eval, 2);
	// (*)
	struct Thunk *mult = init_thunk(int_mult_eval, 2);

	// (!=).0
	struct Thunk *neq0 = apply(neq, zero);
	// (!=).1
	struct Thunk *neq1 = apply(neq, one);
	// (*).2
	struct Thunk *mult2 = apply(mult, two);
	// (*).10
	struct Thunk *mult10 = apply(mult, ten);

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

	return 0;
}*/
