#include <stdio.h>
#include "lib.h"
#include "thunk.h"
#include "mymap.h"
#include <string.h>
#include "natives.h"

struct Thunk *makeInt(int x) {
	int *i = malloc(4);
	*i = x;
	return init_thunk_literal(i);
}

struct Thunk *makeBool(char x) {
	return makeChar(x);
}	

struct Thunk *makeChar(char x) {
	char *b = malloc(1);
	*b = x;
	return init_thunk_literal(b);
}

struct Thunk *makeFloat(float x) {
	float *f = malloc(8);
	*f = x;
	return init_thunk_literal(f);
}

struct Tuple *makeTuple(void *data1, void *data2, int t1, int t2) {
	struct Tuple *newtup = malloc(sizeof(struct Tuple));

	newtup->t1 = t1;
	newtup->t2 = t2;

	struct Thunk *thunk_data1 = init_thunk_literal(data1);
	struct Thunk *thunk_data2 = init_thunk_literal(data2);

	newtup->first = thunk_data1;
	newtup->second = thunk_data2;

	return newtup;
}

struct Maybe *makeMaybe(void *data, int ty) {
	struct Maybe *may = malloc(sizeof(struct Maybe));
	if (data) {
		may->is_none = 0;
	} else {
		may->is_none = 1;
	}

	struct Thunk *data_thunk = init_thunk_literal(data);
	may->data = data_thunk;
	return may;
}

struct Thunk *makeEmptyList(int ty) {
	struct List *new = malloc(sizeof(struct List));	
	memset(new,0,sizeof(struct List));

	new->start = 0;
	new->end = 0;
	new->curr_index = -1;
	new->last_eval = NULL;
	new->type = LITLIST;
	new->content_type = ty;

	
	return init_thunk_literal(new);
}

struct Thunk *makeRangeList(int start, int end) {
	struct List *list = malloc(sizeof(struct List));
	list->start = start;
	list->end = end;
	list->content_type = INT;
	list->type = RANGE;
	list->curr_index = start;

	struct Thunk *data = makeInt(start);
	list->head = makeNode(data);
	list->last_eval = list->head;

	explodeRangeList(list);

	return init_thunk_literal(list);
}

struct Thunk *makeInfinite(int start) {
	struct List *list = malloc(sizeof(struct List));

	list->content_type = INT;
	list->type = INFINITE;
	list->start = start;
	list->end = -1;
	list->last_eval = 0;
	list->curr_index = start;

	struct Thunk *data = makeInt(start);
	list->head = makeNode(data);	
	
	list->last_eval = list->head;
	return init_thunk_literal(list);
}

struct Node *makeNode(struct Thunk *data_thunk) {
	struct Node *new = malloc(sizeof(struct Node));

	new->data = data_thunk;
	new->next = NULL;
	return new;
}


void explodeRangeList(void *list) {
	struct List *llist = (struct List *)list;
	
	while (llist->curr_index < llist->end) {
		evalNextNode(list);
	}
}

void evalNextNode(void *list) {
	struct List *llist = (struct List *)list;
	
	if (llist->type == RANGE || llist->type == INFINITE) {
		llist->curr_index++;
		struct Thunk *data = makeInt(llist->curr_index);
		struct Node *newNode = makeNode(data);
		llist = appendNode(llist, newNode);
	}
}

struct List *appendNode(struct List *list, struct Node *node) {
	if (!(list->head)) {
		list->head = node;
		list->last_eval = node;
	} else {
		(list->last_eval)->next = node;
		list->last_eval = node;
	}
	return list;
}

struct Thunk *appendNodeThunk(struct Thunk *list, struct Node *node) {
	return init_thunk_literal(appendNode(invoke(list),node));
}

void printAny(void *thing, int ty) {
	if (ty <= FLOAT) {
		printPrim(thing, ty);
	} else if (ty == LIST) {
		printList(thing);
	} else if (ty == TUPLE) {
		printTuple(thing);
	} else if (ty == MAYBE) {
		printMaybe(thing);
	}
}

void printList(void *list_thunk) {
	// TODO lol does this work 
/*	struct List *list = invoke(list_thunk);

	int type = llist->type;	
	struct Node *curr = llist->head;		
	int content_type = llist->content_type;

	if (type == RANGE) {
		printRangeList(list);
	} else if (type == INFINITE) {
		printInfinteList(list);
	} else if (type == COMP) {
		//TODO
	} else  {
		printPrimList(list);
	}*/
}

void printPrimList(struct Thunk *list_thunk) {
	struct List *list = invoke(list_thunk);
	
	int ty = list->content_type;
	struct Node *curr = list->head;

	if (ty!= CHAR)
		printf("[");
	while (curr) {
		invoke(curr->data);
		printAny((curr->data)->value, ty);	
		curr = curr->next;
		if (curr && ty != CHAR) {
			printf(", ");
		}
	}	
	if (ty != 2)
		printf("]");
}

void printInfinteList(void *list) {
	struct List *llist = (struct List*) list;
	
	int ty = llist->content_type;
	struct Node *head = llist->head;

	printf("[");
	printAny((head->data)->value, ty);
	printf("...]");
}

void printRangeList(void *list) {
	struct List *llist = (struct List*) list;
	
	int ty = llist->content_type;
	struct Node *head = llist->head;	

	if (llist->curr_index == llist->end) {
		printPrimList(list);
	} else {
		explodeRangeList(list);
		printPrimList(list);
	}
}

void printCompList(void *list);

void printTuple(void *tup) {
	struct Tuple *tupl = (struct Tuple*)tup;
	int t1 = tupl->t1;
	int t2 = tupl->t2;

	printf("(");
	printAny(((tupl->first))->value, t1);		
	printf(", ");
	printAny((tupl->second)->value, t2);
	printf(")");	
}

void printMaybe(void *may) {
	struct Maybe* mayb = (struct Maybe*)may;
	int ty = mayb->ty;
	if (mayb->is_none) {
		printf("None");
	} else {
		printf("Some ");
		printAny((mayb->data)->value, ty);
	}
}

void printPrim(void *data, int ty) {
	if (ty == INT) {
		int int_data = *(int *)data;
		printf("%d",int_data);
	} else if (ty == BOOL) {
		int bool_data = *(int *)data;
		if (bool_data) {
			printf("true");
		} else {
			printf("false");
		}
	} else if (ty == FLOAT) {
		float float_data = *(float *)data;
		printf("%f",float_data);
	} else if (ty == CHAR) {
		char char_data = *(char *)data;
		printf("%c",char_data);
	}
}

void printBool(char b) {
    printf("%s", b != 0 ? "true" : "false");
}

/*
int main() {
	struct List *front = makeRangeList(1,5);
	explodeRangeList(front);
	printf("front:\t");
	printPrimList(front);
	printf("\n");

	struct List *end = makeRangeList(6,10);
	explodeRangeList(end);
	printf("end:\t");
	printPrimList(end);
	printf("\n");

	printf("front length: %d\n", length(front));
	printf("end length: %d\n", length(end));

	printf("front head: %d\n", *(int *)head(front));
	printf("end head: %d\n", *(int *)head(end));

	struct List *front_tail = tail(front);
	struct List *end_tail = tail(end);
	
	printf("front_tail: ");
	printPrimList(front_tail);
	printf("\n");

	printf("end_tail: ");
	printPrimList(end_tail);
	printf("\n");

	int _100 = 100;

	struct List *front_cons100 = cons(&_100, front);
	struct List *end_cons100 = cons(&_100, end);

	printf("front cons 100: ");
	printPrimList(front_cons100);
	printf("\n");

	printf("end cons 100: ");
	printPrimList(end_cons100);
	printf("\n");

	struct List *cat_front_end = cat(front, end);
	printf("cat front end: ");
	printPrimList(cat_front_end);
	printf("\n");

	struct List *cat_fronttail_endtail = cat(front_tail, end_tail);
	printf("cat front_tail end_tail: ");
	printPrimList(cat_fronttail_endtail);
	printf("\n");

	int _2 = 2;
	struct Thunk *two = init_thunk_literal(&_2);
	struct Thunk mul[1];
	init_thunk(mul,mult_eval,2);
	struct Thunk *mult2 = apply(mul, two);

	struct List *mult2_cat_fronttail_endtail = map(cat_fronttail_endtail,mult2);
	printf("map mult2 cat fronttail endtail: ");
	printPrimList(mult2_cat_fronttail_endtail);
	printf("\n");
	
	printf("front:\t");
	printPrimList(front);
	printf("\n");

	printf("end:\t");
	printPrimList(end);
	printf("\n");

	printf("front_tail: ");
	printPrimList(front_tail);
	printf("\n");

	printf("end_tail: ");
	printPrimList(end_tail);
	printf("\n");

	printf("cat front end: ");
	printPrimList(cat_front_end);
	printf("\n");

	printf("cat front_tail end_tail: ");
	printPrimList(cat_fronttail_endtail);
	printf("\n");

	return 0;
}*/
