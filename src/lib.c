#include <stdio.h>
#include "lib.h"
#include "thunk.h"
#include <string.h>

int *makeInt(int x) {
	int *i = malloc(4);
	*i = x;
	return i;
}

void *makeBool(char x) {
	return makeChar(x);
}	

void *makeChar(char x) {
	char *b = malloc(1);
	*b = x;
	return b;
}

float *makeFloat(float x) {
	float *f = malloc(8);
	*f = x;
	return f;
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

struct List *makeEmptyList(int ty) {
	struct List *new = malloc(sizeof(struct List));	

	new->start = 0;
	new->end = 0;
	new->curr_index = -1;
	new->last_eval = NULL;
	new->type = LITLIST;
	new->content_type = ty;

	return new;
}

struct List *makeRangeList(int start, int end) {
	struct List *list = malloc(sizeof(struct List));
	list->start = start;
	list->end = end;
	list->content_type = INT;
	list->type = RANGE;
	list->curr_index = start;

	int *data = makeInt(start);
	list->head = makeNode(data);
	list->last_eval = list->head;

	return list;
}

struct List *makeInfinite(int start) {
	struct List *list = malloc(sizeof(struct List));

	list->content_type = INT;
	list->type = INFINITE;
	list->start = start;
	list->end = -1;
	list->last_eval = 0;
	list->curr_index = start;

	int *data = makeInt(start);
	list->head = makeNode(data);	
	
	list->last_eval = list->head;
	return list;
}

struct Node *makeNode(void *data) {
	struct Node *new = malloc(sizeof(struct Node));

	struct Thunk *thunk_data = init_thunk_literal(data);

	new->data = thunk_data;
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
		int *data = makeInt(llist->curr_index);
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

void printList(void *list) {
	struct List *llist = (struct List*) list;

	int type = llist->type;	
	struct Node *curr = llist->head;		
	int content_type = llist->content_type;

	if (type == RANGE) {
		printRangeList(list);
	} else if (type == INFINITE) {
		printInfinteList(list);
	} else if (type == COMP) {
		//TODO
	} else /* LISTLIT */ {
		printPrimList(list);
	}
}

void printPrimList(void *list) {
	struct List *llist = (struct List*) list;
	
	int ty = llist->content_type;
	struct Node *curr = llist->head;

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

struct List *cons(void *data, struct List *list) {
	struct Node *newnode = makeNode(data);
	
	struct List *newlist = malloc(sizeof(struct List));	
	memcpy(newlist, list, sizeof(struct List));
	
	// TODO: deep copy of rest of list??????????

	return newlist;	
}

struct List *cat(struct List *l1, struct List *l2) {
	return NULL;
}

void *head(struct List *list) {
	struct Thunk *data = (list->head)->data;	
	void *value = invoke(data);
	return value;
}

struct List *tail(struct List *list) {
	struct List *newlist = malloc(sizeof(struct List));
	memcpy(newlist, list, sizeof(struct List));
	newlist->head = NULL;
	newlist->last_eval = NULL;
	
	struct Node *curr = list->head;
	if (!curr)
		return newlist;

	curr = curr->next;
	while (curr) {
		struct Thunk *data = curr->data;

		// TODO: check this deep copy	
		struct Node *newnode = malloc(sizeof(struct Node));
		newnode->next = NULL;	
		struct Thunk *newdata = malloc(sizeof(struct Thunk));
		memcpy(newdata,data,sizeof(struct Thunk));	
		newdata->args = malloc(newdata->num_args * sizeof(void *));
		memcpy(newdata->args, data->args, newdata->num_args * sizeof(void *));
		newnode->data = newdata;

		appendNode(newlist,newnode);

		curr = curr->next;
	}
	return newlist;
}

int length(struct List *list) {
	struct Node *curr = list->head;	
	int count = 0;
	while (curr) {
		count++;
		curr = curr->next;
	}
	return count;
}
