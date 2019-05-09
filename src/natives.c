#include <string.h>
#include "lib.h"
#include "mymap.h"
#include "thunk.h"
#include "natives.h"

int *add(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *x = x_thunk->value;
	void *y = y_thunk->value;
	// CODEGEN SPECIFIC: malloc result based on type
	
	int x_ = *(int *)x;
	int y_ = *(int *)y;
	int local = x_ + y_;

	int *result = malloc(sizeof(int));
	
	*result = local;
	return result;
}

void *add_eval(struct Thunk *t) {
	struct Thunk *x_ = ((t->args)[0]);
	struct Thunk *y_ = ((t->args)[1]);

	int * result = add(x_,y_);
	
	return result;
}

int *mult(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = x_thunk->value;
	void *data2 = y_thunk->value;
	
	int x_ = *(int *)data1;
	int y_ = *(int *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ * y_;

	return result;
}

void *mult_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = mult(x,y);

	return result;
}

int *neq(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = x_thunk->value;
	void *data2 = y_thunk->value;	

	int x_ = *(int *)data1;
	int y_ = *(int *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ != y_;

	return result;
}

void *neq_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = neq(x,y);
	
	return result;
}

int *sub(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *x = x_thunk->value;
	void *y = y_thunk->value;
	// CODEGEN SPECIFIC: malloc result based on type
	
	int x_ = *(int *)x;
	int y_ = *(int *)y;
	int local = x_ - y_;

	int *result = malloc(sizeof(int));
	
	*result = local;
	return result;
}

void *sub_eval(struct Thunk *t) {
	struct Thunk *x_ = ((t->args)[0]);
	struct Thunk *y_ = ((t->args)[1]);

	int * result = sub(x_,y_);
	
	return result;
}

struct List *cons(struct Thunk *data_thunk, struct Thunk *list_thunk) {
	struct List *list = list_thunk->value;

	struct Node *newhead = malloc(sizeof(struct Node));
	newhead->data = data_thunk;
	
	struct List *newlist = malloc(sizeof(struct List));	
	memcpy(newlist, list, sizeof(struct List));
	
	newlist->head = newhead;
	newlist->last_eval = newhead;

	struct Node *curr = list->head;

	while(curr) {
		struct Node *newnode = malloc(sizeof(struct Node));
		newnode->next = NULL;
		newnode->data = curr->data;
		appendNode(newlist, newnode);
		curr = curr->next;
	}

	return newlist;	
}

void *cons_eval(struct Thunk *t) {
	struct Thunk *data_thunk = ((t->args)[0]);
	struct Thunk *list_thunk = ((t->args)[1]);

	struct List *result = cons(data_thunk, list_thunk);	
	return result;
}

struct List *cat(struct Thunk *lthunk1, struct Thunk *lthunk2) {
	struct List *l1 = lthunk1->value;
	struct List *l2 = lthunk2->value;	

	struct List *new = malloc(sizeof(struct List));
	memcpy(new,l2,sizeof(struct List));
	new->head = NULL;
	new->last_eval = NULL;

	struct Node *curr1 = l1->head;
	while (curr1) {
		struct Node *newnode = malloc(sizeof(struct Node));
		newnode->data = curr1->data;
		newnode->next = NULL;
		appendNode(new, newnode);
		curr1 = curr1->next;
	}

	struct Node *curr2 = l2->head;
	while (curr2) {
		struct Node *newnode = malloc(sizeof(struct Node));
		newnode->data = curr2->data;
		newnode->next = NULL;
		appendNode(new, newnode);
		curr2 = curr2->next;
	}

	return new;
}

void *cat_eval(struct Thunk *t) {
	struct Thunk *lthunk1 = ((t->args)[0]);
	struct Thunk *lthunk2 = ((t->args)[1]);

	struct List *result = cat(lthunk1, lthunk2);

	return result;
}

void *head(struct Thunk *lthunk) {
	struct List *list = lthunk->value;
	struct Thunk *data = (list->head)->data;	
	void *value = invoke(data);
	return value;
}

void *head_eval(struct Thunk *t) {
	struct Thunk *lthunk = ((t->args)[0]);
	void *result = head(lthunk);
	return result;
}

struct List *tail(struct Thunk *lthunk) {
	struct List *list = lthunk->value;
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

		struct Node *newnode = malloc(sizeof(struct Node));
		newnode->next = NULL;	
		newnode->data = curr->data;

		appendNode(newlist,newnode);

		curr = curr->next;
	}
	return newlist;
}

void *tail_eval(struct Thunk *t) {
	struct Thunk *lthunk = ((t->args)[0]);
	struct List *result = tail(lthunk);
	return result;
}

void *length_eval(struct Thunk *t) {
	struct Thunk *lthunk = ((t->args)[0]);
	int *result = length(lthunk);
	return result;
}

int *length(struct Thunk *lthunk) {
	struct List *list = lthunk->value;

	struct Node *curr = list->head;	
	int count = 0;
	while (curr) {
		count++;
		curr = curr->next;
	}
	int *result = malloc(sizeof(int));
	*result = count;
	return result;
}

void *addf_eval(struct Thunk *t) {
	struct Thunk *thunk1 = ((t->args)[0]);
	struct Thunk *thunk2 = ((t->args)[1]);
	float *result = addf(thunk1,thunk2);
	return result;
}

float *addf(struct Thunk *x_thunk, struct Thunk *y_thunk) {	
	void *x = x_thunk->value;
	void *y = y_thunk->value;
	// CODEGEN SPECIFIC: malloc result based on type
	
	float x_ = *(float *)x;
	float y_ = *(float *)y;
	float local = x_ + y_;

	float *result = malloc(sizeof(float));
	
	*result = local;
	return result;

}
