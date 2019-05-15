#include <string.h>
#include "lib.h"
#include "mymap.h"
#include "thunk.h"
#include "natives.h"
#include <math.h>

// Integer operations
int *add(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *x = invoke(x_thunk);
	void *y = invoke(y_thunk);
	
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

int *sub(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *x = invoke(x_thunk);
	void *y = invoke(y_thunk);
	
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

int *mult(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);
	
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

int *divi(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);
	
	int x_ = *(int *)data1;
	int y_ = *(int *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ / y_;

	return result;
}

void *divi_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = divi(x,y);

	return result;
}

int *mod(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);
	
	int x_ = *(int *)data1;
	int y_ = *(int *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ % y_;

	return result;
}

void *mod_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = mod(x,y);

	return result;
}

int *powe(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);
	
	int x_ = *(int *)data1;
	int y_ = *(int *)data2;

	int *result = malloc(sizeof(int));
	*result = (int) (pow((double) x_, (double) y_));

	return result;
}

void *powe_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = powe(x,y);

	return result;
}

int *eq(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);	

	int x_ = *(int *)data1;
	int y_ = *(int *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ == y_;

	return result;
}

void *eq_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = eq(x,y);
	
	return result;
}

int *neq(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);	

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

int *geq(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);	

	int x_ = *(int *)data1;
	int y_ = *(int *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ >= y_;

	return result;
}

void *geq_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = geq(x,y);
	
	return result;
}

int *leq(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);	

	int x_ = *(int *)data1;
	int y_ = *(int *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ <= y_;

	return result;
}

void *leq_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = leq(x,y);
	
	return result;
}

int *less(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);	

	int x_ = *(int *)data1;
	int y_ = *(int *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ < y_;

	return result;
}

void *less_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = less(x,y);
	
	return result;
}

int *greater(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);	

	int x_ = *(int *)data1;
	int y_ = *(int *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ > y_;

	return result;
}

void *greater_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = greater(x,y);
	
	return result;
}

int *neg(struct Thunk *x_thunk) {
	void *data1 = invoke(x_thunk);	

	int x_ = *(int *)data1;

	int *result = malloc(sizeof(int));
	*result = -x_;

	return result;
}

void *neg_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);

	void *result = neg(x);
	
	return result;
}


// Float operations
float *addf(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *x = invoke(x_thunk);
	void *y = invoke(y_thunk);
	
	float x_ = *(float *)x;
	float y_ = *(float *)y;
	float local = x_ + y_;

	float *result = malloc(sizeof(float));
	
	*result = local;
	return result;
}

void *addf_eval(struct Thunk *t) {
	struct Thunk *x_ = ((t->args)[0]);
	struct Thunk *y_ = ((t->args)[1]);

	void *result = addf(x_,y_);
	
	return result;
}

float *subf(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *x = invoke(x_thunk);
	void *y = invoke(y_thunk);
	
	float x_ = *(float *)x;
	float y_ = *(float *)y;
	float local = x_ - y_;

	float *result = malloc(sizeof(float));
	
	*result = local;
	return result;
}

void *subf_eval(struct Thunk *t) {
	struct Thunk *x_ = ((t->args)[0]);
	struct Thunk *y_ = ((t->args)[1]);

	void *result = subf(x_,y_);
	
	return result;
}

float *multf(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);

	float x_ = *(float *)data1;
	float y_ = *(float *)data2;

	float *result = malloc(sizeof(float));
	*result = x_ * y_;

	return result;
}

void *multf_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = multf(x,y);

	return result;
}

float *divf(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);
	
	float x_ = *(float *)data1;
	float y_ = *(float *)data2;

	float *result = malloc(sizeof(float));
	*result = x_ / y_;

	return result;
}

void *divf_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = divf(x,y);

	return result;
}

float *powef(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);
	
	float x_ = *(float *)data1;
	float y_ = *(float *)data2;

	float *result = malloc(sizeof(float));

	*result = (float) (pow((double) x_, (double) y_));

	return result;
}

void *powef_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = powef(x,y);

	return result;
}

int *eqf(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);	

	float x_ = *(float *)data1;
	float y_ = *(float *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ == y_;

	return result;
}

void *eqf_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = eqf(x,y);
	
	return result;
}

int *neqf(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);	

	float x_ = *(float *)data1;
	float y_ = *(float *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ != y_;

	return result;
}

void *neqf_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = neqf(x,y);
	
	return result;
}

int *geqf(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);	

	float x_ = *(float *)data1;
	float y_ = *(float *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ >= y_;

	return result;
}

void *geqf_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = geqf(x,y);
	
	return result;
}

int *leqf(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);	

	float x_ = *(float *)data1;
	float y_ = *(float *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ <= y_;

	return result;
}

void *leqf_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = leqf(x,y);
	
	return result;
}

int *lessf(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);	

	float x_ = *(float *)data1;
	float y_ = *(float *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ < y_;

	return result;
}

void *lessf_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = lessf(x,y);
	
	return result;
}

int *greaterf(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);	

	float x_ = *(float *)data1;
	float y_ = *(float *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ > y_;

	return result;
}

void *greaterf_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = greaterf(x,y);
	
	return result;
}

float *negf(struct Thunk *x_thunk) {
	void *data1 = invoke(x_thunk);	

	float x_ = *(float *)data1;

	float *result = malloc(sizeof(float));
	*result = -x_;

	return result;
}

void *negf_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);

	void *result = negf(x);
	
	return result;
}

// Boolean operations
int *andb(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = invoke(x_thunk);
	void *data2 = invoke(y_thunk);		

	int x_ = *(int *)data1;
	int y_ = *(int *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ && y_;

	return result;
}

void *andb_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = andb(x,y);
	
	return result;
}

int *orb(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = x_thunk->value;
	void *data2 = y_thunk->value;	

	int x_ = *(int *)data1;
	int y_ = *(int *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ || y_;

	return result;
}

void *orb_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = orb(x,y);
	
	return result;
}

int *notb(struct Thunk *x_thunk) {
	void *data1 = invoke(x_thunk);

	int x_ = *(int *)data1;

	int *result = malloc(sizeof(int));
	*result = !x_;

	return result;
}

void *notb_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);

	void *result = notb(x);
	
	return result;
}


// List operations
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
	struct List *l1 = invoke(lthunk1);
	struct List *l2 = invoke(lthunk2);	

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

int *length(struct Thunk *lthunk) {
	struct List *list = invoke(lthunk);

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

void *length_eval(struct Thunk *t) {
	struct Thunk *lthunk = ((t->args)[0]);
	int *result = length(lthunk);
	return result;
}

void *head(struct Thunk *lthunk) {
	struct List *list = invoke(lthunk);
	
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
	struct List *list = invoke(lthunk);
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


// Tuple operations
void *first(struct Thunk *lthunk) {
	struct Tuple *tuple = invoke(lthunk);
	struct Thunk *data = tuple->first;	
	void *value = invoke(data);
	return value;
}

void *first_eval(struct Thunk *t) {
	struct Thunk *lthunk = ((t->args)[0]);
	void *result = first(lthunk);
	return result;
}

void *second(struct Thunk *lthunk) {
	struct Tuple *tuple = invoke(lthunk);
	struct Thunk *data = tuple->second;	
	void *value = invoke(data);
	return value;
}

void *second_eval(struct Thunk *t) {
	struct Thunk *lthunk = ((t->args)[0]);
	void *result = second(lthunk);
	return result;
}

void *isNone(struct Thunk *the_thunk){
    	struct Maybe *mb = invoke(the_thunk);
 	int is_none = mb -> is_none;
	char *is_none_char = malloc(sizeof(char));
	*is_none_char = (char)is_none;
	return is_none_char;
}

void *fromJust(struct Thunk *the_thunk){
	struct Maybe *mb = invoke(the_thunk);
	struct Thunk *data = mb -> data;
	void *value = invoke(data);
	return value;
}

void *from_just_eval(struct Thunk *t){
	struct Thunk *argy = ((t -> args)[0]);
	void *result = fromJust(argy);
	return result;
}


void *is_none_eval(struct Thunk *t){
	struct Thunk *argy = ((t->args)[0]);
	void *result = isNone(argy);
	return result;
}


void *int_to_float(struct Thunk *i) {
	int *val = invoke(i);
	float *result = malloc(sizeof(float));
	*result = *(int *)val;
	return result;
}

void *int_to_float_eval(struct Thunk *t) {
	struct Thunk *th = ((t->args)[0]);
	void *result = int_to_float(th);
	return result;
}
