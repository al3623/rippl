#include "lib.h"
#include "mymap.h"
#include "thunk.h"

void *add(struct Thunk *x_thunk, struct Thunk *y_thunk) {
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

int *int_mult(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = x_thunk->value;
	void *data2 = y_thunk->value;
	
	int x_ = *(int *)data1;
	int y_ = *(int *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ * y_;

	return result;
}

void *int_mult_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = int_mult(x,y);

	return result;
}

int *int_nequal(struct Thunk *x_thunk, struct Thunk *y_thunk) {
	void *data1 = x_thunk->value;
	void *data2 = y_thunk->value;	

	int x_ = *(int *)data1;
	int y_ = *(int *)data2;

	int *result = malloc(sizeof(int));
	*result = x_ != y_;

	return result;
}

void *int_nequal_eval(struct Thunk *t) {
	struct Thunk *x = ((t->args)[0]);
	struct Thunk *y = ((t->args)[1]);

	void *result = int_nequal(x,y);
	
	return result;
}

void *sub(struct Thunk *x_thunk, struct Thunk *y_thunk) {
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


