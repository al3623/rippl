#include <string.h>
#include "thunk.h"
#include "lib.h"

struct Thunk *init_thunk(struct Thunk *(*f)(struct Thunk *,void *), 
//	int[] types,
	int num_args) {
	
	struct Thunk *thunk = malloc(sizeof(struct Thunk));

	thunk->f = f;
	thunk->num_args = num_args;
	thunk->filled_args = 0;
	thunk->args = malloc(num_args * sizeof(void *));
//	thunk->types = malloc(num_args *sizeof(int));
//	memcpy(thunk->types, types, sizeof(int)*num_args);
	thunk->value = NULL;

	return thunk;
}

void *add(int x, int y) {
	// CODEGEN SPECIFIC: malloc result based on type
	int *result = malloc(sizeof(int));

	int local = x + y;
	
	*result = local;
	return result;
}

struct Thunk *add_thunk(struct Thunk *thunk, void *arg) {
	struct Thunk *new_thunk = malloc(sizeof(struct Thunk));
	memcpy(new_thunk, thunk, sizeof(struct Thunk));
	new_thunk->args = malloc(new_thunk->num_args * sizeof(void *));
	memcpy(new_thunk->args, thunk->args, new_thunk->num_args * sizeof(void *));

	if (new_thunk->filled_args == new_thunk->num_args -1) {
		// CODEGEN SPECIFIC: push locals to stack
		// CODEGEN SPECIFIC: locals types based on thunk types or inference?
		int x1 = *(int *) ((new_thunk->args)[0]);
		int y1 = *(int *) arg;

		void *res = add(x1,y1);
		fprintf(stderr, "%d + %d = %d\n", x1,y1,*(int *)res);

		new_thunk->value = res;
	} else if (new_thunk->filled_args < new_thunk->num_args - 1) {
		(new_thunk->args)[new_thunk->filled_args] = arg;
		fprintf(stderr, "+ %d\n", 
			(*(int *)(new_thunk->args)[new_thunk->filled_args]));
		new_thunk->filled_args++;
	} else {
		fprintf(stderr, "not a thunk, can't be applied");
		exit(1);
	}
	return new_thunk;
}

struct Thunk *invoke(struct Thunk *t, void *arg) {
	struct Thunk* (*f) (struct Thunk *thunk, void *a) = t->f;
	return f(t,arg);	
}

/*
int main() {
	int types[] = {0, 0};

	struct Thunk *orig_thunk = init_thunk(add_thunk, 2);
	struct Thunk *orig2 = init_thunk(add_thunk, 2);
	
	int five = 5;
	int zero = 0;
	int two = 2;

	struct Thunk *add5 = invoke(orig_thunk, &five);	
	fprintf(stderr, "in add5: %d\n", *(int *)((add5->args)[0]));
	struct Thunk *add2 = invoke(orig_thunk, &two);
	fprintf(stderr, "in add5: %d\n", *(int *)((add5->args)[0]));
	
	struct Thunk *add50 = invoke(add5, &zero);
	struct Thunk *add52 = invoke(add5, &two);
	struct Thunk *add20 = invoke(add2, &zero);

	int seven_ = *(int *)(add52->value);
	int five_ = *(int *)(add50->value);
	int two_ = *(int *)(add20->value);

	printf("%d\n",seven_);
	printf("%d\n",five_);
	printf("%d\n",two_);

	return 0;
} */
