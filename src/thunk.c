#include <string.h>
#include "thunk.h"
#include "lib.h"
#include "natives.h"

struct Thunk *init_thunk(struct Thunk *thunk,
	void *(*eval)(struct Thunk  *), int num_args) {

	thunk->eval = eval;
	thunk->num_args = num_args;
	thunk->filled_args = 0;
	thunk->args = malloc(num_args * sizeof(struct Thunk*));
	thunk->value = NULL;

	return thunk;
}

struct Thunk *init_thunk_literal(void *data) {
	struct Thunk *lit = malloc(sizeof(struct Thunk));
	lit = init_thunk(lit,NULL, 1);

	lit->filled_args = 1;
	lit->value = data;
	(lit->args)[0] = lit;
	return lit;
}

struct Thunk *apply(struct Thunk *thunk, struct Thunk *arg) {
	struct Thunk *new_thunk = malloc(sizeof(struct Thunk));
	memcpy(new_thunk, thunk, sizeof(struct Thunk));
	new_thunk->args = malloc(new_thunk->num_args * sizeof(struct Thunk*));
	memcpy(new_thunk->args, thunk->args, 
		new_thunk->num_args * sizeof(struct Thunk *));

	if (thunk->value == thunk) {
		new_thunk->value = new_thunk;
	}

	if (new_thunk->filled_args < new_thunk->num_args) {	
		(new_thunk->args)[new_thunk->filled_args] = arg;
		new_thunk->filled_args++;
	} else {

		// Top thunk is filled, let's fill in the last arg
		struct Thunk *last_arg = new_thunk->args[new_thunk->num_args - 1];
		if (last_arg->filled_args < last_arg->num_args) {
			(last_arg->args)[last_arg->filled_args] = arg;
			last_arg->filled_args++;
		} else {
			fprintf(stderr, "fully applied");
			exit(1);
		}
	}
	return new_thunk;

}


void *invoke(struct Thunk *t) {
	if (t->filled_args != t->num_args) {
		return (t->value = t);
	} else {
		if (t->value && (t->value != t)) {
			return t->value;
		} else {
			int i;
			for (i = 0; i < t->num_args; i++) {
		                invoke(t->args[i]);
			}

			t->value = t->eval(t);
			return t->value;
		}
	}
}
/*
int main() {
	struct Thunk *orig_thunk = init_thunk(add_eval,2);
	
	int _5 = 5;
	int _0 = 0;
	int _2 = 2;
		
	struct Thunk *five = init_thunk_literal(&_5);
	struct Thunk *zero = init_thunk_literal(&_0);
	struct Thunk *two = init_thunk_literal(&_2);

	struct Thunk *add5 = apply(orig_thunk, five);	
	fprintf(stderr, "in add5: %d\n", *(int *)(((add5->args)[0])->value));
	struct Thunk *add2 = apply(orig_thunk, two);
	fprintf(stderr, "in add2: %d\n", *(int *)(((add2->args)[0])->value));
	
	struct Thunk *add50 = apply(add5, zero);	
	fprintf(stderr, "in add50: %d\n", *(int *)(((add50->args)[1])->value));
	struct Thunk *add52 = apply(add5, two);
	fprintf(stderr, "in add52: %d\n", *(int *)(((add52->args)[1])->value));
	struct Thunk *add20 = apply(add2, zero);
	fprintf(stderr, "in add20: %d\n", *(int *)(((add20->args)[1])->value));
	
	int seven_ = *(int *)invoke(add52);
	int five_ = *(int *)invoke(add50);
	int two_ = *(int *)invoke(add20);

	printf("%d\n",seven_);
	printf("%d\n",five_);
	printf("%d\n",two_);

	return 0;
}*/
