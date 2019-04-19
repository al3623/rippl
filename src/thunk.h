#ifndef THUNK
#define THUNK

struct Thunk {
	struct Thunk *(* f)(struct Thunk *, void *);
	int num_args;
	int filled_args;
	void **args;
	void *value;
};

struct Thunk *init_thunk(struct Thunk *( *f)(struct Thunk *, void *), 
	int num_args);
struct Thunk *apply(struct Thunk *t, void *arg);
void *invoke(struct Thunk *t);
#endif
