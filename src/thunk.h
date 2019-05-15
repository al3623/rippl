#ifndef THUNK
#define THUNK

struct Thunk {
	void *(* eval)(struct Thunk *);
	int num_args;
	int filled_args;
	struct Thunk **args;
	void *value;
	int is_ite;
};

struct Thunk *init_thunk_literal(void *data);
struct Thunk *init_thunk(struct Thunk *t,
	void *(*eval)(struct Thunk *), int num_args, int is_ite);
struct Thunk *apply(struct Thunk *t, struct Thunk *arg);
void *invoke(struct Thunk *t);
#endif
