#ifndef THUNK
#define THUNK

struct Thunk {
	struct Thunk *(* f)(struct Thunk *, struct Thunk *);
	void *(* eval)(struct Thunk *);
	int num_args;
	int filled_args;
	struct Thunk **args;
	void *value;
};

struct Thunk *init_thunk_literal(void *data);
struct Thunk *init_thunk(struct Thunk *( *f)(struct Thunk *, struct Thunk*),
	void *(*eval)(struct Thunk *), 
	int num_args);
struct Thunk *apply(struct Thunk *t, struct Thunk *arg);
void *invoke(struct Thunk *t);
#endif
