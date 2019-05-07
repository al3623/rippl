#include "lib.h"
#include "mymap.h"
#include "thunk.h"

int *add(struct Thunk *x_thunk, struct Thunk *y_thunk);
int *add_eval(struct Thunk *t);

int *mult(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *mult_eval(struct Thunk *t);

int *neq(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *neq_eval(struct Thunk *t);

