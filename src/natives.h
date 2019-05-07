#include "lib.h"
#include "mymap.h"
#include "thunk.h"

int *add(struct Thunk *x_thunk, struct Thunk *y_thunk);
int *add_eval(struct Thunk *t);

int *int_mult(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *int_mult_eval(struct Thunk *t);

int *int_nequal(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *int_nequal_eval(struct Thunk *t);

