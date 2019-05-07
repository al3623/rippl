#include "lib.h"
#include "mymap.h"
#include "thunk.h"

int *add(struct Thunk *x_thunk, struct Thunk *y_thunk);
int *add_eval(struct Thunk *t);

int *mult(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *mult_eval(struct Thunk *t);

int *neq(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *neq_eval(struct Thunk *t);

int *sub(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *sub_eval(struct Thunk *t);

/* TODO: put in natives.ml and remove from lib.ml */
struct List *cons(struct Thunk *data_thunk, struct Thunk *list_thunk);
void *cons_eval(struct Thunk *);

struct List *cat(struct Thunk *lthunk1, struct Thunk *lthunk2); 
struct List *cat_eval(struct Thunk *t);

void *head(struct Thunk *lthunk);
void *head_eval(struct Thunk *t);

struct List *tail(struct Thunk *lthunk);
void *tail_eval(struct Thunk *lthunk);

int *length(struct Thunk *lthunk);
void *length_eval(struct Thunk *t);
