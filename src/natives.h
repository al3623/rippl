#include "lib.h"
#include "mymap.h"
#include "thunk.h"

int *add(struct Thunk *x_thunk, struct Thunk *y_thunk);
int *add_eval(struct Thunk *t);
struct Thunk add_init_thunk[1];

int *mult(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *mult_eval(struct Thunk *t);
struct Thunk mult_init_thunk[1];

int *neq(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *neq_eval(struct Thunk *t);
struct Thunk neq_init_thunk[1];


int *sub(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *sub_eval(struct Thunk *t);
struct Thunk sub_init_thunk[1];


/* TODO: put in natives.ml and remove from lib.ml */
struct List *cons(struct Thunk *data_thunk, struct Thunk *list_thunk);
void *cons_eval(struct Thunk *);
struct Thunk cons_init_thunk[1];

struct List *cat(struct Thunk *lthunk1, struct Thunk *lthunk2); 
struct List *cat_eval(struct Thunk *t);
struct Thunk cat_init_thunk[1];

void *head(struct Thunk *lthunk);
void *head_eval(struct Thunk *t);
struct Thunk head_init_thunk[1];

struct List *tail(struct Thunk *lthunk);
void *tail_eval(struct Thunk *lthunk);
struct Thunk tail_init_thunk[1];

int *length(struct Thunk *lthunk);
void *length_eval(struct Thunk *t);
struct Thunk init_thunk_length[1];


void *addf_eval(struct Thunk *);
float *addf(struct Thunk *, struct Thunk *);
struct Thunk init_thunk_addf[1];
