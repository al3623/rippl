#include "lib.h"
#include "mymap.h"
#include "thunk.h"


// Integer operations
int *add(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *add_eval(struct Thunk *t);
extern struct Thunk add_init_thunk[1];

int *sub(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *sub_eval(struct Thunk *t);
extern struct Thunk sub_init_thunk[1];

int *mult(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *mult_eval(struct Thunk *t);
extern struct Thunk mult_init_thunk[1];

int *divi(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *divi_eval(struct Thunk *t);
extern struct Thunk divi_init_thunk[1];

int *mod(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *mod_eval(struct Thunk *t);
extern struct Thunk mod_init_thunk[1];

int *powe(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *powe_eval(struct Thunk *t);
extern struct Thunk powe_init_thunk[1];

int *eq(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *eq_eval(struct Thunk *t);
extern struct Thunk eq_init_thunk[1];

int *neq(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *neq_eval(struct Thunk *t);
extern struct Thunk neq_init_thunk[1];

int *geq(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *geq_eval(struct Thunk *t);
extern struct Thunk geq_init_thunk[1];

int *leq(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *leq_eval(struct Thunk *t);
extern struct Thunk leq_init_thunk[1];

int *less(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *less_eval(struct Thunk *t);
extern struct Thunk less_init_thunk[1];

int *greater(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *greater_eval(struct Thunk *t);
extern struct Thunk greater_init_thunk[1];

int *neg(struct Thunk *x_thunk);
void *neg_eval(struct Thunk *t);
extern struct Thunk neg_init_thunk[1];


// Float operations
float *addf(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *addf_eval(struct Thunk *t);
extern struct Thunk addf_init_thunk[1];

float *subf(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *subf_eval(struct Thunk *t);
extern struct Thunk subf_init_thunk[1];

float *multf(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *multf_eval(struct Thunk *t);
extern struct Thunk multf_init_thunk[1];

float *divf(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *divf_eval(struct Thunk *t);
extern struct Thunk divf_init_thunk[1];

float *powef(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *powef_eval(struct Thunk *t);
extern struct Thunk powf_init_thunk[1];

int *eqf(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *eqf_eval(struct Thunk *t);
extern struct Thunk eqf_init_thunk[1];

int *neqf(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *neqf_eval(struct Thunk *t);
extern struct Thunk neqf_init_thunk[1];

int *geqf(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *geqf_eval(struct Thunk *t);
extern struct Thunk geqf_init_thunk[1];

int *leqf(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *leqf_eval(struct Thunk *t);
extern struct Thunk leqf_init_thunk[1];

int *lessf(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *lessf_eval(struct Thunk *t);
extern struct Thunk lessf_init_thunk[1];

int *greaterf(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *greaterf_eval(struct Thunk *t);
extern struct Thunk greaterf_init_thunk[1];

float *negf(struct Thunk *x_thunk);
void *negf_eval(struct Thunk *t);
extern struct Thunk negf_init_thunk[1];

// Boolean operations
int *andb(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *andb_eval(struct Thunk *t);
extern struct Thunk andb_init_thunk[1];

int *orb(struct Thunk *x_thunk, struct Thunk *y_thunk);
void *orb_eval(struct Thunk *t);
extern struct Thunk orb_init_thunk[1];

int *notb(struct Thunk *x_thunk);
void *notb_eval(struct Thunk *t);
extern struct Thunk notb_init_thunk[1];


// List operations
struct List *cons(struct Thunk *data_thunk, struct Thunk *list_thunk);
void *cons_eval(struct Thunk *);
extern struct Thunk cons_init_thunk[1];

struct List *cat(struct Thunk *lthunk1, struct Thunk *lthunk2); 
void *cat_eval(struct Thunk *t);
extern struct Thunk cat_init_thunk[1];

int *length(struct Thunk *lthunk);
void *length_eval(struct Thunk *t);
extern struct Thunk length_init_thunk[1];

void *head(struct Thunk *lthunk);
void *head_eval(struct Thunk *t);
extern struct Thunk head_init_thunk[1];

struct List *tail(struct Thunk *lthunk);
void *tail_eval(struct Thunk *lthunk);
extern struct Thunk tail_init_thunk[1];

// Tuple operations
void *first(struct Thunk *lthunk);
void *first_eval(struct Thunk *t);
extern struct Thunk first_init_thunk[1];

void *second(struct Thunk *lthunk);
void *second_eval(struct Thunk *t);
extern struct Thunk second_init_thunk[1];

void *int_to_float(struct Thunk *t);
void *int_to_float_eval(struct Thunk *t);
extern struct Thunk int_to_float_init_thunk[1];
