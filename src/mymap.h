#ifndef MAP
#define MAP
 
struct List *map(struct List *list, struct Thunk *func);

struct List *filter(struct List *list, struct Thunk *filter);

struct List *map_list(struct List *apps, struct List *vals);

#endif
