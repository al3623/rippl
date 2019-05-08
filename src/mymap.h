#ifndef MAP
#define MAP
 
struct Thunk *map(struct Thunk *list, struct Thunk *func);

struct Thunk *filter(struct Thunk *list, struct Thunk *filter);

struct Thunk *map_list(struct Thunk *apps, struct Thunk *vals);
#endif
