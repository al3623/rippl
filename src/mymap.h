#ifndef MAP
#define MAP
 
struct Thunk *mapl(struct Thunk *list, struct Thunk *func, int ty);

struct Thunk *filterl(struct Thunk *list, struct Thunk *filter,int ty);

struct Thunk *map_listl(struct Thunk *apps, struct Thunk *vals, int ty);
#endif
