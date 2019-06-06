#ifndef LIB
#define LIB

#include <stdio.h>
#include <stdlib.h>

#define	INT		0
#define	BOOL		1
#define	CHAR		2
#define	FLOAT		3
#define	LIST		4
#define	TUPLE		5
#define MAYBE		6

#define RANGE		0
#define COMP		2
#define LITLIST		3

struct Tuple {
	int t1;
	int t2;
	struct Thunk *first;
	struct Thunk *second;
};

struct Maybe {
	int ty;
	int is_none;
	struct Thunk *data;
};

struct Node {
	struct Thunk *data;
	struct Node *next;
};

struct List {
	struct Node *head;
	int content_type;
	int type;

	/* INDEXING ACCESS STUFF */
	struct Node *last_eval;
	int curr_index; 			// useful for laziness in ranges/infinites
	int start;					// useful for ranges/infintes
	int end;					// useful for ranges
	
};

void printBool(char b);
void printPrim(void *data, int ty);
void printAny(void *thing, int ty);
void printAnyThunk(struct Thunk *t, int *ty, int index);
void printList(struct List *list, int *ty, int index);
void printTuple(void *tup, int *ty, int index);
void printMaybe(void *may, int *ty, int index);
void printRangeList(struct Thunk *list);
void printCompList(void *list);
void printMaybe(void *mayb, int *ty, int index);

struct Thunk *makeInt(int x);
struct Thunk *makeBool(char x);
struct Thunk *makeChar(char x);
struct Thunk *makeFloat(float x);
struct Node *makeNode(struct Thunk *data);
struct Thunk *makeEmptyList(int ty);
struct Thunk *makeRangeList(struct Thunk *start, struct Thunk *end);
struct Thunk *makeTuple(struct Thunk *data1, struct Thunk *data2, int t1, int t2);
struct Thunk *makeMaybe(struct Thunk *data, int ty);

void explodeRangeList(void *list);
void evalNextNode(void *list);
struct List *appendNode(struct List *list, struct Node *node);
struct Thunk *appendNodeThunk(struct Thunk *list, struct Node *node);
struct Node *evalNextNodeComp(void *list, int num_vbinds);
void initNativeThunks();

struct Thunk *makeIte(struct Thunk *cond, struct Thunk *then_thunk, 
	struct Thunk *else_thunk);

#endif
