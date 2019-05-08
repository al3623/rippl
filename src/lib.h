#ifndef LIB
#define LIB

#include <stdio.h>
#include <stdlib.h>

#define	INT			0
#define	BOOL		1
#define	CHAR		2
#define	FLOAT		3
#define	LIST		4
#define	TUPLE		5
#define MAYBE		6

#define RANGE		0
#define INFINITE	1
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
	
	/* COMPREHENSION STUFF */
	void *(*expr)(void *,...);	// expression for each element
	struct List *listvbinds;	// lists the comprehension is over
	struct Node *indexes;		// curr index in each sublist for vbindings
	int (**filt)(void *,...);	// boolean filters for vbind values to accept
	int num_vbinds;
};

void printBool(char b);
void printPrim(void *data, int ty);
void printAny(void *thing, int ty);
void printList(void *list);
void printTuple(void *tup);
void printMaybe(void *may);
void printPrimList(struct Thunk *list);
void printRangeList(void *list);
void printInfinteList(void *list);
void printCompList(void *list);

struct Thunk *makeInt(int x);
struct Thunk *makeBool(char x);
struct Thunk *makeChar(char x);
struct Thunk *makeFloat(float x);
struct Node *makeNode(struct Thunk *data);
struct Thunk *makeEmptyList(int ty);
struct Thunk *makeInfinite(int start);
struct Thunk *makeRangeList(int start, int end);
struct Tuple *makeTuple(void *data1, void *data2, int t1, int t2);
struct Maybe *makeMaybe(void *data, int ty);

void explodeRangeList(void *list);
void evalNextNode(void *list);
struct Thunk *appendNode(struct Thunk *list, struct Node *node);
struct Node *evalNextNodeComp(void *list, int num_vbinds);

#endif
