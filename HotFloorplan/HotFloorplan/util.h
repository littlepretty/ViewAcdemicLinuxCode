#ifndef __UTIL_H
#define __UTIL_H

#define MAX(x,y)		(((x)>(y))?(x):(y))
#define MIN(x,y)		(((x)<(y))?(x):(y))
#define MAX3(a,b,c)		MAX(MAX(a,b),c)
#define MIN3(a,b,c)		MIN(MIN(a,b),c)
#define MID3(a,b,c)		((MIN(a,b)<(c))?(MIN(MAX(a,b),c)):(MAX(MIN(a,b),c)))
#define MAX4(a,b,c,d)	MAX(MAX(MAX(a,b),c),d)
#define MIN4(a,b,c,d)	MIN(MIN(MIN(a,b),c),d)
#define DELTA			1.0e-10
#define NULLFILE		"(null)"


#define TRUE			1
#define	FALSE			0

#define RAND_SEED		1500450271

#define MAX_STR			512
#define MAX_ENTRIES		512

int eq(float x, float y);
int le(float x, float y);
int ge(float x, float y);
int fatal (char *s);

/* initialize random number generator	*/
void init_rand(void);
/* random number within the range [0, max-1]	*/
int rand_upto(int max);
/* random number in the range [0, 1)	*/
double rand_fraction(void);

/* a table of name value pairs	*/
typedef struct str_pair_st
{
	char name[MAX_STR];
	char value[MAX_STR];
}str_pair;
/* 
 * reads tab-separated name-value pairs from file into
 * a table of size max_entries and returns the number 
 * of entries read successfully
 */
int read_str_pairs(str_pair *table, int max_entries, char *file);
/* same as above but from command line instead of a file	*/
int parse_cmdline(str_pair *table, int max_entries, int argc, char **argv);
/* append the table onto a file	*/
void dump_str_pairs(str_pair *table, int size, char *file, char *prefix);
/* table lookup	for a name */
int get_str_index(str_pair *table, int size, char *str);
/* 
 * remove duplicate names in the table - the entries later 
 * in the table are discarded. returns the new size of the
 * table
 */
int str_pairs_remove_duplicates(str_pair *table, int size);
#endif
