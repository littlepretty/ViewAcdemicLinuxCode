#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

int eq(float x, float y)
{
	return (fabs(x-y) <  DELTA);
}

int le(float x, float y)
{
	return ((x < y) || eq(x,y));
}

int ge(float x, float y)
{
	return ((x > y) || eq(x,y));
}

int fatal(char *s)
{
	fprintf(stderr, s);
	exit(1);
	return 1;
}

/* initialize random number generator	*/
void init_rand(void)
{
	srand(RAND_SEED);
}

/* random number within the range [0, max-1]	*/
int rand_upto(int max)
{
	return (max * (double) rand() / (RAND_MAX+1.0));
}

/* random number in the range [0, 1)	*/
double rand_fraction(void)
{
	return ((double) rand() / (RAND_MAX+1.0));
}

/* 
 * reads tab-separated name-value pairs from file into
 * a table of size max_entries and returns the number 
 * of entries read successfully
 */
int read_str_pairs(str_pair *table, int max_entries, char *file)
{
	int i=0;
	char str[MAX_STR], copy[MAX_STR];
	char name[MAX_STR];
	char *ptr;
	FILE *fp = fopen (file, "r");
	if (!fp) {
		sprintf (str,"error: %s could not be opened for reading\n", file);
		fatal(str);
	}
	while(i < max_entries) {
		fgets(str, MAX_STR, fp);
		if (feof(fp))
			break;
		strcpy(copy, str);
		ptr = strtok(str, " \t\n");
		if (ptr && ptr[0] != '#') {    /* ignore comments and empty lines  */
			if ((sscanf(copy, "%s%s", name, table[i].value) != 2) || (name[0] != '-'))
				fatal("invalid file format\n");
			/* ignore the leading "-"	*/
			strcpy(table[i].name, &name[1]);
			i++;
		}
	}
	fclose(fp);
	return i;
}

/* 
 * same as above but from command line instead of a file. the command
 * line is of the form <prog_name> <name-value pairs> where
 * <name-value pairs> is of the form -<variable> <value>
 */
int parse_cmdline(str_pair *table, int max_entries, int argc, char **argv)
{
	int i, count;
	for (i=1, count=0; i < argc && count < max_entries; i++) {
		if (i % 2) {	/* variable name	*/
			if (argv[i][0] != '-')
				fatal("invalid command line. check usage\n");
			/* ignore the leading "-"	*/	
			strncpy(table[count].name, &argv[i][1], MAX_STR-1);
			table[count].name[MAX_STR-1] = '\0';
		} else {		/* value	*/
			strncpy(table[count].value, argv[i], MAX_STR-1);
			table[count].value[MAX_STR-1] = '\0';
			count++;
		}
	}
	return count;
}

/* append the table onto a file	*/
void dump_str_pairs(str_pair *table, int size, char *file, char *prefix)
{
	int i; 
	char str[MAX_STR];
	FILE *fp = fopen (file, "w");
	if (!fp) {
		sprintf (str,"error: %s could not be opened for writing\n", file);
		fatal(str);
	}
	for(i=0; i < size; i++)
		fprintf(fp, "%s%s\t%s\n", prefix, table[i].name, table[i].value);
	fclose(fp);	
}

/* table lookup	for a name */
int get_str_index(str_pair *table, int size, char *str)
{
	int i;

	if (!table)
		fatal("null pointer in get_str_index\n");

	for (i = 0; i < size; i++) 
		if (!strcasecmp(str, table[i].name)) 
			return i;
	return -1;
}

/* delete entry at 'at'	*/
void delete_entry(str_pair *table, int size, int at)
{
	int i;
	/* 
	 * overwrite this entry using the next and 
	 * shift all later entries once
	 */
	for (i=at+1; i < size; i++) {
		strcpy(table[i-1].name, table[i].name);
		strcpy(table[i-1].value, table[i].value);
	}
}

/* 
 * remove duplicate names in the table - the entries later 
 * in the table are discarded. returns the new size of the
 * table
 */
int str_pairs_remove_duplicates(str_pair *table, int size)
{
	int i, j;

	for(i=0; i < size-1; i++)
		for(j=i+1; j < size; j++)
			if (!strcasecmp(table[i].name, table[j].name)) {
				delete_entry(table, size, j);
				size--;
				j--;
			}
	return size;
}

/* debug	*/
void print_str_pairs(str_pair *table, int size)
{
	int i;
	fprintf(stdout, "printing string table\n");
	for (i=0; i < size; i++)
		fprintf(stdout, "%s\t%s\n", table[i].name, table[i].value);
}
