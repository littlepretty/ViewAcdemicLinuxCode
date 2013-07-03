#ifndef __FLP_H_
#define __FLP_H_

#include "util.h"

#define STR_SIZE		256
#define MAX_UNITS		128
#define MAX_MOVES		16
#define	MAX_STACK		128

/* types of cuts	*/
#define CUT_NONE		-1
#define CUT_VERTICAL	-2
#define	CUT_HORIZONTAL	-3

/* wrap around L2 with extra arms	*/
#define L2_LEFT			0
#define L2_RIGHT		1
#define L2_ARMS			2
#define L2_LEFT_STR		"_left"
#define L2_RIGHT_STR	"_right"
/* L2 sizing ratio of arm width to base height	*/
#define WRAP_L2_RATIO	5

/* 
 * chip edge has true dead space, which is 
 * modeled by the following blocks
 */
#define RIM_LEFT		0
#define RIM_RIGHT		1
#define RIM_TOP			2
#define RIM_BOTTOM		3
#define RIM_BLOCKS		4
#define RIM_LEFT_STR	"RIM_left"
#define RIM_RIGHT_STR	"RIM_right"
#define RIM_TOP_STR		"RIM_top"
#define RIM_BOTTOM_STR	"RIM_bottom"

/* prefix denoting dead block	*/
#define DEAD_PREFIX		"_"

/* piecewise linear shape curve	*/
typedef struct shape_t_st
{
  double *x;	/* width	*/
  double *y;	/* height	*/
  /* 
   * if this is not a leaf in an flp,
   * the position of the orientations
   * in the contributing shape curve
   */
  int *left_pos;
  int *right_pos;
  /*
   * the position of the demarcation
   * in the combined shape curve
   */
  double *median;
  int size;
}shape_t;

/* unplaced unit	*/
typedef struct unplaced_t_st
{
  char name[STR_SIZE];
  /* can be rotated?	*/
  int rotable;
  double area;
  /* minimum and maximum aspect ratios	*/
  double min_aspect;
  double max_aspect;
  /* shape curve for this unit	*/
  shape_t *shape;
}unplaced_t;

/* normalized polish expression	*/
typedef struct NPE_t_st
{
	int *elements;
	int size;

	/* positions of the units	*/
	int *unit_pos;
	int n_units;

	/* 
	 * flipping positions  - where 
	 * a unit is immediately adjacent 
	 * to a cut_type and vice-versa
	 */
	int *flip_pos;
	int n_flips;

	/* positions of the chains	*/
	int *chain_pos;
	int n_chains;

	/* no. of units till this position	*/
	int *ballot_count;
}NPE_t;

/* slicing tree node	*/
typedef struct tree_node_t_st
{
	shape_t *curve;
	union {
		int cut_type;
		int unit;
	}label;	
	struct tree_node_t_st *left;
	struct tree_node_t_st *right;
}tree_node_t;

/* tree node stack	*/
typedef struct tree_node_stack_t_st
{
	tree_node_t *array[MAX_STACK];
	int top;
}tree_node_stack_t;

/* placed functional unit */
typedef struct unit_t_st
{
	char name[STR_SIZE];
	double width;
	double height;
	double leftx;
	double bottomy;
}unit_t;

/* floorplan data structure	*/
typedef struct flp_t_st
{
	unit_t *units;
	int n_units;
  	/* density of wires between units	*/
  	double **wire_density;
} flp_t;

/* configuration parameters for the floorplan	*/
typedef struct flp_config_t_st
{
	/* wrap around L2?	*/
	int wrap_l2;
	/* name of L2 to look for	*/
	char l2_label[STR_SIZE];

	/* model dead space around the rim of the chip? */
	int model_rim;
	double rim_thickness;

	/* area ratio below which to ignore dead space	*/
	double compact_ratio;

	/* 
	 * no. of discrete orientations for a shape curve.
	 * should be an even number greater than 1
	 */
	int n_orients;
	
	/* annealing parameters	*/
	double P0;		/* initial acceptance probability	*/
	double Davg;	/* average change (delta) in cost	*/
	int Kmoves;		/* no. of moves to try in each step	*/
	double Rcool;	/* ratio for the cooling schedule */
	double Rreject;	/* ratio of rejects at which to stop annealing */
	int Nmax;		/* absolute max no. of annealing steps	*/

	/* weights for the metric: A + lambdaT * T + lambdaW * W	*/
	double lambdaT;
	double lambdaW;
} flp_config_t;

/* input description for floorplanning	*/
typedef struct flp_desc_t_st
{
  unplaced_t *units;
  /* density of wires between units	*/
  double **wire_density;
  /* configuration parameters	*/
  flp_config_t config;
  int n_units;
}flp_desc_t;

/* routines	*/

/* 
 * make a shape curve from area and aspect ratios
 * - allocates memory internally
 */
shape_t *shape_from_aspect(double area, double min, 
                           double max, int rotable,
						   int n_orients);
/* shape curve arithmetic	*/
shape_t *shape_add(shape_t *shape1, shape_t *shape2, int cut_type);
void free_shape(shape_t *shape); 
int min_area_pos(shape_t *curve);
/* debug print	*/
void print_shape(shape_t *shape);

/* debug print	*/
void print_unplaced(unplaced_t *unit);

/* read floorplan description and allocate memory	*/
flp_desc_t *read_flp_desc(char *file, flp_config_t *config);
void free_flp_desc(flp_desc_t *flp_desc);
/* debug print	*/
void print_flp_desc(flp_desc_t *flp_desc);

/* stack operations	*/
tree_node_stack_t *new_tree_node_stack(void);
void tree_node_stack_push(tree_node_stack_t *stack, tree_node_t *node);
tree_node_t *tree_node_stack_pop(tree_node_stack_t *stack);
int tree_node_stack_isfull(tree_node_stack_t *stack);
int tree_node_stack_isempty(tree_node_stack_t *stack);
void free_tree_node_stack(tree_node_stack_t *stack);
void tree_node_stack_clear(tree_node_stack_t *stack);

/* tree operations	*/
/* construct floorplan slicing tree from NPE	*/
tree_node_t *tree_from_NPE(flp_desc_t *flp_desc, 
						   tree_node_stack_t *stack, 
						   NPE_t *expr);
void free_tree(tree_node_t *root);
void print_tree(tree_node_t *root, flp_desc_t *flp_desc);
/* 
 * print only the portion of the shape curves 
 * corresponding to the `pos'th entry of root->curve
 */
void print_tree_relevant(tree_node_t *root, int pos, flp_desc_t *flp_desc);
/* 
 * convert slicing tree into actual floorplan
 * returns the number of dead blocks compacted
 */
int tree_to_flp(tree_node_t *root, flp_t *flp, int compact_dead, 
				double compact_ratio);

/* 
 * print the floorplan in a FIG like format 
 * that can be read by tofig.pl to produce 
 * an xfig output 
 */
void print_flp_fig (flp_t *flp);
struct RC_model_t_st;	/* forward declaration: see temperature.h	*/
/* 
 * main flooplanning routine - allocates 
 * memory internally. returns the number
 * of compacted blocks
 */ 
int floorplan(flp_t *flp, flp_desc_t *flp_desc,
			  struct RC_model_t_st *model, double *power);
/* wrap the L2 around this floorplan	*/			   
void flp_wrap_l2(flp_t *flp, flp_desc_t *flp_desc);
/* dump the floorplan onto a file	*/
void dump_flp(flp_t *flp, char *file, int dump_connects);
/* skip floorplanning and read floorplan directly from file */
flp_t *read_flp(char *file, int read_connects);
/* memory uninitialization	*/
void free_flp(flp_t *flp, int compacted);
/* create a floorplan placeholder from description	*/
flp_t *flp_placeholder(flp_desc_t *flp_desc);
/* the metric used to evaluate the floorplan	*/
double flp_evaluate_metric(flp_t *flp, struct RC_model_t_st *model, 
						   double *power, double lambdaT, double lambdaW);

/* default flp_config	*/
flp_config_t default_flp_config(void);
/* 
 * parse a table of name-value string pairs and add the configuration
 * parameters to 'config'
 */
void flp_config_add_from_strs(flp_config_t *config, str_pair *table, int size);
/* 
 * convert config into a table of name-value pairs. returns the no.
 * of parameters converted
 */
int flp_config_to_strs(flp_config_t *config, str_pair *table, int max_entries);

/* NPE routines	*/

/* the starting solution for simulated annealing	*/
NPE_t *NPE_get_initial(flp_desc_t *flp_desc);
void free_NPE(NPE_t *expr);
/* debug print	*/
void print_NPE(NPE_t *expr, flp_desc_t *flp_desc);
/* 
 * move M1 of the floorplan paper 
 * swap two units adjacent in the NPE	
 */
void NPE_swap_units(NPE_t *expr, int pos);
/* move M2 - invert a chain of cut_types in the NPE	*/
void NPE_invert_chain(NPE_t *expr, int pos);
/* move M3 - swap adjacent cut_type and unit in the NPE	*/
int NPE_swap_cut_unit(NPE_t *expr, int pos);
/* make a random move out of the above	*/
NPE_t *make_random_move(NPE_t *expr);
/* make a copy of this NPE	*/
NPE_t *NPE_duplicate(NPE_t *expr);

/* placed floorplan access routines	*/

/* get unit index from its name	*/
int get_blk_index(flp_t *flp, char *name);
/* are the units horizontally adjacent?	*/
int is_horiz_adj(flp_t *flp, int i, int j);
/* are the units vertically adjacent?	*/
int is_vert_adj (flp_t *flp, int i, int j);
/* shared length between units	*/
double get_shared_len(flp_t *flp, int i, int j);
/* total chip width	*/
double get_total_width(flp_t *flp);
/* total chip height */
double get_total_height(flp_t *flp);
/* other queries	*/
double get_manhattan_dist(flp_t *flp, int i, int j);
double get_total_area(flp_t *flp);
double get_occupied_area(flp_t *flp);
double get_wire_metric(flp_t *flp);
#endif
