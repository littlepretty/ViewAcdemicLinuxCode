#ifndef __TEMPERATURE_H_
#define __TEMPERATURE_H_

#include "flp.h"
#include "util.h"

/* number of extra nodes due to the model	*/
/* 5 spreader and 5 heat sink nodes (north, south, east, west and bottom)	*/
#define EXTRA	10
#define	SP_W	0
#define	SP_E	1
#define	SP_N	2
#define	SP_S	3
#define	SP_B	4
#define	SINK_W	5
#define	SINK_E	6
#define	SINK_N	7
#define	SINK_S	8
#define	SINK_B	9

/* functional block layers	*/
/* total	*/
#define NL	3
/* silicon is always layer 0	*/
/* interface layer	*/
#define IFACE	1
/* heat spreader	*/
#define HSP	2

/* physical constants	*/
#define RHO_SI	0.01	/* thermal resistivity of silicon between 300K-400K in (mK)/W	*/
#define	RHO_CU	0.0025	/* thermal resistivity of copper between 300K-400K in (mK)/W	*/
#define RHO_INT	0.75	/* thermal resistivity of the interface material in (mK)/W	*/
#define K_SI	(1.0/RHO_SI)	/* thermal conductivity of silicon	*/
#define K_CU	(1.0/RHO_CU)	/* thermal conductivity of copper	*/
#define K_INT	(1.0/RHO_INT)	/* thermal conductivity of the interface material	*/
#define SPEC_HEAT_SI	1.75e6	/* specfic heat of silicon in J/(m^3K)	*/
#define SPEC_HEAT_CU	3.55e6	/* specific heat of copper in J/(m^3K)	*/
#define SPEC_HEAT_INT	4e6		/* specific heat of the interface material in J/(m^3K)	*/

/* model specific constants	*/
#define C_FACTOR	0.5		/* fitting factor to match floworks (due to lumping)	*/

/* thermal model configuration	*/
typedef struct thermal_config_t_st
{
	/* chip specs	*/
	double t_chip;	/* chip thickness in meters	*/
	double thermal_threshold;	/* temperature threshold for DTM (Kelvin)*/

	/* heat sink specs	*/
	double c_convec;	/* convection capacitance in J/K */
	double r_convec;	/* convection resistance in K/W	*/
	double s_sink;	/* heatsink side in meters	*/
	double t_sink;	/* heatsink thickness in meters	*/

	/* heat spreader specs	*/
	double s_spreader;	/* spreader side in meters	*/
	double t_spreader;	/* spreader thickness in meters	*/

	/* interface material specs	*/
	double t_interface;	/* interface material thickness in meters	*/

	/* others	*/
	double ambient;			/* ambient temperature in kelvin	*/
	int omit_lateral;		/* omit lateral resistance?	*/
	/* initial temperatures	from file	*/
	char init_file[STR_SIZE];
	double init_temp;		/* if init_file is NULL	*/
	/* steady state temperatures to file	*/
	char steady_file[STR_SIZE];
	double sampling_intvl;	/* interval per call to compute_temp	*/
	double base_proc_freq;	/* in Hz	*/
	int dtm_used;			/* flag to guide the scaling of init Ts	*/
}thermal_config_t;

/* defaults	*/
thermal_config_t default_thermal_config(void);
/* 
 * parse a table of name-value string pairs and add the configuration
 * parameters to 'config'
 */
void thermal_config_add_from_strs(thermal_config_t *config, str_pair *table, int size);
/* 
 * convert config into a table of name-value pairs. returns the no.
 * of parameters converted
 */
int thermal_config_to_strs(thermal_config_t *config, str_pair *table, int max_entries);

/* hotspot thermal model	*/
typedef struct RC_model_t_st
{
	/* floorplan	*/
	flp_t *flp;

	/* configuration	*/
	thermal_config_t config;

	/* main matrices	*/
	/* conductance matrix and inverse	*/
	double **b, **invb;
	/* inverse of capacitance matrix	*/
	double **inva;
	/* c = inva * b	*/
	double **c;

	/* thermal capacitance fitting factors	*/
	double factor_pack;	/* for package 	*/
	double factor_chip;	/* for silicon	*/
	double factor_int;	/* for interface	*/

	/* for RK4 computation	*/
	double max_slope;

	/* intermediate vectors and matrices	*/
	double *gx, *gy, *c_ver, *gx_sp, *gy_sp, *t_vector;
	double **len, **g, **t_matrix;
	int **border;

	/* total no. of nodes	*/
	int n_nodes;
	/* total no. of blocks	*/
	int n_units;

	/* flags	*/
	int r_ready;	/* are the R's initialized?	*/
	int c_ready;	/* are the C's initialized?	*/
}RC_model_t;

/* constructor/destructor	*/
/* placeholder is an empty floorplan frame with only the names of the functional units	*/
RC_model_t *alloc_RC_model(thermal_config_t config, flp_t *placeholder);
void delete_RC_model(RC_model_t *model);

/* initialization	*/
void populate_R_model(RC_model_t *model, flp_t *flp);
void populate_C_model(RC_model_t *model, flp_t *flp);

/* hotspot main interfaces - temperature.c	*/
void steady_state_temp(RC_model_t *model, double *power, double *temp);
void compute_temp(RC_model_t *model, double *power, double *temp, double time_elapsed);
/* differs from 'vector()' in that memory for internal nodes is also allocated	*/
double *hotspot_vector(RC_model_t *model);
void set_temp (RC_model_t *model, double *temp, double val);
void dump_temp (RC_model_t *model, double *temp, char *file);
void read_temp (RC_model_t *model, double *temp, char *file, int clip);
void dump_power(RC_model_t *model, double *power, char *file);
void read_power (RC_model_t *model, double *power, char *file);
double find_max_temp(RC_model_t *model, double *temp);


/* other functions used by the above interfaces	*/

/* LUP decomposition	*/
void lupdcmp(double**a, int n, int *p);

/* get the thermal resistance values	*/
double getr(double k, double Wb, double Lb, double Ws, double t);

/* LU forward and backward substitution	*/
void lusolve(double **a, int n, int *p, double *b, double *x);

/* 4th order Runge Kutta solver	*/
void rk4(double **c, double *y, double *pow, int n, double h, double *yout);

/* vector routines	*/
double 	*vector(int n);
void free_vector(double *v);
void dump_vector(double *v, int n);

int *ivector(int n);
void free_ivector(int *v);
void dump_ivector(int *v, int n);

/* matrix routines	*/
double **matrix(int nr, int nc);
void free_matrix(double **m, int nr);
void dump_matrix(double **m, int nr, int nc);
void copy_matrix(double **dst, double **src, int nr, int nc);

int **imatrix(int nr, int nc);
void free_imatrix(int **m, int nr);
void dump_imatrix(int **m, int nr, int nc);

void matmult(double **c, double **a, double **b, int n);
void matvectmult(double *vout, double **m, double *vin, int n);
void matinv(double **INV, double **M, int n);

#endif
