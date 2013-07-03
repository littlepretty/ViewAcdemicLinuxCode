#include "temperature.h"
#include "flp.h"
#include "util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <strings.h>

/* maximum power density possible (say 150W for a 10mm x 10mm chip)	*/
#define MAX_PD	(1.5e6)

/* default thermal configuration parameters	*/
thermal_config_t default_thermal_config(void)
{
	thermal_config_t config;

	/* chip specs	*/
	config.t_chip = 0.5e-3;			/* chip thickness in meters	*/
	/* temperature threshold for DTM (Kelvin)*/
	config.thermal_threshold = 111.8 + 273.15;

	/* heat sink specs	*/
	config.c_convec = 140.4;		/* convection capacitance in J/K */
	config.r_convec = 0.1;			/* convection resistance in K/W	*/
	config.s_sink = 60e-3;			/* heatsink side in m	*/
	config.t_sink = 6.9e-3; 		/* heatsink thickness  in m	*/

	/* heat spreader specs	*/
	config.s_spreader = 30e-3;		/* spreader side in m	*/
	config.t_spreader = 1e-3;		/* spreader thickness in m	*/

	/* interface material specs	*/
	config.t_interface = 75e-6;		/* interface material thickness in m */

	/* others	*/
	config.ambient = 40 + 273.15;	/* in kelvin	*/
	config.omit_lateral = FALSE;	/* omit lateral chip resistances?	*/
	/* initial temperatures	from file	*/
	strcpy(config.init_file, NULLFILE);	
	config.init_temp = 60 + 273.15;	/* in Kelvin	*/
	/* steady state temperatures to file	*/
	strcpy(config.steady_file, NULLFILE);
 	/* 3.33 us sampling interval = 10K cycles at 3GHz	*/
	config.sampling_intvl = 3.333e-6;
	config.base_proc_freq = 3e9;	/* base processor frequency in Hz	*/
	config.dtm_used = FALSE;		/* set accordingly	*/

	return config;
}

/* 
 * parse a table of name-value string pairs and add the configuration
 * parameters to 'config'
 */
void thermal_config_add_from_strs(thermal_config_t *config, str_pair *table, int size)
{
	int idx;
	if ((idx = get_str_index(table, size, "t_chip")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->t_chip) != 1)
			fatal("invalid format for configuration  parameter t_chip");
	if ((idx = get_str_index(table, size, "thermal_threshold")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->thermal_threshold) != 1)
			fatal("invalid format for configuration  parameter thermal_threshold");
	if ((idx = get_str_index(table, size, "c_convec")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->c_convec) != 1)
			fatal("invalid format for configuration  parameter c_convec");
	if ((idx = get_str_index(table, size, "r_convec")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->r_convec) != 1)
			fatal("invalid format for configuration  parameter r_convec");
	if ((idx = get_str_index(table, size, "s_sink")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->s_sink) != 1)
			fatal("invalid format for configuration  parameter s_sink");
	if ((idx = get_str_index(table, size, "t_sink")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->t_sink) != 1)
			fatal("invalid format for configuration  parameter t_sink");
	if ((idx = get_str_index(table, size, "s_spreader")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->s_spreader) != 1)
			fatal("invalid format for configuration  parameter s_spreader");
	if ((idx = get_str_index(table, size, "t_spreader")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->t_spreader) != 1)
			fatal("invalid format for configuration  parameter t_spreader");
	if ((idx = get_str_index(table, size, "t_interface")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->t_interface) != 1)
			fatal("invalid format for configuration  parameter t_interface");
	if ((idx = get_str_index(table, size, "ambient")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->ambient) != 1)
			fatal("invalid format for configuration  parameter ambient");
	if ((idx = get_str_index(table, size, "omit_lateral")) >= 0)
		if(sscanf(table[idx].value, "%d", &config->omit_lateral) != 1)
			fatal("invalid format for configuration  parameter omit_lateral");
	if ((idx = get_str_index(table, size, "init_file")) >= 0)
		if(sscanf(table[idx].value, "%s", config->init_file) != 1)
			fatal("invalid format for configuration  parameter init_file");
	if ((idx = get_str_index(table, size, "init_temp")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->init_temp) != 1)
			fatal("invalid format for configuration  parameter init_temp");
	if ((idx = get_str_index(table, size, "steady_file")) >= 0)
		if(sscanf(table[idx].value, "%s", config->steady_file) != 1)
			fatal("invalid format for configuration  parameter steady_file");
	if ((idx = get_str_index(table, size, "sampling_intvl")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->sampling_intvl) != 1)
			fatal("invalid format for configuration  parameter sampling_intvl");
	if ((idx = get_str_index(table, size, "base_proc_freq")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->base_proc_freq) != 1)
			fatal("invalid format for configuration  parameter base_proc_freq");
	if ((idx = get_str_index(table, size, "dtm_used")) >= 0)
		if(sscanf(table[idx].value, "%d", &config->dtm_used) != 1)
			fatal("invalid format for configuration  parameter dtm_used");
	
	if ((config->t_chip <= 0) || (config->s_sink <= 0) || (config->t_sink <= 0) || 
		(config->s_spreader <= 0) || (config->t_spreader <= 0) || 
		(config->t_interface <= 0))
		fatal("chip and package dimensions should be greater than zero\n");
	if ((config->thermal_threshold < 0) || (config->c_convec < 0) || 
		(config->r_convec < 0) || (config->ambient < 0) || 
		(config->base_proc_freq <= 0) || (config->sampling_intvl <= 0))
		fatal("invalid thermal simulation parameters\n");
}

/* 
 * convert config into a table of name-value pairs. returns the no.
 * of parameters converted
 */
int thermal_config_to_strs(thermal_config_t *config, str_pair *table, int max_entries)
{
	if (max_entries < 17)
		fatal("not enough entries in table\n");

	sprintf(table[0].name, "t_chip");
	sprintf(table[1].name, "thermal_threshold");
	sprintf(table[2].name, "c_convec");
	sprintf(table[3].name, "r_convec");
	sprintf(table[4].name, "s_sink");
	sprintf(table[5].name, "t_sink");
	sprintf(table[6].name, "s_spreader");
	sprintf(table[7].name, "t_spreader");
	sprintf(table[8].name, "t_interface");
	sprintf(table[9].name, "ambient");
	sprintf(table[10].name, "omit_lateral");
	sprintf(table[11].name, "init_file");
	sprintf(table[12].name, "init_temp");
	sprintf(table[13].name, "steady_file");
	sprintf(table[14].name, "sampling_intvl");
	sprintf(table[15].name, "base_proc_freq");
	sprintf(table[16].name, "dtm_used");

	sprintf(table[0].value, "%lg", config->t_chip);
	sprintf(table[1].value, "%lg", config->thermal_threshold);
	sprintf(table[2].value, "%lg", config->c_convec);
	sprintf(table[3].value, "%lg", config->r_convec);
	sprintf(table[4].value, "%lg", config->s_sink);
	sprintf(table[5].value, "%lg", config->t_sink);
	sprintf(table[6].value, "%lg", config->s_spreader);
	sprintf(table[7].value, "%lg", config->t_spreader);
	sprintf(table[8].value, "%lg", config->t_interface);
	sprintf(table[9].value, "%lg", config->ambient);
	sprintf(table[10].value, "%d", config->omit_lateral);
	sprintf(table[11].value, "%s", config->init_file);
	sprintf(table[12].value, "%lg", config->init_temp);
	sprintf(table[13].value, "%s", config->steady_file);
	sprintf(table[14].value, "%lg", config->sampling_intvl);
	sprintf(table[15].value, "%lg", config->base_proc_freq);
	sprintf(table[16].value, "%d", config->dtm_used);

	return 17;
}

/* 
 * allocate memory for the matrices. placeholder can be an empty 
 * floorplan frame with only the names of the functional units
 */
RC_model_t *alloc_RC_model(thermal_config_t config, flp_t *placeholder)
{
	/* shortcuts	*/
	int n = placeholder->n_units;
	int m = NL*n+EXTRA;

	RC_model_t *model = (RC_model_t *) calloc (1, sizeof(RC_model_t));
	if (!model)
		fatal("memory allocation error\n");
	model->config = config;
	model->n_units = n;
	model->n_nodes = m;

	model->border = imatrix(n, 4);
	model->len = matrix(n, n);	/* len[i][j] = length of shared edge bet. i & j	*/
	model->g = matrix(m, m);	/* g[i][j] = conductance bet. nodes i & j */
	model->gx = vector(n);		/* lumped conductances in x direction	*/
	model->gy = vector(n);		/* lumped conductances in y direction	*/
	model->gx_sp = vector(n);	/* lateral conductances in the spreader	layer */
	model->gy_sp = vector(n);
	model->c_ver = vector(m);	/* vertical capacitance	*/
	model->t_vector = vector(m);/* scratch pad	*/

	/* B, C, INVA  and INVB are (NL*n+EXTRA)x(NL*n+EXTRA) matrices	*/
	model->b = matrix(m, m);
	model->c = matrix(m, m);
	model->inva = matrix(m, m);
	model->invb = matrix(m, m);
	model->t_matrix = matrix (m, m);	/* copy of B	*/
	
	model->factor_pack = C_FACTOR;

	model->flp = placeholder;
	return model;
}

/* creates matrices  B and invB: BT = Power in the steady state. 
 * NOTE: EXTRA nodes: 1 interface bottom, 5 spreader and 5 heat sink nodes
 * (north, south, east, west and bottom).
 */
void populate_R_model(RC_model_t *model, flp_t *flp)
{
	/*	shortcuts	*/
	double **b = model->b, **invb = model->invb;
	double *gx = model->gx, *gy = model->gy;
	double *gx_sp = model->gx_sp, *gy_sp = model->gy_sp;
	double **len = model->len, **g = model->g, **t_matrix = model->t_matrix;
	int **border = model->border;
	double t_chip = model->config.t_chip;
	double r_convec = model->config.r_convec;
	double s_sink = model->config.s_sink;
	double t_sink = model->config.t_sink;
	double s_spreader = model->config.s_spreader;
	double t_spreader = model->config.t_spreader;
	double t_interface = model->config.t_interface;

	int i, j, k = 0, n = flp->n_units;
	double r_sp1, r_sp2, r_hs;	/* lateral resistances to spreader and heatsink	*/

	/* NOTE: *_mid - the vertical Rs from CENTER nodes of spreader 
	 * and heatsink. *_per - the vertical Rs from PERIPHERAL (n,s,e,w) nodes
	 */
	double r_sp_per, r_hs_mid, r_hs_per;
	double gn_sp=0, gs_sp=0, ge_sp=0, gw_sp=0;

	double w_chip = get_total_width (flp);	/* x-axis	*/
	double l_chip = get_total_height (flp);	/* y-axis	*/

	/* sanity check on floorplan sizes	*/
	if (w_chip > s_sink || l_chip > s_sink || 
		w_chip > s_spreader || l_chip > s_spreader) {
		print_flp_fig(flp);
		fatal("inordinate floorplan size!\n");
	}	

	/* gx's and gy's of blocks	*/
	for (i = 0; i < n; i++) {
		/* at the silicon layer	*/
		if (model->config.omit_lateral) {
			gx[i] = gy[i] = 0;
		}
		else {
			gx[i] = 1.0/getr(K_SI, flp->units[i].height, flp->units[i].width, l_chip, t_chip);
			gy[i] = 1.0/getr(K_SI, flp->units[i].width, flp->units[i].height, w_chip, t_chip);
		}

		/* at the spreader layer	*/
		gx_sp[i] = 1.0/getr(K_CU, flp->units[i].height, flp->units[i].width, l_chip, t_spreader);
		gy_sp[i] = 1.0/getr(K_CU, flp->units[i].width, flp->units[i].height, w_chip, t_spreader);
	}

	/* shared lengths between blocks	*/
	for (i = 0; i < n; i++) 
		for (j = i; j < n; j++) 
			len[i][j] = len[j][i] = get_shared_len(flp, i, j);

	/* lateral R's of spreader and sink */
	r_sp1 = getr(K_CU, (s_spreader+3*w_chip)/4.0, (s_spreader-w_chip)/4.0, w_chip, t_spreader);
	r_sp2 = getr(K_CU, (3*s_spreader+w_chip)/4.0, (s_spreader-w_chip)/4.0, (s_spreader+3*w_chip)/4.0, t_spreader);
	r_hs = getr(K_CU, (s_sink+3*s_spreader)/4.0, (s_sink-s_spreader)/4.0, s_spreader, t_sink);

	/* vertical R's of spreader and sink */
	r_sp_per = RHO_CU * t_spreader * 4.0 / (s_spreader * s_spreader - w_chip*l_chip);
	r_hs_mid = RHO_CU * t_sink / (s_spreader*s_spreader);
	r_hs_per = RHO_CU * t_sink * 4.0 / (s_sink * s_sink - s_spreader*s_spreader);

	/* short the R's from block centers to a particular chip edge	*/
	for (i = 0; i < n; i++) {
		if (eq(flp->units[i].bottomy + flp->units[i].height, l_chip)) {
			gn_sp += gy_sp[i];
			border[i][2] = 1;	/* block is on northern border 	*/
		}	
		if (eq(flp->units[i].bottomy, 0)) {
			gs_sp += gy_sp[i];
			border[i][3] = 1;	/* block is on southern border	*/
		}	
		if (eq(flp->units[i].leftx + flp->units[i].width, w_chip)) {
			ge_sp += gx_sp[i];
			border[i][1] = 1;	/* block is on eastern border	*/
		}	
		if (eq(flp->units[i].leftx, 0)) {
			gw_sp += gx_sp[i];
			border[i][0] = 1;	/* block is on western border	*/
		}	
	}

	/* overall Rs between nodes */
	for (i = 0; i < n; i++) {
		double area = (flp->units[i].height * flp->units[i].width);
		/* 
		 * amongst functional units	in the various layers
		 * resistances in the interface layer are assumed 
		 * to be infinite
		 */
		for (j = 0; j < n; j++) {
			double part = 0, part_sp = 0;
			if (is_horiz_adj(flp, i, j)) {
				part = gx[i] / flp->units[i].height;
				part_sp = gx_sp[i] / flp->units[i].height;
			}
			else if (is_vert_adj(flp, i,j))  {
				part = gy[i] / flp->units[i].width;
				part_sp = gy_sp[i] / flp->units[i].width;
			}
			g[i][j] = part * len[i][j];
			g[HSP*n+i][HSP*n+j] = part_sp * len[i][j];
		}

 		/* vertical g's in the silicon layer	*/
		g[i][IFACE*n+i]=g[IFACE*n+i][i]=2.0/(RHO_SI * t_chip / area);
 		/* vertical g's in the interface layer	*/
		g[IFACE*n+i][HSP*n+i]=g[HSP*n+i][IFACE*n+i]=2.0/(RHO_INT * t_interface / area);
		/* vertical g's in the spreader layer	*/
		g[HSP*n+i][NL*n+SP_B]=g[NL*n+SP_B][HSP*n+i]=2.0/(RHO_CU * t_spreader / area);

		/* lateral g's from block center (spreader layer) to peripheral (n,s,e,w) spreader nodes	*/
		g[HSP*n+i][NL*n+SP_N]=g[NL*n+SP_N][HSP*n+i]=2.0*border[i][2]/((1.0/gy_sp[i])+r_sp1*gn_sp/gy_sp[i]);
		g[HSP*n+i][NL*n+SP_S]=g[NL*n+SP_S][HSP*n+i]=2.0*border[i][3]/((1.0/gy_sp[i])+r_sp1*gs_sp/gy_sp[i]);
		g[HSP*n+i][NL*n+SP_E]=g[NL*n+SP_E][HSP*n+i]=2.0*border[i][1]/((1.0/gx_sp[i])+r_sp1*ge_sp/gx_sp[i]);
		g[HSP*n+i][NL*n+SP_W]=g[NL*n+SP_W][HSP*n+i]=2.0*border[i][0]/((1.0/gx_sp[i])+r_sp1*gw_sp/gx_sp[i]);
	}

	/* vertical g's between central nodes	*/
 	/* between spreader bottom and sink bottom	*/
	g[NL*n+SINK_B][NL*n+SP_B]=g[NL*n+SP_B][NL*n+SINK_B]=2.0/r_hs_mid;

	/* g's from peripheral(n,s,e,w) nodes	*/
	for (i = 1; i <= 4; i++) {
 		/* vertical g's between peripheral spreader nodes and spreader bottom */
		g[NL*n+SP_B-i][NL*n+SP_B]=g[NL*n+SP_B][NL*n+SP_B-i]=2.0/r_sp_per;
 		/* lateral g's between peripheral spreader nodes and peripheral sink nodes	*/
		g[NL*n+SP_B-i][NL*n+SINK_B-i]=g[NL*n+SINK_B-i][NL*n+SP_B-i]=2.0/(r_hs + r_sp2);
 		/* vertical g's between peripheral sink nodes and sink bottom	*/
		g[NL*n+SINK_B-i][NL*n+SINK_B]=g[NL*n+SINK_B][NL*n+SINK_B-i]=2.0/r_hs_per;
	}	

	/* calculate matrix B such that BT = POWER in steady state */
	for (i = 0; i < NL*n+EXTRA; i++) {
		for (j = 0; j < NL*n+EXTRA; j++) {
			if (i==j) {
				if (i == NL*n+SINK_B)	/* sink bottom */
					b[i][j] = 1.0 / r_convec;
				else
					b[i][j] = 0;
				for (k = 0; k < NL*n+EXTRA; k++) {
					if ((g[i][k]==0.0)||(g[k][i])==0.0) 
						continue;
					else 
					/* here is why the 2.0 factor comes when calculating g[][]	*/
						b[i][j] += 1.0/((1.0/g[i][k])+(1.0/g[k][i]));
				}
			} else {
				if ((g[i][j]==0.0)||(g[j][i])==0.0)
					b[i][j]=0.0;
				else
					b[i][j]=-1.0/((1.0/g[i][j])+(1.0/g[j][i]));
			}
		}
	}

	/* we will also be needing INVB so store it too	*/
	copy_matrix(t_matrix, b, NL*n+EXTRA, NL*n+EXTRA);
	matinv(invb, t_matrix, NL*n+EXTRA);

	/* done	*/
	model->flp = flp;
	model->r_ready = TRUE;
}

/* creates 2 matrices: invA, C: dT + A^-1*BT = A^-1*Power, 
 * C = A^-1 * B. note that A is a diagonal matrix (no lateral
 * capacitances. all capacitances are to ground). so, inva[i][i]
 * (= 1/a[i][i]) is just enough.
 *
 * NOTE: EXTRA nodes: 1 interface bottom, 5 spreader and 5 heat sink nodes
 * (north, south, east, west and bottom).
 */

void populate_C_model(RC_model_t *model, flp_t *flp)
{
	/*	shortcuts	*/
	double **inva = model->inva, **c = model->c;
	double **b = model->b;
	double *c_ver = model->c_ver;
	double t_chip = model->config.t_chip;
	double c_convec = model->config.c_convec;
	double s_sink = model->config.s_sink;
	double t_sink = model->config.t_sink;
	double s_spreader = model->config.s_spreader;
	double t_spreader = model->config.t_spreader;
	double t_interface = model->config.t_interface;

	int i, n = flp->n_units;

	if (!model->r_ready)
		fatal("R model not ready\n");
	if (model->flp != flp)
		fatal("different floorplans for R and C models!");

	/* NOTE: *_mid - the vertical Cs from CENTER nodes of spreader 
	 * and heatsink. *_per - the vertical Cs from PERIPHERAL (n,s,e,w) nodes
	 */
	double  c_sp_per, c_hs_mid, c_hs_per;

	double w_chip = get_total_width (flp);	/* x-axis	*/
	double l_chip = get_total_height (flp);	/* y-axis	*/

	/* compute the silicon fitting factor - see pg 10 of the UVA CS tech report - CS-TR-2003-08	*/
	model->factor_chip = C_FACTOR * ((SPEC_HEAT_INT / SPEC_HEAT_SI) * (w_chip + 0.88 * t_interface) \
				* (l_chip + 0.88 * t_interface) * t_interface / ( w_chip * l_chip * t_chip) + 1);

	/* fitting factor for interface	 - same rationale as above */
	model->factor_int = C_FACTOR * ((SPEC_HEAT_CU / SPEC_HEAT_INT) * (w_chip + 0.88 * t_spreader) \
				* (l_chip + 0.88 * t_spreader) * t_spreader / ( w_chip * l_chip * t_interface) + 1);

	/*fprintf(stdout, "fitting factors : %lf, %lf\n", factor_chip, factor_int);	*/

	/* vertical C's of spreader and sink */
	c_sp_per = model->factor_pack * SPEC_HEAT_CU * t_spreader * (s_spreader * s_spreader - w_chip*l_chip) / 4.0;
	c_hs_mid = model->factor_pack * SPEC_HEAT_CU * t_sink * (s_spreader * s_spreader);
	c_hs_per = model->factor_pack * SPEC_HEAT_CU * t_sink * (s_sink * s_sink - s_spreader*s_spreader) / 4.0;

	/* overall Cs between nodes */
	for (i = 0; i < n; i++) {
		double area = (flp->units[i].height * flp->units[i].width);
		/* C's from functional units to ground	*/
		c_ver[i] = model->factor_chip * SPEC_HEAT_SI * t_chip * area;
		/* C's from interface portion of the functional units to ground	*/
		c_ver[IFACE*n+i] = model->factor_int * SPEC_HEAT_INT * t_interface * area;
		/* C's from spreader portion of the functional units to ground	*/
		c_ver[HSP*n+i] = model->factor_pack * SPEC_HEAT_CU * t_spreader * area;
	}

	/* max slope (max_power * max_vertical_R / vertical RC time constant) for silicon	*/
	model->max_slope = MAX_PD / (model->factor_chip * t_chip * SPEC_HEAT_SI);

	/* vertical C's between central nodes	*/
 	/* from spreader bottom to ground	*/
	c_ver[NL*n+SP_B]=c_hs_mid;
 	/* from sink bottom to ground	*/
	c_ver[NL*n+SINK_B] = model->factor_pack * c_convec;

	/* C's from peripheral(n,s,e,w) nodes	*/
	for (i = 1; i <= 4; i++) {
 		/* from peripheral spreader nodes to ground	*/
		c_ver[NL*n+SP_B-i]=c_sp_per;
 		/* from peripheral sink nodes to ground	*/
		c_ver[NL*n+SINK_B-i]=c_hs_per;
	}

	/* calculate diagonal matrix A such that A(dT) + BT = POWER */
	for (i = 0; i < NL*n+EXTRA; i++)
		inva[i][i] = 1.0/c_ver[i];

	/* we are always going to use the eqn dT + A^-1 * B T = A^-1 * POWER. so, store  C = A^-1 * B	*/
	matmult(c, inva, b, NL*n+EXTRA);

	/*	done	*/
	model->c_ready = TRUE;
}

/* setting internal node power numbers	*/
void set_internal_power (RC_model_t *model, double *pow)
{
	int i;
	for (i=model->n_units; i < model->n_nodes-1; i++)
		pow[i] = 0;
	pow[model->n_nodes-1] = model->config.ambient / model->config.r_convec;
}

/* power and temp should both be alloced using hotspot_vector. 
 * 'b' is the 'thermal conductance' matrix. i.e, b * temp = power
 *  => temp = invb * power
 */
void steady_state_temp(RC_model_t *model, double *power, double *temp) 
{
	if (!model->r_ready)
		fatal("R model not ready\n");

	/* set power numbers for the virtual nodes */
	set_internal_power(model, power);

	/* find temperatures	*/
	matvectmult(temp, model->invb, power, model->n_nodes);
}

/* required precision in degrees	*/
#define PRECISION	0.001
#define TOO_LONG	100000
#define MIN_ITER	1

/* compute_temp: solve for temperature from the equation dT + CT = inv_A * Power 
 * Given the temperature (temp) at time t, the power dissipation per cycle during the 
 * last interval (time_elapsed), find the new temperature at time t+time_elapsed.
 * power and temp should both be alloced using hotspot_vector
 */
void compute_temp(RC_model_t *model, double *power, double *temp, double time_elapsed)
{
	int i;
	double h, n_iter;
	
	if (!model->r_ready || !model->c_ready)
		fatal("RC model not ready\n");
	if (temp == model->t_vector)
		fatal("output same as scratch pad\n");

	/* set power numbers for the virtual nodes */
	set_internal_power(model, power);

	/* use the scratch pad vector to find (inv_A)*POWER */
	matvectmult(model->t_vector, model->inva, power, model->n_nodes);

 	/* step size for 4th-order Runge-Kutta  - assume worst case	*/
	h = PRECISION / model->max_slope;
	n_iter = time_elapsed / h;
	n_iter = (n_iter > MIN_ITER) ? n_iter : MIN_ITER;	/* do at least MIN_ITER iterations	*/
	h = time_elapsed / n_iter;
	
	if (n_iter >= TOO_LONG)
		fprintf(stdout, "warning: calling interval too large, performing %.0f iterations - it may take REALLY long\n", n_iter);
	
	/* Obtain temp at time (t+h). 
	 * Instead of getting the temperature at t+h directly, we do it 
	 * in n_iter steps to reduce the error due to rk4
	 */
	for (i = 0; i < n_iter; i++) 	
		rk4(model->c, temp, model->t_vector, model->n_nodes, h, temp);
}

/* differs from 'vector()' in that memory for internal nodes is also allocated	*/
double *hotspot_vector(RC_model_t *model)
{
	return vector(model->n_nodes);
}

/* sets the temperature of a vector 'temp' allocated using 'hotspot_vector'	*/
void set_temp(RC_model_t *model, double *temp, double val)
{
	int i;
	for(i=0; i < model->n_nodes; i++)
		temp[i] = val;
}

/* dump temperature vector alloced using 'hotspot_vector' to 'file' */ 
void dump_temp(RC_model_t *model, double *temp, char *file)
{
	flp_t *flp = model->flp;
	int i;
	char str[STR_SIZE];
	FILE *fp = fopen (file, "w");
	if (!fp) {
		sprintf (str,"error: %s could not be opened for writing\n", file);
		fatal(str);
	}
	/* on chip temperatures	*/
	for (i=0; i < flp->n_units; i++)
		fprintf(fp, "%s\t%.1f\n", flp->units[i].name, temp[i]);

	/* interface temperatures	*/
	for (i=0; i < flp->n_units; i++)
		fprintf(fp, "iface_%s\t%.1f\n", flp->units[i].name, temp[IFACE*flp->n_units+i]);

	/* spreader temperatures	*/
	for (i=0; i < flp->n_units; i++)
		fprintf(fp, "hsp_%s\t%.1f\n", flp->units[i].name, temp[HSP*flp->n_units+i]);

	/* internal node temperatures	*/
	for (i=0; i < EXTRA; i++) {
		sprintf(str, "inode_%d", i);
		fprintf(fp, "%s\t%.1f\n", str, temp[i+NL*flp->n_units]);
	}
	fclose(fp);	
}

/* 
 * read temperature vector alloced using 'hotspot_vector' from 'file'
 * which was dumped using 'dump_temp'. values are clipped to thermal
 * threshold based on 'clip'
 */ 
void read_temp(RC_model_t *model, double *temp, char *file, int clip)
{
	/*	shortcuts	*/
	flp_t *flp = model->flp;
	double thermal_threshold = model->config.thermal_threshold;
	double ambient = model->config.ambient;

	int i, idx;
	double max=0, val;
	char str[STR_SIZE], name[STR_SIZE];
	FILE *fp = fopen (file, "r");
	if (!fp) {
		sprintf (str,"error: %s could not be opened for reading\n", file);
		fatal(str);
	}	

	/* find max temp on the chip	*/
	for (i=0; i < flp->n_units; i++) {
		fgets(str, STR_SIZE, fp);
		if (feof(fp))
			fatal("not enough lines in temperature file\n");
		if (sscanf(str, "%s%lf", name, &val) != 2)
			fatal("invalid temperature file format\n");
		idx = get_blk_index(flp, name);
		if (idx >= 0)
			temp[idx] = val;
		else	/* since get_blk_index calls fatal, the line below cannot be reached	*/
			fatal ("unit in temperature file not found in floorplan\n");
		if (temp[idx] > max)
			max = temp[idx];
	}

	/* interface material temperatures	*/
	for (i=0; i < flp->n_units; i++) {
		fgets(str, STR_SIZE, fp);
		if (feof(fp))
			fatal("not enough lines in temperature file\n");
		if (sscanf(str, "iface_%s%lf", name, &val) != 2)
			fatal("invalid temperature file format\n");
		idx = get_blk_index(flp, name);
		if (idx >= 0)
			temp[idx+IFACE*flp->n_units] = val;
		else	/* since get_blk_index calls fatal, the line below cannot be reached	*/
			fatal ("unit in temperature file not found in floorplan\n");
	}

	/* heat spreader temperatures	*/
	for (i=0; i < flp->n_units; i++) {
		fgets(str, STR_SIZE, fp);
		if (feof(fp))
			fatal("not enough lines in temperature file\n");
		if (sscanf(str, "hsp_%s%lf", name, &val) != 2)
			fatal("invalid temperature file format\n");
		idx = get_blk_index(flp, name);
		if (idx >= 0)
			temp[idx+HSP*flp->n_units] = val;
		else	/* since get_blk_index calls fatal, the line below cannot be reached	*/
			fatal ("unit in temperature file not found in floorplan\n");
	}

	/* internal node temperatures	*/	
	for (i=0; i < EXTRA; i++) {
		fgets(str, STR_SIZE, fp);
		if (feof(fp))
			fatal("not enough lines in temperature file\n");
		if (sscanf(str, "%s%lf", name, &val) != 2)
			fatal("invalid temperature file format\n");
		sprintf(str, "inode_%d", i);
		if (strcasecmp(str, name))
			fatal("invalid temperature file format\n");
		temp[i+NL*flp->n_units] = val;	
	}

	fclose(fp);	

	/* clipping	*/
	if (clip && (max > thermal_threshold)) {
		/* if max has to be brought down to thermal_threshold, 
		 * (w.r.t the ambient) what is the scale down factor?
		 */
		double factor = (thermal_threshold - ambient) / (max - ambient);
	
		/* scale down all temperature differences (from ambient) by the same factor	*/
		for (i=0; i < NL*flp->n_units + EXTRA; i++)
			temp[i] = (temp[i]-ambient)*factor + ambient;
	}
}

/* dump power numbers to file	*/
void dump_power(RC_model_t *model, double *power, char *file)
{
	flp_t *flp = model->flp;
	int i;
	char str[STR_SIZE];
	FILE *fp = fopen (file, "w");
	if (!fp) {
		sprintf (str,"error: %s could not be opened for writing\n", file);
		fatal(str);
	}
	for (i=0; i < flp->n_units; i++)
		fprintf(fp, "%s\t%.1f\n", flp->units[i].name, power[i]);
	fclose(fp);	
}

/* 
 * read power vector alloced using 'hotspot_vector' from 'file'
 * which was dumped using 'dump_power'. 
 */ 
void read_power (RC_model_t *model, double *power, char *file)
{
	flp_t *flp = model->flp;
	int i=0, idx;
	double val;
	char str[STR_SIZE], name[STR_SIZE];
	FILE *fp = fopen (file, "r");
	if (!fp) {
		sprintf (str,"error: %s could not be opened for reading\n", file);
		fatal(str);
	}	
	while(TRUE) {
		fgets(str, STR_SIZE, fp);
		if (feof(fp))
			break;
		if (sscanf(str, "%s%lf", name, &val) != 2)
			fatal("invalid power file format\n");
		idx = get_blk_index(flp, name);
		if (idx >= 0)
			power[idx] = val;
		else	/* since get_blk_index calls fatal, the line below cannot be reached	*/
			fatal ("unit in power file not found in floorplan\n");
		i++;	
	}
	fclose(fp);
}

double find_max_temp(RC_model_t *model, double *temp)
{
	int i;
	double max = 0.0;
	for(i=0; i < model->n_units; i++) {
		if (temp[i] < 0)
			fatal("negative temperature!");
		else if (max < temp[i])
			max = temp[i];
	}

	return max;
}

void delete_RC_model(RC_model_t *model)
{
	free_matrix(model->b, model->n_nodes);
	free_matrix(model->invb, model->n_nodes);
	free_matrix(model->inva, model->n_nodes);
	free_matrix(model->c, model->n_nodes);

	free_vector(model->gx);
	free_vector(model->gy);
	free_vector(model->c_ver);
	free_vector(model->gx_sp);
	free_vector(model->gy_sp);
	free_vector(model->t_vector);

	free_matrix(model->len, model->n_units);
	free_matrix(model->g, model->n_nodes);
	free_matrix(model->t_matrix, model->n_nodes);

	free_imatrix(model->border, model->n_units);

	free(model);
}
