#include "flp.h"
#include "util.h"
#include "temperature.h"

#include <stdio.h>
#include <string.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

/* default flp_config	*/
flp_config_t default_flp_config(void)
{
	flp_config_t config;

	/* wrap around L2?	*/
	config.wrap_l2 = TRUE;
	strcpy(config.l2_label, "L2");
	
	/* model dead space around the rim of the chip? */
	config.model_rim = TRUE;
	config.rim_thickness = 50e-6;

	/* area ratio below which to ignore dead space	*/
	config.compact_ratio = 0.005;

	/* 
	 * no. of discrete orientations for a shape curve.
	 * should be an even number greater than 1
	 */
	config.n_orients = 300;
	
	/* annealing parameters	*/
	config.P0 = 0.99;		/* initial acceptance probability	*/
	/* 
	 * average change (delta) in cost. varies according to
	 * the metric. need not be very accurate. just the right
	 * order of magnitude is enough. for instance, if the
	 * metric is flp area, this Davg is the average difference
	 * in the area of successive slicing floorplan attempts.
	 * since the areas are in the order of mm^2, the delta
	 * is also in the same ball park. 1 mm^2 = 1e-06 m^2. 
	 */
	config.Davg = 0.001;	/* for a temperature * area metric	*/
	config.Kmoves = 7;		/* no. of moves to try in each step	*/
	config.Rcool = 0.99;	/* ratio for the cooling schedule */
	config.Rreject = 0.99;	/* ratio of rejects at which to stop annealing */
	config.Nmax = 1000;		/* absolute max no. of annealing steps	*/

	/* weights for the metric: A + lambdaT * T + lambdaW * W	*/
	config.lambdaT = 1.0e-3;
	config.lambdaW = 9.2e-7;

	return config;
}

/* 
 * parse a table of name-value string pairs and add the configuration
 * parameters to 'config'
 */
void flp_config_add_from_strs(flp_config_t *config, str_pair *table, int size)
{
	int idx;
	if ((idx = get_str_index(table, size, "wrap_l2")) >= 0)
		if(sscanf(table[idx].value, "%d", &config->wrap_l2) != 1)
			fatal("invalid format for configuration  parameter wrap_l2");
	if ((idx = get_str_index(table, size, "l2_label")) >= 0)
		if(sscanf(table[idx].value, "%s", config->l2_label) != 1)
			fatal("invalid format for configuration  parameter l2_label");
	if ((idx = get_str_index(table, size, "model_rim")) >= 0)
		if(sscanf(table[idx].value, "%d", &config->model_rim) != 1)
			fatal("invalid format for configuration  parameter model_rim");
	if ((idx = get_str_index(table, size, "rim_thickness")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->rim_thickness) != 1)
			fatal("invalid format for configuration  parameter rim_thickness");
	if ((idx = get_str_index(table, size, "compact_ratio")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->compact_ratio) != 1)
			fatal("invalid format for configuration  parameter compact_ratio");
	if ((idx = get_str_index(table, size, "n_orients")) >= 0)
		if(sscanf(table[idx].value, "%d", &config->n_orients) != 1)
			fatal("invalid format for configuration  parameter n_orients");
	if ((idx = get_str_index(table, size, "P0")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->P0) != 1)
			fatal("invalid format for configuration  parameter P0");
	if ((idx = get_str_index(table, size, "Davg")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->Davg) != 1)
			fatal("invalid format for configuration  parameter Davg");
	if ((idx = get_str_index(table, size, "Kmoves")) >= 0)
		if(sscanf(table[idx].value, "%d", &config->Kmoves) != 1)
			fatal("invalid format for configuration  parameter Kmoves");
	if ((idx = get_str_index(table, size, "Rcool")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->Rcool) != 1)
			fatal("invalid format for configuration  parameter Rcool");
	if ((idx = get_str_index(table, size, "Rreject")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->Rreject) != 1)
			fatal("invalid format for configuration  parameter Rreject");
	if ((idx = get_str_index(table, size, "Nmax")) >= 0)
		if(sscanf(table[idx].value, "%d", &config->Nmax) != 1)
			fatal("invalid format for configuration  parameter Nmax");
	if ((idx = get_str_index(table, size, "lambdaT")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->lambdaT) != 1)
			fatal("invalid format for configuration  parameter lambdaT");
	if ((idx = get_str_index(table, size, "lambdaW")) >= 0)
		if(sscanf(table[idx].value, "%lf", &config->lambdaW) != 1)
			fatal("invalid format for configuration  parameter lambdaW");
			
	if (config->rim_thickness <= 0)
		fatal("rim thickness should be greater than zero\n");
	if ((config->compact_ratio < 0) || (config->compact_ratio > 1))
		fatal("compact_ratio should be between 0 and 1\n");
	if ((config->n_orients <= 1) || (config->n_orients & 1))
		fatal("n_orients should be an even number greater than 1\n");
	if (config->Kmoves < 0)
		fatal("Kmoves should be non-negative\n");
	if ((config->P0 < 0) || (config->P0 > 1))
		fatal("P0 should be between 0 and 1\n");
	if ((config->Rcool < 0) || (config->Rcool > 1))
		fatal("Rcool should be between 0 and 1\n");
	if ((config->Rreject < 0) || (config->Rreject > 1))
		fatal("Rreject should be between 0 and 1\n");
	if (config->Nmax < 0)
		fatal("Nmax should be non-negative\n");
}

/* 
 * convert config into a table of name-value pairs. returns the no.
 * of parameters converted
 */
int flp_config_to_strs(flp_config_t *config, str_pair *table, int max_entries)
{
	if (max_entries < 14)
		fatal("not enough entries in table\n");

	sprintf(table[0].name, "wrap_l2");
	sprintf(table[1].name, "l2_label");
	sprintf(table[2].name, "model_rim");
	sprintf(table[3].name, "rim_thickness");
	sprintf(table[4].name, "compact_ratio");
	sprintf(table[5].name, "n_orients");
	sprintf(table[6].name, "P0");
	sprintf(table[7].name, "Davg");
	sprintf(table[8].name, "Kmoves");
	sprintf(table[9].name, "Rcool");
	sprintf(table[10].name, "Rreject");
	sprintf(table[11].name, "Nmax");
	sprintf(table[12].name, "lambdaT");
	sprintf(table[13].name, "lambdaW");

	sprintf(table[0].value, "%d", config->wrap_l2);
	sprintf(table[1].value, "%s", config->l2_label);
	sprintf(table[2].value, "%d", config->model_rim);
	sprintf(table[3].value, "%lg", config->rim_thickness);
	sprintf(table[4].value, "%lg", config->compact_ratio);
	sprintf(table[5].value, "%d", config->n_orients);
	sprintf(table[6].value, "%lg", config->P0);
	sprintf(table[7].value, "%lg", config->Davg);
	sprintf(table[8].value, "%d", config->Kmoves);
	sprintf(table[9].value, "%lg", config->Rcool);
	sprintf(table[10].value, "%lg", config->Rreject);
	sprintf(table[11].value, "%d", config->Nmax);
	sprintf(table[12].value, "%lg", config->lambdaT);
	sprintf(table[13].value, "%lg", config->lambdaW);

	return 14;
}

/* 
 * copy L2 connectivity from 'from' of flp_desc to 'to' 
 * of flp. 'size' elements are copied. the arms are not 
 * connected amidst themselves or with L2 base block
 */
void copy_l2_info (flp_t *flp, int to, flp_desc_t *flp_desc, int from, int size)
{
	int j, count;

	for(count=0; count < L2_ARMS + 1; count++, to++) {
		/* copy names */
		strcpy(flp->units[to].name, flp_desc->units[from].name);
		for(j=0; j < size; j++) {
			/* rows	*/
			flp->wire_density[to][j] = flp_desc->wire_density[from][j];
			/* columns	*/	
			flp->wire_density[j][to] = flp_desc->wire_density[j][from];
		}
	}
	/* fix the names of the arms	*/
	strcat(flp->units[to-L2_ARMS+L2_LEFT].name, L2_LEFT_STR);	
	strcat(flp->units[to-L2_ARMS+L2_RIGHT].name, L2_RIGHT_STR);
}

/* initialize rim names	*/
void set_rim_names (flp_t *flp, int at)
{
	strcpy(flp->units[at + RIM_LEFT].name, RIM_LEFT_STR);
	strcpy(flp->units[at + RIM_RIGHT].name, RIM_RIGHT_STR);
	strcpy(flp->units[at + RIM_TOP].name, RIM_TOP_STR);
	strcpy(flp->units[at + RIM_BOTTOM].name, RIM_BOTTOM_STR);
}

/* create a floorplan placeholder from description	*/
flp_t *flp_placeholder(flp_desc_t *flp_desc)
{
	int i, j, count, n_dead;
	flp_t *flp;

	/* wrap L2 around?	*/
	int wrap_l2 = FALSE;
	if (flp_desc->config.wrap_l2 && 
		!strcasecmp(flp_desc->units[flp_desc->n_units-1].name, flp_desc->config.l2_label))
		wrap_l2 = TRUE;

	flp = (flp_t *) calloc (1, sizeof(flp_t));
	if(!flp)
		fatal("memory allocation error\n");
	/* 
	 * number of dead blocks = no. of core blocks - 1.
	 * (one per vertical or horizontal cut). if L2 is 
	 * wrapped around, core blocks = flp_desc->n_units-1
	 */
	n_dead = flp_desc->n_units - !!(wrap_l2) - 1; 
	flp->n_units = flp_desc->n_units + n_dead;

	/* wrap L2 around - extra arms are added */
	if (wrap_l2)
		flp->n_units += L2_ARMS;

	/* model the dead space in the edge	*/
	if (flp_desc->config.model_rim)
		flp->n_units += RIM_BLOCKS;

	flp->units = (unit_t *) calloc (flp->n_units, sizeof(unit_t));
	flp->wire_density = (double **) calloc(flp->n_units, sizeof(double *));
	if (!flp->units || !flp->wire_density)
		fatal("memory allocation error\n");
	for (i=0; i < flp->n_units; i++) {
	  flp->wire_density[i] = (double *) calloc(flp->n_units, sizeof(double));
	  if (!flp->wire_density[i])
	  	fatal("memory allocation error\n");
	}

	/* copy connectivity (only for non-dead core blocks) */
	for(i=0; i < flp_desc->n_units-!!(wrap_l2); i++) {
	  strcpy(flp->units[i].name, flp_desc->units[i].name);
	  for (j=0; j < flp_desc->n_units-!!(wrap_l2); j++) {
	  	flp->wire_density[i][j] = flp_desc->wire_density[i][j];
	  }
	}

	/* name the dead blocks	*/
	for(count=0; count < n_dead; count++, i++)
		sprintf(flp->units[i].name, DEAD_PREFIX"%d", count);

	/* L2 connectivity info	*/
	if (wrap_l2)
		copy_l2_info(flp, i, flp_desc, flp_desc->n_units-1, flp_desc->n_units-1);

	/* rim names 	*/
	if (flp_desc->config.model_rim)
		/* go past L2 only if present	*/
		set_rim_names(flp, i+(L2_ARMS+1)*(!!wrap_l2));

	return flp;
}

/* 
 * note that if wrap_l2 is true, L2 is beyond the boundary in flp_desc 
 * but flp contains it within its boundaries.
 */
void restore_dead_blocks(flp_t *flp, flp_desc_t *flp_desc, 
						 int compacted, int wrap_l2, 
						 int model_rim)
{
	int i, j, idx=0;
	/* remove L2 and rim blocks and restore the compacted blocks */
	if (wrap_l2)
		flp->n_units -= (L2_ARMS+1);
	if(model_rim)
		flp->n_units -= RIM_BLOCKS;
	flp->n_units += compacted;

	/* reinitialize the dead blocks	*/
	for(i=0; i < flp_desc->n_units-1; i++) {
		idx = flp_desc->n_units + i;
		sprintf(flp->units[idx].name, DEAD_PREFIX"%d", i);
		flp->units[idx].leftx = flp->units[idx].bottomy = 0;
		flp->units[idx].width = flp->units[idx].height = 0;
		for(j=0; j < flp->n_units; j++)
			flp->wire_density[idx][j] = flp->wire_density[j][idx] = 0;
	}

	/* re-add L2 from flp_desc from beyond the boundary	*/
	if (wrap_l2) {
		flp->n_units += (L2_ARMS+1);
		copy_l2_info(flp, idx+1, flp_desc, 
					 flp_desc->n_units, flp_desc->n_units);
	}

	/* re-add the rim blocks	*/
	if(model_rim) {
		flp->n_units += RIM_BLOCKS;
		/* go past L2 only if present	*/
		set_rim_names(flp, idx+1+(L2_ARMS+1)*(!!wrap_l2)); 
	}
}

/* translate the floorplan to new origin (x,y)	*/
void flp_translate(flp_t *flp, double x, double y)
{
	int i;
	for (i=0; i < flp->n_units; i++) {
		flp->units[i].leftx += x;
		flp->units[i].bottomy += y;
	}
}

/* 
 * wrap the L2 around this floorplan. L2's area information 
 * is obtained from flp_desc. memory for L2 and its arms has
 * already been allocated in the flp. note that flp & flp_desc 
 * have L2 hidden beyond the boundary at this point
 */
void flp_wrap_l2(flp_t *flp, flp_desc_t *flp_desc)
{
	/* 
	 * x is the width of the L2 arms
	 * y is the height of the bottom portion
	 */
	double x, y, core_width, core_height, total_side, core_area, l2_area;
	unit_t *l2, *l2_left, *l2_right;

	/* find L2 dimensions so that the total chip becomes a square	*/
	core_area = get_total_area(flp);
	core_width = get_total_width(flp);
	core_height = get_total_height(flp);
	/* flp_desc has L2 hidden beyond the boundary	*/
	l2_area = flp_desc->units[flp_desc->n_units].area;
	total_side = sqrt(core_area + l2_area);
	/* 
	 * width of the total chip after L2 wrapping is equal to 
	 * the width of the core plus the width of the two arms
	 */
	x = (total_side - core_width) / 2.0;
	y = total_side - core_height;
	/* 
	 * we are trying to solve the equation 
	 * (2*x+core_width) * (y+core_height) 
	 * = l2_area + core_area
	 * for x and y. it is possible that the values 
	 * turnout to be negative if we restrict the
	 * total chip to be a square. in that case,
	 * theoretically, any value of x in the range
	 * (0, l2_area/(2*core_height)) and the 
	 * corresponding value of y or any value of y
	 * in the range (0, l2_area/core_width) and the
	 * corresponding value of x would be a solution
	 * we look for a solution with a reasonable 
	 * aspect ratio. i.e., we constrain kx = y (or
	 * ky = x  depending on the aspect ratio of the 
	 * core) where k = WRAP_L2_RATIO. solving the equation 
	 * with this constraint, we get the following
	 */
	if ( x <= 0 || y <= 0.0) {
		double sum;
		if (core_width >= core_height) {
			sum = WRAP_L2_RATIO * core_width + 2 * core_height;
			x = (sqrt(sum*sum + 8*WRAP_L2_RATIO*l2_area) - sum) / (4*WRAP_L2_RATIO);
			y = WRAP_L2_RATIO * x;
		} else {
			sum = core_width + 2 * WRAP_L2_RATIO * core_height;
			y = (sqrt(sum*sum + 8*WRAP_L2_RATIO*l2_area) - sum) / (4*WRAP_L2_RATIO);
			x = WRAP_L2_RATIO * y;
		}
		total_side = 2 * x + core_width;
	}
	
	/* fix the positions of core blocks	*/
	flp_translate(flp, x, y);

	/* restore the L2 blocks	*/
	flp->n_units += (L2_ARMS+1);
	/* copy L2 info again from flp_desc but from beyond the boundary	*/
	copy_l2_info(flp, flp->n_units-L2_ARMS-1, flp_desc, 
				 flp_desc->n_units, flp_desc->n_units);

	/* fix the positions of the L2  blocks. connectivity
	 * information has already been fixed (in flp_placeholder).
	 * bottom L2 block - (leftx, bottomy) is already (0,0)
	 */
	l2 = &flp->units[flp->n_units-1-L2_ARMS];
	l2->width = total_side;
	l2->height = y;
	l2->leftx = l2->bottomy = 0;

	/* left L2 arm */
	l2_left = &flp->units[flp->n_units-L2_ARMS+L2_LEFT];
	l2_left->width = x;
	l2_left->height = core_height;
	l2_left->leftx = 0;
	l2_left->bottomy = y;

	/* right L2 arm */
	l2_right = &flp->units[flp->n_units-L2_ARMS+L2_RIGHT];
	l2_right->width = x;
	l2_right->height = core_height;
	l2_right->leftx = x + core_width;
	l2_right->bottomy = y;
}

/*
 * left and right rim strips extend beyond the floorplan 
 * boundary while top and bottom are bounded by the left
 * and right strips
 */
void flp_wrap_rim(flp_t *flp, double rim_thickness)
{
	double width = get_total_width(flp);
	double height = get_total_height(flp);
	int i = flp->n_units;

	flp_translate(flp, rim_thickness, rim_thickness);
	set_rim_names(flp, i);

	flp->units[i+RIM_LEFT].width = rim_thickness;
	flp->units[i+RIM_LEFT].height = height + 2*rim_thickness;
	flp->units[i+RIM_LEFT].leftx = 0;
	flp->units[i+RIM_LEFT].bottomy = 0;

	flp->units[i+RIM_RIGHT].width = rim_thickness;
	flp->units[i+RIM_RIGHT].height = height + 2*rim_thickness;
	flp->units[i+RIM_RIGHT].leftx = width + rim_thickness;
	flp->units[i+RIM_RIGHT].bottomy = 0;

	flp->units[i+RIM_TOP].width = width;
	flp->units[i+RIM_TOP].height = rim_thickness;
	flp->units[i+RIM_TOP].leftx = rim_thickness;
	flp->units[i+RIM_TOP].bottomy = height + rim_thickness;

	flp->units[i+RIM_BOTTOM].width = width;
	flp->units[i+RIM_BOTTOM].height = rim_thickness;
	flp->units[i+RIM_BOTTOM].leftx = rim_thickness;
	flp->units[i+RIM_BOTTOM].bottomy = 0;

	flp->n_units += RIM_BLOCKS;
}

/* 
 * floorplanning using simulated annealing.
 * precondition: flp is a pre-allocated placeholder
 * returns the number of compacted blocks in the selected
 * floorplan
 */
int floorplan(flp_t *flp, flp_desc_t *flp_desc, 
			  struct RC_model_t_st *model, double *power)
{
	NPE_t *expr, *next, *best;	/* Normalized Polish Expressions */
	tree_node_stack_t *stack;	/* for NPE evaluation	*/
	tree_node_t *root;			/* shape curve tree	*/
	double cost, new_cost, best_cost, T, Tcold;
	int i, steps, downs, n, rejects, compacted;
	int total_accepts = 0, total_rejects = 0;

	/* shortcut	*/
	flp_config_t cfg = flp_desc->config;

	/* wrap L2 around?	*/
	int wrap_l2 = FALSE;
	if (cfg.wrap_l2 && 
		!strcasecmp(flp_desc->units[flp_desc->n_units-1].name, cfg.l2_label)) {
		wrap_l2 = TRUE;
		/* 
		 * make L2 disappear for slicing tree purposes. 
		 * can be restored at the end
		 */
		flp_desc->n_units--;
		flp->n_units -= (L2_ARMS+1);
	}

	/* make the rim strips disappear too	*/
	if (cfg.model_rim)
		flp->n_units -= RIM_BLOCKS;

	/* initialization	*/
	expr = NPE_get_initial(flp_desc);
	stack = new_tree_node_stack();
	init_rand();
	
	/* convert NPE to flp	*/
	root = tree_from_NPE(flp_desc, stack, expr);
	/* compacts too small dead blocks	*/
	compacted = tree_to_flp(root, flp, TRUE, cfg.compact_ratio);
	free_tree(root);
	if(wrap_l2)
		flp_wrap_l2(flp, flp_desc);
	if(cfg.model_rim)
		flp_wrap_rim(flp, cfg.rim_thickness);

	cost = flp_evaluate_metric(flp, model, power, cfg.lambdaT, cfg.lambdaW);
	/* restore compacted dead blocks	*/
	restore_dead_blocks(flp, flp_desc, compacted, wrap_l2, cfg.model_rim);
	/* hide L2 and rim	*/
	if (wrap_l2)
		flp->n_units -= (L2_ARMS+1);
	if(cfg.model_rim)
		flp->n_units -= RIM_BLOCKS;
	best = NPE_duplicate(expr);	/* best till now	*/
	best_cost = cost;

	/* simulated annealing	*/
	steps = 0;
	/* initial annealing temperature	*/
	T = -cfg.Davg / log(cfg.P0);
	/* 
	 * final annealing temperature - we stop when there
	 * are fewer than (1-cfg.Rreject) accepts.
	 * of those accepts, assuming half are uphill moves,
	 * we want the temperature so that the probability
	 * of accepting uphill moves is as low as
	 * (1-cfg.Rreject)/2.
	 */
	Tcold = -cfg.Davg / log ((1.0 - cfg.Rreject) / 2.0);
	#if VERBOSE > 0
	fprintf(stdout, "initial cost: %g\tinitial T: %g\tfinal T: %g\n", cost, T, Tcold);
	#endif
	/* 
	 * stop annealing if temperature has cooled down enough or
	 * max no. of iterations have been tried
	 */
	while (T >= Tcold && steps < cfg.Nmax) {
		/* shortcut	*/
		n = cfg.Kmoves * flp->n_units; 
		i = downs = rejects = 0;
		/* try enough total or downhill moves per T */
		while ((i < 2 * n) && (downs < n)) {
			next = make_random_move(expr);

			/* convert NPE to flp	*/
			root = tree_from_NPE(flp_desc, stack, next);
			compacted = tree_to_flp(root, flp, TRUE, cfg.compact_ratio);
			free_tree(root);
			if(wrap_l2)
				flp_wrap_l2(flp, flp_desc);
			if(cfg.model_rim)
				flp_wrap_rim(flp, cfg.rim_thickness);

			new_cost = flp_evaluate_metric(flp, model, power, cfg.lambdaT, cfg.lambdaW);
			restore_dead_blocks(flp, flp_desc, compacted, wrap_l2, cfg.model_rim);
			/* hide L2 and rim	*/
			if (wrap_l2)
				flp->n_units -= (L2_ARMS+1);
			if(cfg.model_rim)
				flp->n_units -= RIM_BLOCKS;
			#if VERBOSE > 1
			fprintf(stdout, "count: %d\tdowns: %d\tcost: %g\t", 
					i, downs, new_cost);
			#endif

			/* move accepted?	*/
			if (new_cost < cost || 	/* downhill always accepted	*/
				/* boltzmann probability function	*/
			    rand_fraction() < exp(-(new_cost-cost)/T)) {

				free_NPE(expr);
				expr = next;

				/* downhill move	*/
				if (new_cost < cost) {
					downs++;
					/* found new best	*/
					if (new_cost < best_cost) {
						free_NPE(best);
						best = NPE_duplicate(expr);
						best_cost = new_cost;
					}
				}
				total_accepts++;

				#if VERBOSE > 1
				fprintf(stdout, "accepted\n");
				#endif
				cost = new_cost;
			} else {	/* rejected move	*/
				rejects++;
				total_rejects++;
				free_NPE(next);
				#if VERBOSE > 1
				fprintf(stdout, "rejected\n");
				#endif
			}
			i++;
		}
		#if VERBOSE > 0
		fprintf(stdout, "step: %d\tT: %g\ttries: %d\taccepts: %d\trejects: %d\t", 
				steps, T, i, (i-rejects), rejects);
		fprintf(stdout, "final cost: %g\ttotal accepts: %d\ttotal rejects: %d\t", 
				cost, total_accepts, total_rejects);
		fprintf(stdout, "best cost: %g\n", best_cost); 
		#endif

		/* stop annealing if there are too little accepts */
		if(((double)rejects/i) > cfg.Rreject)
			break;

		/* annealing schedule	*/
		T *= cfg.Rcool;
		steps++;	
	}

	/* best floorplan found	*/
	root = tree_from_NPE(flp_desc, stack, best);
	#if VERBOSE > 0
	{
		int pos = min_area_pos(root->curve);
		print_tree_relevant(root, pos, flp_desc);
	}	
	#endif
	compacted = tree_to_flp(root, flp, TRUE, cfg.compact_ratio);
	free_tree(root);
	/*  restore L2 and rim */
	if(wrap_l2) {
		flp_wrap_l2(flp, flp_desc);
		flp_desc->n_units++;
	}
	if(cfg.model_rim)
		flp_wrap_rim(flp, cfg.rim_thickness);

	free_NPE(expr);
	free_NPE(best);
	free_tree_node_stack(stack);

	/* 
	 * return the number of blocks compacted finally
	 * so that any deallocator can take care of memory
	 * accordingly
	 */
	return compacted; 
}

void print_flp_fig (flp_t *flp)
{
	int i;
	double leftx, bottomy, rightx, topy;

	fprintf(stdout, "FIG starts\n");
	for (i=0; i< flp->n_units; i++) {
		leftx = flp->units[i].leftx;
		bottomy = flp->units[i].bottomy;
		rightx = flp->units[i].leftx + flp->units[i].width;
		topy = flp->units[i].bottomy + flp->units[i].height;
		fprintf(stdout, "%.5f %.5f %.5f %.5f %.5f %.5f %.5f %.5f %.5f %.5f\n", 
			    leftx, bottomy, leftx, topy, rightx, topy, rightx, bottomy, 
				leftx, bottomy);
		fprintf(stdout, "%s\n", flp->units[i].name);
	}
	fprintf(stdout, "FIG ends\n");
}

/* functions duplicated from flp_desc.c */
/* 
 * find the number of units from the 
 * floorplan file
 */
int flp_count_units(FILE *fp)
{
    char str1[STR_SIZE], str2[STR_SIZE];
	char name[STR_SIZE];
	double leftx, bottomy, width, height;
	char *ptr;
    int count = 0;

	fseek(fp, 0, SEEK_SET);
	while(!feof(fp)) {
		fgets(str1, STR_SIZE, fp);
		if (feof(fp))
			break;
		strcpy(str2, str1);
		
		ptr = strtok(str1, " \t\n");
		if (ptr && ptr[0] != '#') {	/* ignore comments and empty lines	*/
		 /* functional block placement information	*/
		  if (sscanf(str2, "%s%lf%lf%lf%lf", name, &leftx, &bottomy,
		  			 &width, &height) == 5)
			count++;
		}	
	}
	return count;
}

flp_t *flp_alloc_init_mem(int count)
{
	int i;
	flp_t *flp;
	flp = (flp_t *) calloc (1, sizeof(flp_t));
	if(!flp)
		fatal("memory allocation error\n");
	flp->units = (unit_t *) calloc(count, sizeof(unit_t));
	flp->wire_density = (double **) calloc(count, sizeof(double *));
	if (!flp->units || !flp->wire_density)
		fatal("memory allocation error\n");
	flp->n_units = count;

	for (i=0; i < count; i++) {
	  flp->wire_density[i] = (double *) calloc(count, sizeof(double));
	  if (!flp->wire_density[i])
	  	fatal("memory allocation error\n");
	}
	return flp;
}

/* populate block information	*/
void flp_populate_blks(flp_t *flp, FILE *fp)
{
	int i=0;
	char str[STR_SIZE], copy[STR_SIZE]; 
	char name1[STR_SIZE], name2[STR_SIZE];
	double width, height, leftx, bottomy;
	double wire_density;
	char *ptr;

	fseek(fp, 0, SEEK_SET);
	while(!feof(fp)) {		/* second pass	*/
		fgets(str, STR_SIZE, fp);
		if (feof(fp))
			break;
		strcpy(copy, str);
		ptr = strtok(str, " \t\n");
		if (ptr && ptr[0] != '#') {	/* ignore comments and empty lines	*/
			if (sscanf(copy, "%s%lf%lf%lf%lf", name1, &width, &height, 
				&leftx, &bottomy) == 5) {
				strcpy(flp->units[i].name, name1);
				flp->units[i].width = width;
				flp->units[i].height = height;
				flp->units[i].leftx = leftx;
				flp->units[i].bottomy = bottomy;
				i++;
				/* skip connectivity info	*/
			} else if (sscanf(copy, "%s%s%lf", name1, name2, &wire_density) != 3) 
				fatal("invalid floorplan file format\n");
		}
	}
	if (i != flp->n_units)
	  fatal("mismatch of number of units\n");
}

/* populate connectivity info	*/
void flp_populate_connects(flp_t *flp, FILE *fp)
{
	char str1[STR_SIZE], str2[STR_SIZE]; 
	char name1[STR_SIZE], name2[STR_SIZE];
	/* dummy fields	*/
	double f1, f2, f3, f4, f5, f6;
	double wire_density;
	char *ptr;

	fseek(fp, 0, SEEK_SET);
	while(!feof(fp)) {
		fgets(str1, STR_SIZE, fp);
		if (feof(fp))
			break;
		strcpy(str2, str1);

		ptr = strtok(str1, " \t\n");
		if (ptr && ptr[0] != '#') {	/* ignore comments and empty lines	*/
		 /* lines with unit positions	*/
		  if (sscanf(str2, "%s%lf%lf%lf%lf%lf%lf", name1, &f1, &f2, &f3, &f4, &f5, &f6) == 7)
		  	continue;
		 /* lines with connectivity info	*/
		  else if (sscanf(str2, "%s%s%lf", name1, name2, &wire_density) == 3) {
		    int x, y;
			x = get_blk_index(flp, name1);
			y = get_blk_index(flp, name2);

			if (x == y)
				fatal("block connected to itself?\n");

			if (!flp->wire_density[x][y] && !flp->wire_density[y][x])
				flp->wire_density[x][y] = flp->wire_density[y][x] = wire_density;
			else if((flp->wire_density[x][y] != flp->wire_density[y][x]) ||
			        (flp->wire_density[x][y] != wire_density)) {
				sprintf(str2, "wrong connectivity information for blocks %s and %s\n", 
				        name1, name2);
				fatal(str2);
			}
		  } else 
		  	fatal("invalid floorplan file format\n");
		} /* end if */
	} /* end while	*/
}

flp_t *read_flp(char *file, int read_connects)
{
	char str[STR_SIZE];
	FILE *fp = fopen(file, "r");
	flp_t *flp;
	int count, i, j;

	if (!fp) {
		sprintf(str, "error opening file %s\n", file);
		fatal(str);
	}

	/* 1st pass - find n_units	*/
	count = flp_count_units(fp);
	if(!count)
		fatal("no units specified in the floorplan file\n");

	/* allocate initial memory */
	flp = flp_alloc_init_mem(count);

	/* 2nd pass - populate block info	*/
	flp_populate_blks(flp, fp);

	/* 3rd pass - populate connectivity info    */
	if (read_connects)
		flp_populate_connects(flp, fp);
	/* older version - no connectivity	*/	
	else for (i=0; i < flp->n_units; i++)
			for (j=0; j < flp->n_units; j++)
				flp->wire_density[i][j] = 1.0;

	fclose(fp);	
	print_flp_fig(flp);
	return flp;
}

void dump_flp(flp_t *flp, char *file, int dump_connects)
{
	char str[STR_SIZE];
	int i, j;
	FILE *fp = fopen(file, "w");
	if (!fp) {
		sprintf(str, "error opening file %s\n", file);
		fatal(str);
	}
	/* functional unit placement info	*/
	for(i=0; i < flp->n_units; i++)  {
		fprintf(fp, "%s\t%.10f\t%.10f\t%.10f\t%.10f\n",
				flp->units[i].name, flp->units[i].width, flp->units[i].height,
				flp->units[i].leftx, flp->units[i].bottomy);
	}

	if (dump_connects) {
		fprintf(fp, "\n");
		/* connectivity information	*/
		for(i=1; i < flp->n_units; i++)
			for(j=0; j < i; j++)
				if (flp->wire_density[i][j])
					fprintf(fp, "%s\t%s\t%.3f\n", flp->units[i].name,
							flp->units[j].name, flp->wire_density[i][j]);
	}
			
	fclose(fp);
}

void free_flp(flp_t *flp, int compacted)
{
	int i;
	for (i=0; i < flp->n_units + compacted; i++) {
		free(flp->wire_density[i]);
	}
	free(flp->units);
	free(flp->wire_density);
	free(flp);
}

int get_blk_index(flp_t *flp, char *name)
{
	int i;
	char msg[STR_SIZE];

	if (!flp)
		fatal("null pointer in get_blk_index\n");

	for (i = 0; i < flp->n_units; i++) {
		if (!strcasecmp(name, flp->units[i].name)) {
			return i;
		}
	}

	sprintf(msg, "block %s not found\n", name);
	fatal(msg);
	return -1;
}

int is_horiz_adj(flp_t *flp, int i, int j)
{
	double x1, x2, x3, x4;
	double y1, y2, y3, y4;

	if (i == j) 
		return FALSE;

	x1 = flp->units[i].leftx;
	x2 = x1 + flp->units[i].width;
	x3 = flp->units[j].leftx;
	x4 = x3 + flp->units[j].width;

	y1 = flp->units[i].bottomy;
	y2 = y1 + flp->units[i].height;
	y3 = flp->units[j].bottomy;
	y4 = y3 + flp->units[j].height;

	/* diagonally adjacent => not adjacent */
	if (eq(x2,x3) && eq(y2,y3))
		return FALSE;
	if (eq(x1,x4) && eq(y1,y4))
		return FALSE;
	if (eq(x2,x3) && eq(y1,y4))
		return FALSE;
	if (eq(x1,x4) && eq(y2,y3))
		return FALSE;

	if (eq(x1,x4) || eq(x2,x3))
		if ((y3 >= y1 && y3 <= y2) || (y4 >= y1 && y4 <= y2) ||
		    (y1 >= y3 && y1 <= y4) || (y2 >= y3 && y2 <= y4))
			return TRUE;

	return FALSE;
}

int is_vert_adj (flp_t *flp, int i, int j)
{
	double x1, x2, x3, x4;
	double y1, y2, y3, y4;

	if (i == j)
		return FALSE;

	x1 = flp->units[i].leftx;
	x2 = x1 + flp->units[i].width;
	x3 = flp->units[j].leftx;
	x4 = x3 + flp->units[j].width;

	y1 = flp->units[i].bottomy;
	y2 = y1 + flp->units[i].height;
	y3 = flp->units[j].bottomy;
	y4 = y3 + flp->units[j].height;

	/* diagonally adjacent => not adjacent */
	if (eq(x2,x3) && eq(y2,y3))
		return FALSE;
	if (eq(x1,x4) && eq(y1,y4))
		return FALSE;
	if (eq(x2,x3) && eq(y1,y4))
		return FALSE;
	if (eq(x1,x4) && eq(y2,y3))
		return FALSE;

	if (eq(y1,y4) || eq(y2,y3))
		if ((x3 >= x1 && x3 <= x2) || (x4 >= x1 && x4 <= x2) ||
		    (x1 >= x3 && x1 <= x4) || (x2 >= x3 && x2 <= x4))
			return TRUE;

	return FALSE;
}

double get_shared_len(flp_t *flp, int i, int j)
{
	double p11, p12, p21, p22;
	p11 = p12 = p21 = p22 = 0.0;

	if (i==j) 
		return FALSE;

	if (is_horiz_adj(flp, i, j)) {
		p11 = flp->units[i].bottomy;
		p12 = p11 + flp->units[i].height;
		p21 = flp->units[j].bottomy;
		p22 = p21 + flp->units[j].height;
	}

	if (is_vert_adj(flp, i, j)) {
		p11 = flp->units[i].leftx;
		p12 = p11 + flp->units[i].width;
		p21 = flp->units[j].leftx;
		p22 = p21 + flp->units[j].width;
	}

	return (MIN(p12, p22) - MAX(p11, p21));
}

double get_total_width(flp_t *flp)
{	
	int i;
	double min_x = flp->units[0].leftx;
	double max_x = flp->units[0].leftx + flp->units[0].width;
	
	for (i=1; i < flp->n_units; i++) {
		if (flp->units[i].leftx < min_x)
			min_x = flp->units[i].leftx;
		if (flp->units[i].leftx + flp->units[i].width > max_x)
			max_x = flp->units[i].leftx + flp->units[i].width;
	}

	return (max_x - min_x);
}

double get_total_height(flp_t *flp)
{	
	int i;
	double min_y = flp->units[0].bottomy;
	double max_y = flp->units[0].bottomy + flp->units[0].height;
	
	for (i=1; i < flp->n_units; i++) {
		if (flp->units[i].bottomy < min_y)
			min_y = flp->units[i].bottomy;
		if (flp->units[i].bottomy + flp->units[i].height > max_y)
			max_y = flp->units[i].bottomy + flp->units[i].height;
	}

	return (max_y - min_y);
}

/* 
 * thermal model and power density are passed as temperature
 * is used as a metric. The metric considered is 
 * A + lambdaT * T  + lambdaW * W
 */
double flp_evaluate_metric(flp_t *flp, RC_model_t *model, double *power,
						   double lambdaT, double lambdaW)
{
	double tmax, area, wire_length;
	/* use the scratch pad vector in 'model' for computing temperature	*/
	populate_R_model(model, flp);
	steady_state_temp(model, power, model->t_vector);
	tmax = find_max_temp(model, model->t_vector);
	area = get_total_area(flp);
	wire_length = get_wire_metric(flp);
	return (area + lambdaT * tmax + lambdaW * wire_length);
}

double get_total_area(flp_t *flp)
{
	int i;
	double area = 0.0;
	for(i=0; i < flp->n_units; i++)
		area += flp->units[i].width * flp->units[i].height;
	return area;	
}

double get_occupied_area(flp_t *flp)
{
	int i, num;
	double dead_area = 0.0;
	for(i=0; i < flp->n_units; i++) {
		/* 
		 * there can be a max of n-1 dead blocks where n is the
		 * number of non-dead blocks (since each cut, vertical
		 * or horizontal, can correspond to a maximum of one
		 * dead block
		 */
		if ((sscanf(flp->units[i].name, DEAD_PREFIX"%d", &num) == 1) &&
			(num < (flp->n_units-1) / 2))
			dead_area += flp->units[i].width * flp->units[i].height;
	}		
	return get_total_area(flp) - dead_area;	
}

double get_wire_metric(flp_t *flp)
{
	int i, j;
	double w = 0.0, dist;

	for (i=0; i < flp->n_units; i++)
		for (j=0; j < flp->n_units; j++)
			if (flp->wire_density[i][j]) {
				dist = get_manhattan_dist(flp, i, j);
				w += flp->wire_density[i][j] * dist;
			}
	return w;		
}

double get_manhattan_dist(flp_t *flp, int i, int j)
{
	double x1 = flp->units[i].leftx + flp->units[i].width / 2.0;
	double y1 = flp->units[i].bottomy + flp->units[i].height / 2.0;
	double x2 = flp->units[j].leftx + flp->units[j].width / 2.0;
	double y2 = flp->units[j].bottomy + flp->units[j].height / 2.0;
	return (fabs(x2-x1) + fabs(y2-y1));
}
