/* 
 * This is a trace-level thermal simulator. It reads power values 
 * from an input trace file and outputs the corresponding instantaneous 
 * temperature values to an output trace file. It also outputs the steady 
 * state temperature values to stdout.
 */
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "flp.h"
#include "temperature.h"
#include "util.h"
#include "hotspot.h"

void usage(int argc, char **argv)
{
	fprintf(stdout, "Usage: %s -f <file> -p <file> -o <file> [-c <file>] [-d <file>] [options]\n", argv[0]);
	fprintf(stdout, "Options:(may be specified in any order, within \"[]\" means optional)\n");
	fprintf(stdout, "   -f <file>\tfloorplan input file\n");
	fprintf(stdout, "   -p <file>\tpower trace input file\n");
	fprintf(stdout, "   -o <file>\ttemperature trace output file\n");
	fprintf(stdout, "  [-c <file>]\tinput configuration parameters from file\n");
	fprintf(stdout, "  [-d <file>]\toutput configuration parameters to file\n");
	fprintf(stdout, "  [options]\tzero or more options of the form \"-<name> <value>\",\n");
	fprintf(stdout, "           \toverride the options from config file\n");
}

/* 
 * parse a table of name-value string pairs and add the configuration
 * parameters to 'config'
 */
void global_config_from_strs(global_config_t *config, str_pair *table, int size)
{
	int idx;
	if ((idx = get_str_index(table, size, "f")) >= 0) {
		if(sscanf(table[idx].value, "%s", config->flp_file) != 1)
			fatal("invalid format for configuration  parameter flp_file");
	} else {
		fatal("required parameter flp_file missing. check usage\n");
	}
	if ((idx = get_str_index(table, size, "p")) >= 0) {
		if(sscanf(table[idx].value, "%s", config->p_infile) != 1)
			fatal("invalid format for configuration  parameter p_infile");
	} else {
		fatal("required parameter p_infile missing. check usage\n");
	}
	if ((idx = get_str_index(table, size, "o")) >= 0) {
		if(sscanf(table[idx].value, "%s", config->t_outfile) != 1)
			fatal("invalid format for configuration  parameter t_outfile");
	} else {
		fatal("required parameter t_outfile missing. check usage\n");
	}
	if ((idx = get_str_index(table, size, "c")) >= 0) {
		if(sscanf(table[idx].value, "%s", config->config) != 1)
			fatal("invalid format for configuration  parameter config");
	} else {
		strcpy(config->config, NULLFILE);
	}
	if ((idx = get_str_index(table, size, "d")) >= 0) {
		if(sscanf(table[idx].value, "%s", config->dump_config) != 1)
			fatal("invalid format for configuration  parameter dump_config");
	} else {
		strcpy(config->dump_config, NULLFILE);
	}
}

/* 
 * convert config into a table of name-value pairs. returns the no.
 * of parameters converted
 */
int global_config_to_strs(global_config_t *config, str_pair *table, int max_entries)
{
	if (max_entries < 5)
		fatal("not enough entries in table\n");

	sprintf(table[0].name, "f");
	sprintf(table[1].name, "p");
	sprintf(table[2].name, "o");
	sprintf(table[3].name, "c");
	sprintf(table[4].name, "d");

	sprintf(table[0].value, "%s", config->flp_file);
	sprintf(table[1].value, "%s", config->p_infile);
	sprintf(table[2].value, "%s", config->t_outfile);
	sprintf(table[3].value, "%s", config->config);
	sprintf(table[4].value, "%s", config->dump_config);

	return 5;
}

/* 
 * read a single line of trace file containing names
 * of functional blocks
 */
int read_names(FILE *fp, char names[][STR_SIZE])
{
	char line[STR_SIZE], *src;
	int i;

	/* read the entire line	*/
	fgets(line, STR_SIZE, fp);
	if (feof(fp))
		fatal("not enough names in trace file\n");

	/* new line not read yet	*/	
	if(line[strlen(line)-1] != '\n')
		fatal("line too long\n");

	/* chop the names from the line read	*/
	for(i=0,src=line; *src && i < MAX_UNITS; i++) {
		if(!sscanf(src, "%s", names[i]))
			fatal("invalid format of names");
		src += strlen(names[i]);
		while (isspace((int)*src))
			src++;
	}
	if(*src && i == MAX_UNITS)
		fatal("no. of units exceeded limit");

	return i;
}

/* read a single line of power trace numbers	*/
int read_vals(FILE *fp, double vals[])
{
	char line[STR_SIZE], temp[STR_SIZE], *src;
	int i;

	/* read the entire line	*/
	fgets(line, STR_SIZE, fp);
	if (feof(fp))
		return 0;

	/* new line not read yet	*/	
	if(line[strlen(line)-1] != '\n')
		fatal("line too long\n");

	/* chop the power values from the line read	*/
	for(i=0,src=line; *src && i < MAX_UNITS; i++) {
		if(!sscanf(src, "%s", temp) || !sscanf(src, "%lf", &vals[i]))
			fatal("invalid format of values");
		src += strlen(temp);
		while (isspace((int)*src))
			src++;
	}
	if(*src && i == MAX_UNITS)
		fatal("no. of entries exceeded limit");

	return i;
}

/* write a single line of functional unit names	*/
void write_names(FILE *fp, char names[][STR_SIZE], int size)
{
	int i;
	for(i=0; i < size-1; i++)
		fprintf(fp, "%s\t", names[i]);
	fprintf(fp, "%s\n", names[i]);
}

/* write a single line of temperature trace(in degree C)	*/
void write_vals(FILE *fp, double vals[], int size)
{
	int i;
	for(i=0; i < size-1; i++)
		fprintf(fp, "%.2f\t", vals[i]-273.15);
	fprintf(fp, "%.2f\n", vals[i]-273.15);
}

/* 
 * main function - reads instantaneous power values (in W) from a trace
 * file (eg: "gcc.p") and outputs instantaneous temperature values (in C) to
 * a trace file("gcc.t"). also outputs steady state temperature values
 * (including those of the internal nodes of the model) onto stdout. the
 * trace files are 2-d matrices with each column representing a functional
 * functional block and each row representing a time unit(sampling_intvl).
 * columns are tab-separated and each row is a separate line. the first
 * line contains the names of the functional blocks. the order in which
 * the columns are specified doesn't have to match that of the floorplan 
 * file.
 */
int main(int argc, char **argv)
{
	int i, n, num, size, lines = 0;
	char names[MAX_UNITS][STR_SIZE];
	double vals[MAX_UNITS];
	/* trace file pointers	*/
	FILE *pin, *tout;
	/* floorplan	*/
	flp_t *flp;
	/* hotspot temperature model	*/
	RC_model_t *model;
	/* instantaneous temperature and power values	*/
	double *temp, *power;
	/* steady state temperature and power values	*/
	double *overall_power, *steady_temp;
	/* thermal model configuration parameters	*/
	thermal_config_t thermal_config;
	/* global configuration parameters	*/
	global_config_t global_config;
	/* table to hold options and configuration */
	str_pair table[MAX_ENTRIES];

	if (!(argc >= 7 && argc % 2)) {
		usage(argc, argv);
		return 1;
	}
	
	size = parse_cmdline(table, MAX_ENTRIES, argc, argv);
	global_config_from_strs(&global_config, table, size);

	/* read configuration file	*/
	if (strcmp(global_config.config, NULLFILE))
		size += read_str_pairs(&table[size], MAX_ENTRIES, global_config.config);

	/* 
	 * earlier entries override later ones. so, command line options 
	 * have priority over config file 
	 */
	size = str_pairs_remove_duplicates(table, size);

	/* get defaults */
	thermal_config = default_thermal_config();
	/* modify according to command line / config file	*/
	thermal_config_add_from_strs(&thermal_config, table, size);

	/* dump configuration if specified	*/
	if (strcmp(global_config.dump_config, NULLFILE)) {
		size = global_config_to_strs(&global_config, table, MAX_ENTRIES);
		size += thermal_config_to_strs(&thermal_config, &table[size], MAX_ENTRIES-size);
		/* prefix the name of the variable with a '-'	*/
		dump_str_pairs(table, size, global_config.dump_config, "-");
	}

	/* initialization	*/
	flp = read_flp(global_config.flp_file, FALSE);

	/* allocate and initialize the RC model	*/
	model = alloc_RC_model(thermal_config, flp);
	populate_R_model(model, flp);
	populate_C_model(model, flp);

	/* allocate the temp and power arrays	*/
	/* using hotspot_vector to internally allocate any extra nodes needed	*/
	temp = hotspot_vector(model);
	power = hotspot_vector(model);
	steady_temp = hotspot_vector(model);
	overall_power = hotspot_vector(model);
	
	/* set up initial instantaneous temperatures */
	if (strcmp(model->config.init_file, NULLFILE)) {
		if (!model->config.dtm_used)	/* initial T = steady T for no DTM	*/
			read_temp(model, temp, model->config.init_file, FALSE);
		else	/* initial T = clipped steady T with DTM	*/
			read_temp(model, temp, model->config.init_file, TRUE);
	} else	/* no input file - use init_temp as the common temperature	*/
		set_temp(model, temp, model->config.init_temp);

	/* shortcut	*/
	n=flp->n_units;

	if(!(pin = fopen(global_config.p_infile, "r")))
		fatal("unable to open power trace input file\n");
	if(!(tout = fopen(global_config.t_outfile, "w")))
		fatal("unable to open temperature trace file for output\n");

	/* names of functional units	*/
	if(read_names(pin, names) != n)
		fatal("no. of units in floorplan and trace file differ\n");

	/* header line of temperature trace	*/	
	write_names(tout, names, n);

	/* read the instantaneous power trace	*/
	while ((num=read_vals(pin, vals)) != 0) {
		if(num != n)
			fatal("invalid trace file format\n");

		/* permute the power numbers according to the floorplan order	*/
		for(i=0; i < n; i++)	
			power[get_blk_index(flp, names[i])] = vals[i];

		/* compute temperature	*/
		compute_temp(model, power, temp, model->config.sampling_intvl);

		/* permute back to the trace file order	*/
		for(i=0; i < n; i++)	
			vals[i] = temp[get_blk_index(flp, names[i])];
	
		/* output instantaneous temperature trace	*/
		write_vals(tout, vals, n);

		/* for computing average	*/
		for(i=0; i < n; i++)
			overall_power[i] += power[i];
		lines++;
	}

	if(!lines)
		fatal("no power numbers in trace file\n");
		
	/* for computing average	*/
	for(i=0; i < n; i++)
		overall_power[i] /= lines;

	/* steady state temperature	*/
	steady_state_temp(model, overall_power, steady_temp);

	/* print steady state results	*/
	fprintf(stdout, "Unit\tSteady\n");
	for (i=0; i < n; i++)
		fprintf(stdout, "%s\t%.2f\n", flp->units[i].name, steady_temp[i]-273.15);
	for (i=0; i < n; i++)
		fprintf(stdout, "interface_%s\t%.2f\n", flp->units[i].name, steady_temp[IFACE*n+i]-273.15);
	for (i=0; i < n; i++)
		fprintf(stdout, "spreader_%s\t%.2f\n", flp->units[i].name, steady_temp[HSP*n+i]-273.15);
	fprintf(stdout, "%s\t%.2f\n", "spreader_west", steady_temp[NL*n+SP_W]-273.15);
	fprintf(stdout, "%s\t%.2f\n", "spreader_east", steady_temp[NL*n+SP_E]-273.15);
	fprintf(stdout, "%s\t%.2f\n", "spreader_north", steady_temp[NL*n+SP_N]-273.15);
	fprintf(stdout, "%s\t%.2f\n", "spreader_south", steady_temp[NL*n+SP_S]-273.15);
	fprintf(stdout, "%s\t%.2f\n", "spreader_bottom", steady_temp[NL*n+SP_B]-273.15);
	fprintf(stdout, "%s\t%.2f\n", "sink_west", steady_temp[NL*n+SINK_W]-273.15);
	fprintf(stdout, "%s\t%.2f\n", "sink_east", steady_temp[NL*n+SINK_E]-273.15);
	fprintf(stdout, "%s\t%.2f\n", "sink_north", steady_temp[NL*n+SINK_N]-273.15);
	fprintf(stdout, "%s\t%.2f\n", "sink_south", steady_temp[NL*n+SINK_S]-273.15);
	fprintf(stdout, "%s\t%.2f\n", "sink_bottom", steady_temp[NL*n+SINK_B]-273.15);

	/* dump steady state temperatures on to file if needed	*/
	if (strcmp(model->config.steady_file, NULLFILE))
		dump_temp(model, steady_temp, model->config.steady_file);

	/* cleanup	*/
	fclose(pin);
	fclose(tout);
	delete_RC_model(model);
	free_flp(flp, 0);
	free_vector(temp);
	free_vector(power);
	free_vector(steady_temp);
	free_vector(overall_power);

	return 0;
}
