/* tc-arm.c  All the arm specific stuff in one convenient, huge,
   slow to compile, easy to find file.
   Contributed by Richard Earnshaw (rwe@pegasus.esprit.ec.org

   Copyright (C) 1994 Free Software Foundation, Inc.

   This file is part of GAS, the GNU Assembler.

   GAS is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GAS is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GAS; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <ctype.h>
#define  NO_RELOC 0
#include "as.h"

/* need TARGET_CPU */
#include "config.h"

#include "obstack.h"
#include <string.h>

/* This array holds the chars that always start a comment.  If the
   pre-processor is disabled, these aren't very useful */
CONST char comment_chars[] = "@;";

/* This array holds the chars that only start a comment at the beginning of
   a line.  If the line seems to have the form '# 123 filename'
   .line and .file directives will appear in the pre-processed output */
/* Note that input_file.c hand checks for '#' at the beginning of the
   first line of the input file.  This is because the compiler outputs
   #NO_APP at the beginning of its output. */
/* Also note that comments like this one will always work. */
CONST char line_comment_chars[] = "#";

CONST char line_separator_chars[] = "";

/* Chars that can be used to separate mant from exp in floating point nums */
CONST char EXP_CHARS[] = "eE";

/* Chars that mean this number is a floating point constant */
/* As in 0f12.456 */
/* or    0d1.2345e12 */

CONST char FLT_CHARS[] = "rRsSfFdDxXeEpP";

CONST relax_typeS md_relax_table[1];

CONST char *md_shortopts = "";
struct option md_longopts[] = {
  {NULL, no_argument, NULL, 0}
};
size_t md_longopts_size = sizeof(md_longopts);

int md_reloc_size = 8;		/* Size of relocation record */

struct arm_it
{
  CONST char *error;
  unsigned long instruction;
  int suffix;
  struct
    {
      bfd_reloc_code_real_type type;
      expressionS exp;
      int pc_rel;
    } reloc;
};

struct arm_it inst;

struct asm_shift
{
  CONST char *template;
  unsigned long value;
};

static CONST struct asm_shift shift[] =
{
  {NULL, 0},
  {"asl", 0},
  {"lsl", 0},
  {"lsr", 0x00000020},
  {"asr", 0x00000040},
  {"ror", 0x00000060},
  {"rrx", 0x00000060}
};

#define NO_SHIFT_RESTRICT 1
#define SHIFT_RESTRICT	  0

#define NUM_FLOAT_VALS 8

CONST char *fp_const[] = 
{
  "0.0", "1.0", "2.0", "3.0", "4.0", "5.0", "0.5", "10.0", 0
};

/* Number of littlenums required to hold an extended precision number */
#define MAX_LITTLENUMS 6

LITTLENUM_TYPE fp_values[NUM_FLOAT_VALS][MAX_LITTLENUMS];

#define FAIL	(-1)
#define SUCCESS (0)

#define FLAG_S	0
#define FLAG_P	1
#define FLAG_B	2
#define FLAG_T	3
#define FLAG_ED	4
#define FLAG_FD	5
#define FLAG_EA	6
#define FLAG_FA	7
#define FLAG_IB	8
#define FLAG_IA	9
#define FLAG_DB	10
#define FLAG_DA	11
#define FLAG_L	12

#define SUFF_S 1
#define SUFF_D 2
#define SUFF_E 3
#define SUFF_P 4

struct asm_cond
{
  CONST char *template;
  unsigned long value;
};

/* This is to save a hash look-up in the common case */
#define COND_ALWAYS 0xe0000000

static CONST struct asm_cond conds[] = 
{
  {NULL, 0},
  {"eq", 0x00000000},
  {"ne", 0x10000000},
  {"cs", 0x20000000}, {"hs", 0x20000000},
  {"cc", 0x30000000}, {"ul", 0x30000000},
  {"mi", 0x40000000},
  {"pl", 0x50000000},
  {"vs", 0x60000000},
  {"vc", 0x70000000},
  {"hi", 0x80000000},
  {"ls", 0x90000000},
  {"ge", 0xa0000000},
  {"lt", 0xb0000000},
  {"gt", 0xc0000000},
  {"le", 0xd0000000},
  {"al", 0xe0000000},
  {"nv", 0xf0000000}
};


#define LONGEST_FLAG 2
    
struct asm_flg
{
  CONST char *template;		/* Basic flag string */
  unsigned long more_flags;	/* Flags that may follow this (bit offset) */
};

static CONST struct asm_flg flags[] = 
{
  {NULL, 0},
  {"s", 0},
  {"p", 0},
  {"b", 0x10},
  {"t", 0},
  {"ed", 0},
  {"fd", 0},
  {"ea", 0},
  {"fa", 0},
  {"ib", 0},
  {"ia", 0},
  {"db", 0},
  {"da", 0},
  {"l", 0},
  {"m", 0},
  {"z", 0},
  {"e", 0}
};

static void do_mul PARAMS ((char *operands, unsigned long flags));
static void do_mla PARAMS ((char *operands, unsigned long flags));
static void do_arit PARAMS ((char *operands, unsigned long flags));
static void do_cmp PARAMS ((char *operands, unsigned long flags));
static void do_mov PARAMS ((char *operands, unsigned long flags));
static void do_ldst PARAMS ((char *operands, unsigned long flags));
static void do_ldmstm PARAMS ((char *operands, unsigned long flags));
static void do_swi PARAMS ((char *operands, unsigned long flags));
static void do_swap PARAMS ((char *operands, unsigned long flags));
static void do_branch PARAMS ((char *operands, unsigned long flags));
static void do_cdp PARAMS ((char *operands, unsigned long flags));
static void do_lstc PARAMS ((char *operands, unsigned long flags));
static void do_co_reg PARAMS ((char *operands, unsigned long flags));
static void do_fp_ctrl PARAMS ((char *operands, unsigned long flags));
static void do_fp_ldst PARAMS ((char *operands, unsigned long flags));
static void do_fp_dyadic PARAMS ((char *operands, unsigned long flags));
static void do_fp_monadic PARAMS ((char *operands, unsigned long flags));
static void do_fp_cmp PARAMS ((char *operands, unsigned long flags));
static void do_fp_from_reg PARAMS ((char *operands, unsigned long flags));
static void do_fp_to_reg PARAMS ((char *operands, unsigned long flags));

static void fix_new_arm PARAMS ((fragS *frag, int where, short int size, 
				 expressionS *exp, int pc_rel, int reloc));
static int arm_reg_parse PARAMS ((char **ccp));


/* All instructions take 4 bytes in the object file */

#define INSN_SIZE	4

/* LONGEST_INST is the longest basic instruction name without conditions or 
 * flags.  I think there is a four-letter one for floating point (there is!)
 */

#define LONGEST_INST 4

struct asm_opcode 
{
    CONST char *template;	/* Basic string to match */
    unsigned long value;	/* Basic instruction code */
    CONST char *comp_suffix;	/* Compulsory suffix that must follow conds */
    unsigned long flags[16];	/* Bits to toggle if flag 'n' set */
    void (*parms)();		/* Function to call to parse args */
};

#define TRANS_BIT	(0x00200000)

static CONST struct asm_opcode insns[] = 
{
  {NULL, 0, 0,
      {0, 0, 0, 0 ,0, 0, 0, 0 ,0, 0, 0, 0, 0, 0, 0},
      NULL},
  {"mul", 0x00000090, 0,
      {0x00100000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_mul},
  {"mla", 0x00200090, 0,
      {0x00100000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_mla},
  {"and", 0x00000000, 0,
      {0x00100000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_arit},
  {"eor", 0x00200000, 0,
      {0x00100000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_arit},
  {"sub", 0x00400000, 0,
      {0x00100000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_arit},
  {"rsb", 0x00600000, 0,
      {0x00100000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_arit},
  {"add", 0x00800000, 0,
      {0x00100000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_arit},
  {"adc", 0x00a00000, 0,
      {0x00100000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_arit},
  {"sbc", 0x00c00000, 0,
      {0x00100000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_arit},
  {"rsc", 0x00e00000, 0,
      {0x00100000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_arit},
  {"orr", 0x01800000, 0,
      {0x00100000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_arit},
  {"bic", 0x01c00000, 0,
      {0x00100000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_arit},
  {"tst", 0x01000000, 0,
      {0x00100000, 0x0010f000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_cmp},
  {"teq", 0x01200000, 0,
      {0x00100000, 0x0010f000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_cmp},
  {"cmp", 0x01400000, 0,
      {0x00100000, 0x0010f000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_cmp},
  {"cmn", 0x01600000, 0,
      {0x00100000, 0x0010f000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_cmp},
  {"mov", 0x01a00000, 0,
      {0x00100000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_mov},
  {"mvn", 0x01e00000, 0,
      {0x00100000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_mov},
  {"str", 0x04000000, 0,
      {0, 0, 0x00400000, TRANS_BIT, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_ldst},
  {"ldr", 0x04100000, 0,
      {0, 0, 0x00400000, TRANS_BIT, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_ldst},
  {"stm", 0x08000000, 0,
      {0, 0, 0, 0, 0x08000000, 0x01000000, 0x00800000, 0x01800000,
       0x01800000, 0x00800000, 0x01000000, 0x08000000, 0, 0, 0},
      do_ldmstm},
  {"ldm", 0x08100000, 0,
      {0, 0, 0, 0, 0x01800000, 0x00800000, 0x01000000, 0x08000000,
       0x01800000, 0x00800000, 0x01000000, 0x08000000, 0, 0, 0},
      do_ldmstm},
  {"swi", 0x0f000000, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_swi},
  {"swp", 0x01000090, 0,
      {0, 0, 0x00400000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_swap},
  {"bl",  0x0b000000, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_branch},
  {"b",   0x0a000000, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_branch},
/* Generic copressor instructions */
  {"cdp",  0x0e000000, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_cdp},
  {"ldc", 0x0c100000, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x00400000, 0, 0},
      do_lstc},
  {"stc", 0x0c000000, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x00400000, 0, 0},
      do_lstc},
  {"mcr", 0x0e000010, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_co_reg},
  {"mrc", 0x0e100010, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_co_reg},
/* Floating point instructions should go here */
  {"wfs", 0x0e200110, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_fp_ctrl},
  {"rfs", 0x0e300110, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_fp_ctrl},
  {"wfc", 0x0e400110, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_fp_ctrl},
  {"rfc", 0x0e500110, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_fp_ctrl},
  {"ldf", 0x0c100100, "sdep",
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_fp_ldst},
  {"stf", 0x0c000100, "sdep",
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_fp_ldst},
  {"mvf", 0x0e008100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"mnf", 0x0e108100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"abs", 0x0e208100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"rnd", 0x0e308100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"sqt", 0x0e408100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"log", 0x0e508100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"lgn", 0x0e608100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"exp", 0x0e708100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"sin", 0x0e808100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"cos", 0x0e908100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"tan", 0x0ea08100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"asn", 0x0eb08100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"acs", 0x0ec08100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"atn", 0x0ed08100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"urd", 0x0ee08100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"nrm", 0x0ef08100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_monadic},
  {"adf", 0x0e000100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_dyadic},
  {"suf", 0x0e200100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_dyadic},
  {"rsf", 0x0e300100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_dyadic},
  {"muf", 0x0e100100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_dyadic},
  {"dvf", 0x0e400100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_dyadic},
  {"rdf", 0x0e500100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_dyadic},
  {"pow", 0x0e600100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_dyadic},
  {"rpw", 0x0e700100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_dyadic},
  {"rmf", 0x0e800100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_dyadic},
  {"fml", 0x0e900100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_dyadic},
  {"fdv", 0x0ea00100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_dyadic},
  {"frd", 0x0eb00100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_dyadic},
  {"pol", 0x0ec00100, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_dyadic},
  {"cmf", 0x0e90f110, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x00400000},
      do_fp_cmp},
  {"cnf", 0x0eb0f110, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x00400000},
      do_fp_cmp},
  /* The FPA10 data sheet suggests that the 'E' of cmfe/cnfe should not
     be an optional suffix, but part of the instruction.  To be compatible,
     we accept either.  */
  {"cmfe", 0x0ed0f110, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_fp_cmp},
  {"cnfe", 0x0ef0f110, 0,
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      do_fp_cmp},
  {"flt", 0x0e000110, "sde",
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_from_reg},
  {"fix", 0x0e100110, 0,
      {0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40, 0x60},
      do_fp_to_reg}
};

/* defines for various bits that we will want to toggle */

#define INST_IMMEDIATE	0x02000000
#define OFFSET_REG	0x02000000
#define SHIFT_BY_REG	0x00000010
#define PRE_INDEX	0x01000000
#define INDEX_UP	0x00800000
#define WRITE_BACK	0x00200000
#define MULTI_SET_PSR	0x00400000

struct reg_entry
{
  CONST char *name;
  int number;
};

#define int_register(reg) ((reg) >= 0 && (reg) <= 15)
#define cp_register(reg) ((reg) >= 32 && (reg) <= 47)
#define fp_register(reg) ((reg) >= 16 && (reg) <= 23)

#define REG_PC	15

/* These are the standard names;  Users can add aliases with .req */
static CONST struct reg_entry reg_table[] =
{
  {"r0", 0},    {"r1", 1},    {"r2", 2},    {"r3", 3},
  {"r4", 4},    {"r5", 5},    {"r6", 6},    {"r7", 7},
  {"r8", 8},    {"r9", 9},    {"r10", 10},  {"r11", 11},
  {"r12", 12},  {"r13", 13},  {"r14", 14},  {"r15", REG_PC},
  {"f0", 16},   {"f1", 17},   {"f2", 18},   {"f3", 19},
  {"f4", 20},   {"f5", 21},   {"f6", 22},   {"f7", 23},
  {"c0", 32},   {"c1", 33},   {"c2", 34},   {"c3", 35},
  {"c4", 36},   {"c5", 37},   {"c6", 38},   {"c7", 39},
  {"c8", 40},   {"c9", 41},   {"c10", 42},  {"c11", 43},
  {"c12", 44},  {"c13", 45},  {"c14", 46},  {"c15", 47},
  {"cr0", 32},  {"cr1", 33},  {"cr2", 34},  {"cr3", 35},
  {"cr4", 36},  {"cr5", 37},  {"cr6", 38},  {"cr7", 39},
  {"cr8", 40},  {"cr9", 41},  {"cr10", 42}, {"cr11", 43},
  {"cr12", 44}, {"cr13", 45}, {"cr14", 46}, {"cr15", 47},
  {NULL, 0}
};

static CONST char *bad_args = "Bad arguments to instruction";
static CONST char *bad_pc = "r15 not allowed here";

static struct hash_control *arm_ops_hsh = NULL;
static struct hash_control *arm_cond_hsh = NULL;
static struct hash_control *arm_flg_hsh = NULL;
static struct hash_control *arm_shift_hsh = NULL;
static struct hash_control *arm_reg_hsh = NULL;

/* This table describes all the machine specific pseudo-ops the assembler
   has to support.  The fields are:
   pseudo-op name without dot
   function to call to execute this pseudo-op
   Integer arg to pass to the function
   */

static void s_req PARAMS ((int));
static void s_bss PARAMS ((int));
static void s_align PARAMS ((int));
static void s_even PARAMS ((int));

static int my_get_expression PARAMS ((expressionS *, char **));

CONST pseudo_typeS md_pseudo_table[] =
{
  {"req", s_req, 0},	/* Never called becasue '.req' does not start line */
  {"bss", s_bss, 0},
  {"align", s_align, 0},
  {"even", s_even, 0},
  {"word", cons, 4},
  {0, 0, 0}
};

/* Check that an immediate is valid, and if so, convert it to the right format
 */

/* OH, for a rotate instruction in C! */

static int
validate_immediate (val)
     int val;
{
  unsigned int a = (unsigned int) val;
  int i;
  
  /* Do the easy (and most common ones) quickly */
  for (i = 0; i <= 24; i += 2)
    {
      if ((a & (0xff << i)) == a)
	return (int) (((32 - i) & 0x1e) << 7) | ((a >> i) & 0xff);
    }

  /* Now do the harder ones */
  for (; i < 32; i += 2)
    {
      if ((a & ((0xff << i) | (0xff >> (32 - i)))) == a)
	{
	  a = ((a >> i) & 0xff) | ((a << (32 - i)) & 0xff);
	  return (int) a | (((32 - i) >> 1) << 8);
	}
    }
  return -1;
}

static int
validate_offset_imm (val)
     int val;
{
  if (val < -4095 || val > 4095)
    as_bad ("bad immediate value for offset (%d)", val);
  return val;
}

    
static void
s_req (a)
     int a;
{
  as_bad ("Invalid syntax for .req directive.");
}

static void
s_bss (ignore)
     int ignore;
{
  /* We don't support putting frags in the BSS segment, we fake it by
     marking in_bss, then looking at s_skip for clues?.. */
  subseg_set (bss_section, 0);
  demand_empty_rest_of_line ();
}

static void
s_even (ignore)
     int ignore;
{
  if (!need_pass_2)		/* Never make frag if expect extra pass. */
    frag_align (1, 0);
  record_alignment (now_seg, 1);
  demand_empty_rest_of_line ();
}

static void
s_align (unused)	/* Same as s_align_ptwo but align 0 => align 2 */
     int unused;
{
  register int temp;
  register long temp_fill;
  long max_alignment = 15;

  temp = get_absolute_expression ();
  if (temp > max_alignment)
    as_bad ("Alignment too large: %d. assumed.", temp = max_alignment);
  else if (temp < 0)
    {
      as_bad ("Alignment negative. 0 assumed.");
      temp = 0;
    }

  if (*input_line_pointer == ',')
    {
      input_line_pointer++;
      temp_fill = get_absolute_expression ();
    }
  else
    temp_fill = 0;

  if (!temp)
    temp = 2;

  /* Only make a frag if we HAVE to. . . */
  if (temp && !need_pass_2)
    frag_align (temp, (int) temp_fill);

  record_alignment (now_seg, temp);

  demand_empty_rest_of_line ();
}

static void
end_of_line (str)
     char *str;
{
  while (*str == ' ')
    str++;

  if (*str != '\0')
    inst.error = "Garbage following instruction";
}

static int
skip_past_comma (str)
     char **str;
{
  char *p = *str, c;
  int comma = 0;
    
  while ((c = *p) == ' ' || c == ',')
    {
      p++;
      if (c == ',' && comma++)
	return FAIL;
    }

  if (c == '\0')
    return FAIL;

  *str = p;
  return comma ? SUCCESS : FAIL;
}

/* A standard register must be given at this point.  Shift is the place to
   put it in the instruction. */

static int
reg_required_here (str, shift)
     char **str;
     int shift;
{
  int reg;
  char *start = *str;

  if ((reg = arm_reg_parse (str)) != FAIL && int_register (reg))
    {
      inst.instruction |= reg << shift;
      return reg;
    }

  /* In the few cases where we might be able to accept something else
     this error can be overridden */
  inst.error = "Register expected";

  /* Restore the start point, we may have got a reg of the wrong class.  */
  *str = start;
  return FAIL;
}

static int co_proc_number (str)
     char **str;
{
  int processor, pchar;

  while (**str == ' ')
    (*str)++;

  /* The data sheet seems to imply that just a number on its own is valid
     here, but the RISC iX assembler seems to accept a prefix 'p'.  We will
     accept either.  */
  if (**str == 'p' || **str == 'P')
    (*str)++;

  pchar = *(*str)++;
  if (pchar >= '0' && pchar <= '9')
    {
      processor = pchar - '0';
      if (**str >= '0' && **str <= '9')
	{
	  processor = processor * 10 + *(*str)++ - '0';
	  if (processor > 15)
	    {
	      inst.error = "Illegal co-processor number";
	      return FAIL;
	    }
	}
    }
  else
    {
      inst.error = "Bad or missing co-processor number";
      return FAIL;
    }

  inst.instruction |= processor << 8;
  return SUCCESS;
}

static int
cp_opc_expr (str, where, length)
     char **str;
     int where;
     int length;
{
  expressionS expr;

  while (**str == ' ')
    (*str)++;

  memset (&expr, '\0', sizeof (expr));

  if (my_get_expression (&expr, str))
    return FAIL;
  if (expr.X_op != O_constant)
    {
      inst.error = "bad or missing expression";
      return FAIL;
    }

  if ((expr.X_add_number & ((1 << length) - 1)) != expr.X_add_number)
    {
      inst.error = "immediate co-processor expression too large";
      return FAIL;
    }

  inst.instruction |= expr.X_add_number << where;
  return SUCCESS;
}

static int
cp_reg_required_here (str, where)
     char **str;
     int where;
{
  int reg;
  char *start = *str;

  if ((reg = arm_reg_parse (str)) != FAIL && cp_register (reg))
    {
      reg &= 15;
      inst.instruction |= reg << where;
      return reg;
    }

  /* In the few cases where we might be able to accept something else
     this error can be overridden */
  inst.error = "Co-processor register expected";

  /* Restore the start point */
  *str = start;
  return FAIL;
}

static int
fp_reg_required_here (str, where)
     char **str;
     int where;
{
  int reg;
  char *start = *str;

  if ((reg = arm_reg_parse (str)) != FAIL && fp_register (reg))
    {
      reg &= 7;
      inst.instruction |= reg << where;
      return reg;
    }

  /* In the few cases where we might be able to accept something else
     this error can be overridden */
  inst.error = "Floating point register expected";

  /* Restore the start point */
  *str = start;
  return FAIL;
}

static int
cp_address_offset (str)
     char **str;
{
  int offset;

  while (**str == ' ')
    (*str)++;

  if (**str != '#')
    {
      inst.error = "immediate expression expected";
      return FAIL;
    }

  (*str)++;
  if (my_get_expression (&inst.reloc.exp, str))
    return FAIL;
  if (inst.reloc.exp.X_op == O_constant)
    {
      offset = inst.reloc.exp.X_add_number;
      if (offset & 3)
	{
	  inst.error = "co-processor address must be word aligned";
	  return FAIL;
	}

      if (offset > 1023 || offset < -1023)
	{
	  inst.error = "offset too large";
	  return FAIL;
	}

      if (offset >= 0)
	inst.instruction |= INDEX_UP;
      else
	offset = -offset;

      inst.instruction |= offset >> 2;
    }
  else
    inst.reloc.type = BFD_RELOC_ARM_CP_OFF_IMM;

  return SUCCESS;
}

static int
cp_address_required_here (str)
     char **str;
{
  char *p = *str;
  int pre_inc = 0;
  int write_back = 0;

  if (*p == '[')
    {
      int reg;

      p++;
      while (*p == ' ')
	p++;

      if ((reg = reg_required_here (&p, 16)) == FAIL)
	{
	  inst.error = "Register required";
	  return FAIL;
	}

      while (*p == ' ')
	p++;

      if (*p == ']')
	{
	  p++;
	  if (skip_past_comma (&p) == SUCCESS)
	    {
	      /* [Rn], #expr */
	      write_back = WRITE_BACK;
	      if (reg == REG_PC)
		{
		  inst.error = "pc may not be used in post-increment";
		  return FAIL;
		}

	      if (cp_address_offset (&p) == FAIL)
		return FAIL;
	    }
	}
      else
	{
	  /* '['Rn, #expr']'[!] */

	  if (skip_past_comma (&p) == FAIL)
	    {
	      inst.error = "pre-indexed expression expected";
	      return FAIL;
	    }

	  pre_inc = PRE_INDEX;
	  if (cp_address_offset (&p) == FAIL)
	    return FAIL;

	  while (*p == ' ')
	    p++;

	  if (*p++ != ']')
	    {
	      inst.error = "missing ]";
	      return FAIL;
	    }

	  while (*p == ' ')
	    p++;

	  if (*p == '!')
	    {
	      if (reg == REG_PC)
		{
		  inst.error = "pc may not be used with write-back";
		  return FAIL;
		}

	      p++;
	      write_back = WRITE_BACK;
	    }
	}
    }
  else
    {
      if (my_get_expression (&inst.reloc.exp, &p))
	return FAIL;

      inst.reloc.type = BFD_RELOC_ARM_CP_OFF_IMM;
      inst.reloc.exp.X_add_number -= 8;  /* PC rel adjust */
      inst.reloc.pc_rel = 1;
      inst.instruction |= (REG_PC << 16);
    }

  inst.instruction |= write_back | pre_inc;
  *str = p;
  return SUCCESS;
}

static void
do_mul (str, flags)
     char *str;
     unsigned long flags;
{
  int rd, rm;
  
  /* only one format "rd, rm, rs" */
  while (*str == ' ')
    str++;

  if ((rd = reg_required_here (&str, 16)) == FAIL)
    {
      inst.error = bad_args;
      return;
    }

  if (rd == REG_PC)
    {
      inst.error = bad_pc;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || (rm = reg_required_here (&str, 0)) == FAIL)
    {
      inst.error = bad_args;
      return;
    }

  if (rm == REG_PC)
    {
      inst.error = bad_pc;
      return;
    }

  if (rm == rd)
    as_tsktsk ("Warning: rd and rm should be different in mul");

  if (skip_past_comma (&str) == FAIL
      || (rm = reg_required_here (&str, 8)) == FAIL)
    {
      inst.error = bad_args;
      return;
    }

  if (rm == REG_PC)
    {
      inst.error = bad_pc;
      return;
    }

  inst.instruction |= flags;
  end_of_line (str);
  return;
}

static void
do_mla (str, flags)
     char *str;
     unsigned long flags;
{
  int rd, rm;

  /* only one format "rd, rm, rs, rn" */
  while (*str == ' ')
    str++;

  if ((rd = reg_required_here (&str, 16)) == FAIL)
    {
      inst.error = bad_args;
      return;
    }

  if (rd == REG_PC)
    {
      inst.error = bad_pc;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || (rm = reg_required_here (&str, 0)) == FAIL)
    {
      inst.error = bad_args;
      return;
    }

  if (rm == REG_PC)
    {
      inst.error = bad_pc;
      return;
    }

  if (rm == rd)
    as_tsktsk ("Warning: rd and rm should be different in mla");

  if (skip_past_comma (&str) == FAIL
      || (rd = reg_required_here (&str, 8)) == FAIL
      || skip_past_comma (&str) == FAIL
      || (rm = reg_required_here (&str, 12)) == FAIL)
    {
      inst.error = bad_args;
      return;
    }

  if (rd == REG_PC || rm == REG_PC)
    {
      inst.error = bad_pc;
      return;
    }

  inst.instruction |= flags;
  end_of_line (str);
  return;
}

/* Returns the index into fp_values of a floating point number, or -1 if
   not in the table.  */
static int
my_get_float_expression (str)
     char **str;
{
  LITTLENUM_TYPE words[MAX_LITTLENUMS];
  char *save_in;
  expressionS exp;
  int i, j;

  memset (words, 0, MAX_LITTLENUMS * sizeof (LITTLENUM_TYPE));
  /* Look for a raw floating point number */
  if ((save_in = atof_ieee (*str, 'x', words)) != NULL
      && (is_end_of_line [(int)(*save_in)] || *save_in == '\0'))
    {
      for (i = 0; i < NUM_FLOAT_VALS; i++)
	{
	  for (j = 0; j < MAX_LITTLENUMS; j++)
	    {
	      if (words[j] != fp_values[i][j])
		break;
	    }

	  if (j == MAX_LITTLENUMS)
	    {
	      *str = save_in;
	      return i;
	    }
	}
    }

  /* Try and parse a more complex expression, this will probably fail
     unless the code uses a floating point prefix (eg "0f") */
  save_in = input_line_pointer;
  input_line_pointer = *str;
  if (expression (&exp) == absolute_section
      && exp.X_op == O_big
      && exp.X_add_number < 0)
    {
      if (gen_to_words (words, 6, (long)15) == 0)
	{
	  for (i = 0; i < NUM_FLOAT_VALS; i++)
	    {
	      for (j = 0; j < MAX_LITTLENUMS; j++)
		{
		  if (words[j] != fp_values[i][j])
		    break;
		}

	      if (j == MAX_LITTLENUMS)
		{
		  *str = input_line_pointer;
		  input_line_pointer = save_in;
		  return i;
		}
	    }
	}
    }

  *str = input_line_pointer;
  input_line_pointer = save_in;
  return -1;
}

/* Return true if anything in the expression is a bignum */
static int
walk_no_bignums (sp)
     symbolS *sp;
{
  if (sp->sy_value.X_op == O_big)
    return 1;

  if (sp->sy_value.X_add_symbol)
    {
      return (walk_no_bignums (sp->sy_value.X_add_symbol)
	      || (sp->sy_value.X_op_symbol
		  && walk_no_bignums (sp->sy_value.X_op_symbol)));
    }

  return 0;
}
  
static int
my_get_expression (ep, str)
     expressionS *ep;
     char **str;
{
  char *save_in;
  segT seg;
  
  save_in = input_line_pointer;
  input_line_pointer = *str;
  seg = expression (ep);
  if (seg != absolute_section
      && seg != text_section
      && seg != data_section
      && seg != bss_section
      && seg != undefined_section)
    {
      inst.error = "bad_segment";
      *str = input_line_pointer;
      input_line_pointer = save_in;
      return 1;
    }

  if (ep->X_op == O_big
      || (ep->X_add_symbol
	  && (walk_no_bignums (ep->X_add_symbol)
	      || (ep->X_op_symbol
		  && walk_no_bignums (ep->X_op_symbol)))))
    {
      inst.error = "Invalid constant";
      *str = input_line_pointer;
      input_line_pointer = save_in;
      return 1;
    }

  *str = input_line_pointer;
  input_line_pointer = save_in;
  return 0;
}

/* unrestrict should be one if <shift> <register> is permitted for this
   instruction */

static int
decode_shift (str, unrestrict)
char **str;
int unrestrict;
{
  struct asm_shift *shft;
  char *p;
  char c;
    
  while (**str == ' ')
    (*str)++;
    
  for (p = *str; isalpha (*p); p++)
    ;

  if (p == *str)
    {
      inst.error = "Shift expression expected";
      return FAIL;
    }

  c = *p;
  *p = '\0';
  shft = (struct asm_shift *) hash_find (arm_shift_hsh, *str);
  *p = c;
  if (shft)
    {
      if (!strcmp (*str, "rrx"))
	{
	  *str = p;
	  inst.instruction |= shft->value;
	  return SUCCESS;
	}

      while (*p == ' ')
	p++;

      if (unrestrict && reg_required_here (&p, 8) != FAIL)
	{
	  inst.instruction |= shft->value | SHIFT_BY_REG;
	  *str = p;
	  return SUCCESS;
	}
      else if (*p == '#')
	{
	  inst.error = NULL;
	  p++;
	  if (my_get_expression (&inst.reloc.exp, &p))
	    return FAIL;

	  /* Validate some simple #expressions */
	  if (!inst.reloc.exp.X_add_symbol)
	    {
	      int num = inst.reloc.exp.X_add_number;
	      if (num < 0 || num > 32
		  || (num == 32 
		      && (shft->value == 0 || shft->value == 0x60)))
		{
		  inst.error = "Invalid immediate shift";
		  return FAIL;
		}

	      /* Shifts of zero should be converted to lsl (which is zero)*/
	      if (num == 0)
		{
		  *str = p;
		  return SUCCESS;
		}

	      /* Shifts of 32 are encoded as 0, for those shifts that
		 support it.  */
	      if (num == 32)
		num = 0;

	      inst.instruction |= (num << 7) | shft->value;
	      *str = p;
	      return SUCCESS;
	    }

	  inst.reloc.type = BFD_RELOC_ARM_SHIFT_IMM;
	  inst.reloc.pc_rel = 0;
	  inst.instruction |= shft->value;
	  *str = p;
	  return SUCCESS;
	}
      else
	{
	  inst.error = unrestrict ? "shift requires register or #expression"
	    : "shift requires #expression";
	  *str = p;
	  return FAIL;
	}
    }

  inst.error = "Shift expression expected";
  return FAIL;
}

static int
data_op2 (str)
     char **str;
{
  while (**str == ' ')
    (*str)++;
    
  if (reg_required_here (str, 0) != FAIL)
    {
      if (skip_past_comma (str) == SUCCESS)
	{
	  /* Shift operation on register */
	  return decode_shift (str, NO_SHIFT_RESTRICT);
	}
      return SUCCESS;
    }
  else
    {
      /* Immediate expression */
      if (*((*str)++) == '#')
	{
	  inst.error = NULL;
	  if (my_get_expression (&inst.reloc.exp, str))
	    return FAIL;

	  if (inst.reloc.exp.X_add_symbol)
	    {
	      inst.reloc.type = BFD_RELOC_ARM_IMMEDIATE;
	      inst.reloc.pc_rel = 0;
	    }
	  else
	    {
	      int value = validate_immediate (inst.reloc.exp.X_add_number);

	      if (value == -1)
		{
		  inst.error = "Invalid constant";
		  return FAIL;
		}

	      inst.instruction |= value;
	    }

	  inst.instruction |= INST_IMMEDIATE;
	  return SUCCESS;
	}

      inst.error = "Register or shift expression expected";
      return FAIL;
    }
}

static int
fp_op2 (str, flags)
     char **str;
     unsigned long flags;
{
  while (**str == ' ')
    (*str)++;

  if (fp_reg_required_here (str, 0) != FAIL)
    return SUCCESS;
  else
    {
      /* Immediate expression */
      if (*((*str)++) == '#')
	{
	  int i;

	  inst.error = NULL;
	  while (**str == ' ')
	    (*str)++;

	  /* First try and match exact strings, this is to guarantee that
	     some formats will work even for cross assembly */

	  for (i = 0; fp_const[i]; i++)
	    {
	      if (strncmp (*str, fp_const[i], strlen (fp_const[i])) == 0)
		{
		  char *start = *str;

		  *str += strlen (fp_const[i]);
		  if (is_end_of_line [(int)**str] || **str == '\0')
		    {
		      inst.instruction |= i + 8;
		      return SUCCESS;
		    }
		  *str = start;
		}
	    }

	  /* Just because we didn't get a match doesn't mean that the
	     constant isn't valid, just that it is in a format that we
	     don't automatically recognize.  Try parsing it with
	     the standard expression routines.  */
	  if ((i = my_get_float_expression (str)) >= 0)
	    {
	      inst.instruction |= i + 8;
	      return SUCCESS;
	    }

	  inst.error = "Invalid floating point immediate expression";
	  return FAIL;
	}
      inst.error = "Floating point register or immediate expression expected";
      return FAIL;
    }
}

static void
do_arit (str, flags)
     char *str;
     unsigned long flags;
{
  while (*str == ' ')
    str++;

  if (reg_required_here (&str, 12) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || reg_required_here (&str, 16) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || data_op2 (&str) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  inst.instruction |= flags;
  end_of_line (str);
  return;
}

static void
do_cmp (str, flags)
     char *str;
     unsigned long flags;
{
  while (*str == ' ')
    str++;

  if (reg_required_here (&str, 16) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || data_op2 (&str) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  inst.instruction |= flags;
  if ((flags & 0x0000f000) == 0)
    inst.instruction |= 0x00100000;
  end_of_line (str);
  return;
}

static void
do_mov (str, flags)
     char *str;
     unsigned long flags;
{
  while (*str == ' ')
    str++;

  if (reg_required_here (&str, 12) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || data_op2 (&str) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  inst.instruction |= flags;
  end_of_line (str);
  return;
}

static int
ldst_extend (str)
     char **str;
{
  int add = INDEX_UP;

  switch (**str)
    {
    case '#':
      (*str)++;
      if (my_get_expression (&inst.reloc.exp, str))
	return FAIL;

      if (inst.reloc.exp.X_op == O_constant)
	{
	  int value = inst.reloc.exp.X_add_number;

	  if (value < -4095 || value > 4095)
	    {
	      inst.error = "address offset too large";
	      return FAIL;
	    }

	  if (value < 0)
	    {
	      value = -value;
	      add = 0;
	    }

	  inst.instruction |= add | value;
	}
      else
	{
	  inst.reloc.type = BFD_RELOC_ARM_OFFSET_IMM;
	  inst.reloc.pc_rel = 0;
	}
      return SUCCESS;

    case '-':
      add = 0;	/* and fall through */
    case '+':
      (*str)++;	/* and fall through */
    default:
      if (reg_required_here (str, 0) == FAIL)
	{
	  inst.error = "Register expected";
	  return FAIL;
	}
      inst.instruction |= add | OFFSET_REG;
      if (skip_past_comma (str) == SUCCESS)
	return decode_shift (str, SHIFT_RESTRICT);
      return SUCCESS;
    }
}

static void
do_ldst (str, flags)
     char *str;
     unsigned long flags;
{
  int pre_inc = 0;
  int conflict_reg;

  while (*str == ' ')
    str++;
    
  if ((conflict_reg = reg_required_here (&str, 12)) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL)
    {
      inst.error = "Address expected";
      return;
    }

  if (*str == '[')
    {
      int reg;

      str++;
      while (*str == ' ')
	str++;

      if ((reg = reg_required_here (&str, 16)) == FAIL)
	{
	  inst.error = "Register required";
	  return;
	}

      conflict_reg = (((conflict_reg == reg)
		       && (inst.instruction & 0x00100000))
		      ? 1 : 0);

      while (*str == ' ')
	str++;

      if (*str == ']')
	{
	  str++;
	  if (skip_past_comma (&str) == SUCCESS)
	    {
	      /* [Rn],... (post inc) */
	      if (ldst_extend (&str) == FAIL)
		return;
	      if (conflict_reg)
		as_warn ("destination register same as write-back base\n");
	    }
	  else
	    {
	      /* [Rn] */
	      flags |= INDEX_UP;
	    }
	}
      else
	{
	  /* [Rn,...] */
	  if (skip_past_comma (&str) == FAIL)
	    {
	      inst.error = "pre-indexed expression expected";
	      return;
	    }

	  pre_inc = 1;
	  if (ldst_extend (&str) == FAIL)
	    return;

	  while (*str == ' ')
	    str++;

	  if (*str++ != ']')
	    {
	      inst.error = "missing ]";
	      return;
	    }

	  while (*str == ' ')
	    str++;

	  if (*str == '!')
	    {
	      if (conflict_reg)
		as_warn ("destination register same as write-back base\n");
	      str++;
	      inst.instruction |= WRITE_BACK;
	    }
	}
    }
  else
    {
      if (my_get_expression (&inst.reloc.exp, &str))
	return;

      inst.reloc.type = BFD_RELOC_ARM_OFFSET_IMM;
      inst.reloc.exp.X_add_number -= 8;  /* PC rel adjust */
      inst.reloc.pc_rel = 1;
      inst.instruction |= (REG_PC << 16);
      pre_inc = 1;
    }
    
  if (pre_inc && (flags & TRANS_BIT))
    inst.error = "Pre-increment instruction with translate";

  inst.instruction |= flags | (pre_inc ? PRE_INDEX : 0);
  end_of_line (str);
  return;
}

static void
do_ldmstm (str, flags)
     char *str;
     unsigned long flags;
{
  int base_reg;

  while (*str == ' ')
    str++;

  if ((base_reg = reg_required_here (&str, 16)) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (base_reg == REG_PC)
    {
      inst.error = "r15 not allowed as base register";
      return;
    }

  while (*str == ' ')
    str++;
  if (*str == '!')
    {
      flags |= WRITE_BACK;
      str++;
    }

  if (skip_past_comma (&str) == FAIL)
    {
      inst.error = bad_args;
      return;
    }

  /* We come back here if we get ranges concatenated by '+' or '|' */
 another_range:
  if (*str == '{')
    {
      int in_range = 0;
      int cur_reg = -1;
      
      str++;
      do
	{
	  int reg;
	    
	  while (*str == ' ')
	    str++;

	  if ((reg = arm_reg_parse (&str)) == FAIL || !int_register (reg))
	    {
	      inst.error = "Register expected";
	      return;
	    }

	  if (in_range)
	    {
	      int i;
	      
	      if (reg <= cur_reg)
		{
		  inst.error = "Bad range in register list";
		  return;
		}

	      for (i = cur_reg + 1; i < reg; i++)
		{
		  if (flags & (1 << i))
		    as_tsktsk 
		      ("Warning: Duplicated register (r%d) in register list",
		       i);
		  else
		    flags |= 1 << i;
		}
	      in_range = 0;
	    }

	  if (flags & (1 << reg))
	    as_tsktsk ("Warning: Duplicated register (r%d) in register list",
		       reg);
	  else if (reg <= cur_reg)
	    as_tsktsk ("Warning: Register range not in ascending order");

	  flags |= 1 << reg;
	  cur_reg = reg;
	} while (skip_past_comma (&str) != FAIL
		 || (in_range = 1, *str++ == '-'));
      str--;
      while (*str == ' ')
	str++;

      if (*str++ != '}')
	{
	  inst.error = "Missing `}'";
	  return;
	}
    }
  else
    {
      expressionS expr;

      if (my_get_expression (&expr, &str))
	return;

      if (expr.X_op == O_constant)
	{
	  if (expr.X_add_number 
	      != (expr.X_add_number & 0x0000ffff))
	    {
	      inst.error = "invalid register mask";
	      return;
	    }
	  if ((flags & expr.X_add_number) != 0)
	    {
	      int regno = flags & expr.X_add_number;

	      regno &= -regno;
	      regno = (1 << regno) - 1;
	      as_tsktsk ("Warning: Duplicated register (r%d) in register list",
			 regno);
	    }
	  flags |= expr.X_add_number;
	}
      else
	{
	  if (inst.reloc.type != 0)
	    {
	      inst.error = "expression too complex";
	      return;
	    }

	  memcpy (&inst.reloc.exp, &expr, sizeof (expressionS));
	  inst.reloc.type = BFD_RELOC_ARM_MULTI;
	  inst.reloc.pc_rel = 0;
	}
    }

  while (*str == ' ')
    str++;

  if (*str == '|' || *str == '+')
    {
      str++;
      goto another_range;
    }

  if (*str == '^')
    {
      str++;
      flags |= MULTI_SET_PSR;
    }
  inst.instruction |= flags;
  end_of_line (str);
  return;
}

static void
do_swi (str, flags)
     char *str;
     unsigned long flags;
{
  if (my_get_expression (&inst.reloc.exp, &str))
    return;

  inst.reloc.type = BFD_RELOC_ARM_SWI;
  inst.reloc.pc_rel = 0;
  inst.instruction |= flags;
  end_of_line (str);
  return;
}

static void
do_swap (str, flags)
     char *str;
     unsigned long flags;
{
  int reg;
  
  while (*str == ' ')
    str++;

  if ((reg = reg_required_here (&str, 12)) == FAIL)
    return;

  if (reg == REG_PC)
    {
      inst.error = bad_pc;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || (reg = reg_required_here (&str, 0)) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (reg == REG_PC)
    {
      inst.error = bad_pc;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || *str++ != '[')
    {
      inst.error = bad_args;
      return;
    }

  while (*str == ' ')
    str++;

  if ((reg = reg_required_here (&str, 16)) == FAIL)
    return;

  if (reg == REG_PC)
    {
      inst.error = bad_pc;
      return;
    }

  while (*str == ' ')
    str++;

  if (*str++ != ']')
    {
      inst.error = "missing ]";
      return;
    }

  inst.instruction |= flags;
  end_of_line (str);
  return;
}

static void
do_branch (str, flags)
     char *str;
     unsigned long flags;
{
  if (my_get_expression (&inst.reloc.exp, &str))
    return;
  inst.reloc.type = BFD_RELOC_ARM_PCREL_BRANCH;
  inst.reloc.pc_rel = 1;
  inst.instruction |= flags | 0x00fffffe;	/* PC-rel adjust */
  end_of_line (str);
  return;
}

static void
do_cdp (str, flags)
     char *str;
     unsigned long flags;
{
  /* Co-processor data operation.
     Format: CDP{cond} CP#,<expr>,CRd,CRn,CRm{,<expr>}  */
  while (*str == ' ')
    str++;

  if (co_proc_number (&str) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || cp_opc_expr (&str, 20,4) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || cp_reg_required_here (&str, 12) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || cp_reg_required_here (&str, 16) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || cp_reg_required_here (&str, 0) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == SUCCESS)
    {
      if (cp_opc_expr (&str, 5, 3) == FAIL)
	{
	  if (!inst.error)
	    inst.error = bad_args;
	  return;
	}
    }

  end_of_line (str);
  return;
}

static void
do_lstc (str, flags)
     char *str;
     unsigned long flags;
{
  /* Co-processor register load/store.
     Format: <LDC|STC{cond}[L] CP#,CRd,<address>  */

  while (*str == ' ')
    str++;

  if (co_proc_number (&str) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || cp_reg_required_here (&str, 12) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || cp_address_required_here (&str) == FAIL)
    {
      if (! inst.error)
	inst.error = bad_args;
      return;
    }

  inst.instruction |= flags;
  end_of_line (str);
  return;
}

static void
do_co_reg (str, flags)
     char *str;
     unsigned long flags;
{
  /* Co-processor register transfer.
     Format: <MCR|MRC>{cond} CP#,<expr1>,Rd,CRn,CRm{,<expr2>}  */

  while (*str == ' ')
    str++;

  if (co_proc_number (&str) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || cp_opc_expr (&str, 21, 3) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || reg_required_here (&str, 12) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || cp_reg_required_here (&str, 16) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || cp_reg_required_here (&str, 0) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == SUCCESS)
    {
      if (cp_opc_expr (&str, 5, 3) == FAIL)
	{
	  if (!inst.error)
	    inst.error = bad_args;
	  return;
	}
    }

  end_of_line (str);
  return;
}

static void
do_fp_ctrl (str, flags)
     char *str;
     unsigned long flags;
{
  /* FP control registers.
     Format: <WFS|RFS|WFC|RFC>{cond} Rn  */

  while (*str == ' ')
    str++;

  if (reg_required_here (&str, 12) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  end_of_line (str);
  return;
}

static void
do_fp_ldst (str, flags)
     char *str;
     unsigned long flags;
{
  while (*str == ' ')
    str++;

  switch (inst.suffix)
    {
    case SUFF_S:
      break;
    case SUFF_D:
      inst.instruction |= 0x00008000;
      break;
    case SUFF_E:
      inst.instruction |= 0x00400000;
      break;
    case SUFF_P:
      inst.instruction |= 0x00408000;
      break;
    default:
      abort ();
    }

  if (fp_reg_required_here (&str, 12) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || cp_address_required_here (&str) == FAIL)
    {
      if (!inst.error)
	inst.error = bad_args;
      return;
    }

  end_of_line (str);
}

static void
do_fp_dyadic (str, flags)
     char *str;
     unsigned long flags;
{
  while (*str == ' ')
    str++;

  switch (inst.suffix)
    {
    case SUFF_S:
      break;
    case SUFF_D:
      inst.instruction |= 0x00000080;
      break;
    case SUFF_E:
      inst.instruction |= 0x00080000;
      break;
    default:
      abort ();
    }

  if (fp_reg_required_here (&str, 12) == FAIL)
    {
      if (! inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || fp_reg_required_here (&str, 16) == FAIL)
    {
      if (! inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || fp_op2 (&str) == FAIL)
    {
      if (! inst.error)
	inst.error = bad_args;
      return;
    }

  inst.instruction |= flags;
  end_of_line (str);
  return;
}

static void
do_fp_monadic (str, flags)
     char *str;
     unsigned long flags;
{
  while (*str == ' ')
    str++;

  switch (inst.suffix)
    {
    case SUFF_S:
      break;
    case SUFF_D:
      inst.instruction |= 0x00000080;
      break;
    case SUFF_E:
      inst.instruction |= 0x00080000;
      break;
    default:
      abort ();
    }

  if (fp_reg_required_here (&str, 12) == FAIL)
    {
      if (! inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || fp_op2 (&str) == FAIL)
    {
      if (! inst.error)
	inst.error = bad_args;
      return;
    }

  inst.instruction |= flags;
  end_of_line (str);
  return;
}

static void
do_fp_cmp (str, flags)
     char *str;
     unsigned long flags;
{
  while (*str == ' ')
    str++;

  if (fp_reg_required_here (&str, 16) == FAIL)
    {
      if (! inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || fp_op2 (&str) == FAIL)
    {
      if (! inst.error)
	inst.error = bad_args;
      return;
    }

  inst.instruction |= flags;
  end_of_line (str);
  return;
}

static void
do_fp_from_reg (str, flags)
     char *str;
     unsigned long flags;
{
  while (*str == ' ')
    str++;

  switch (inst.suffix)
    {
    case SUFF_S:
      break;
    case SUFF_D:
      inst.instruction |= 0x00000080;
      break;
    case SUFF_E:
      inst.instruction |= 0x00080000;
      break;
    default:
      abort ();
    }

  if (fp_reg_required_here (&str, 16) == FAIL)
    {
      if (! inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || reg_required_here (&str, 12) == FAIL)
    {
      if (! inst.error)
	inst.error = bad_args;
      return;
    }

  inst.instruction |= flags;
  end_of_line (str);
  return;
}

static void
do_fp_to_reg (str, flags)
     char *str;
     unsigned long flags;
{
  while (*str == ' ')
    str++;

  if (reg_required_here (&str, 12) == FAIL)
    {
      if (! inst.error)
	inst.error = bad_args;
      return;
    }

  if (skip_past_comma (&str) == FAIL
      || fp_reg_required_here (&str, 0) == FAIL)
    {
      if (! inst.error)
	inst.error = bad_args;
      return;
    }

  inst.instruction |= flags;
  end_of_line (str);
  return;
}

static void
insert_reg (entry)
     int entry;
{
  int len = strlen (reg_table[entry].name) + 2;
  char *buf = (char *) xmalloc (len);
  char *buf2 = (char *) xmalloc (len);
  int i = 0;

#ifdef REGISTER_PREFIX
  buf[i++] = REGISTER_PREFIX;
#endif

  strcpy (buf + i, reg_table[entry].name);

  for (i = 0; buf[i]; i++)
    buf2[i] = islower (buf[i]) ? toupper (buf[i]) : buf[i];

  buf2[i] = '\0';

  hash_insert (arm_reg_hsh, buf, (PTR) &reg_table[entry]);
  hash_insert (arm_reg_hsh, buf2, (PTR) &reg_table[entry]);
}

static void
insert_reg_alias (str, regnum)
     char *str;
     int regnum;
{
  struct reg_entry *new =
    (struct reg_entry *)xmalloc (sizeof (struct reg_entry));
  char *name = xmalloc (strlen (str) + 1);
  strcpy (name, str);

  new->name = name;
  new->number = regnum;

  hash_insert (arm_reg_hsh, name, (PTR) new);
}

static void
set_constant_flonums ()
{
  int i;

  for (i = 0; i < NUM_FLOAT_VALS; i++)
    if (atof_ieee ((char *)fp_const[i], 'x', fp_values[i]) == NULL)
      abort ();
}

void
md_begin ()
{
  int i;

  if ((arm_ops_hsh = hash_new ()) == NULL
      || (arm_cond_hsh = hash_new ()) == NULL
      || (arm_flg_hsh = hash_new ()) == NULL
      || (arm_shift_hsh = hash_new ()) == NULL
      || (arm_reg_hsh = hash_new ()) == NULL)
    as_fatal ("Virtual memory exhausted");
    
  for (i = 1; i < sizeof (insns) / sizeof (struct asm_opcode); i++)
    hash_insert (arm_ops_hsh, insns[i].template, (PTR) (insns + i));
  for (i = 1; i < sizeof (conds) / sizeof (struct asm_cond); i++)
    hash_insert (arm_cond_hsh, conds[i].template, (PTR) (conds + i));
  for (i = 1; i < sizeof (flags) / sizeof (struct asm_flg); i++)
    hash_insert (arm_flg_hsh, flags[i].template, (PTR) (flags + i));
  for (i = 1; i < sizeof (shift) / sizeof (struct asm_shift); i++)
    hash_insert (arm_shift_hsh, shift[i].template, (PTR) (shift + i));

  for (i = 0; reg_table[i].name; i++)
    insert_reg (i);

  set_constant_flonums ();
}

/* This funciton is called once, before the assembler exits.  It is
   supposed to do any final cleanup for this part of the assembler.
   */
void
md_end ()
{
}

/* Turn an integer of n bytes (in val) into a stream of bytes appropriate
   for use in the a.out file, and stores them in the array pointed to by buf.
   This knows about the endian-ness of the target machine and does
   THE RIGHT THING, whatever it is.  Possible values for n are 1 (byte)
   2 (short) and 4 (long)  Floating numbers are put out as a series of
   LITTLENUMS (shorts, here at least)
   */
void
md_number_to_chars (buf, val, n)
     char *buf;
     valueT val;
     int n;
{
  number_to_chars_littleendian (buf, val, n);
}

/*
   This is identical to the md_atof in m68k.c.  I think this is right,
   but I'm not sure.

   Turn a string in input_line_pointer into a floating point constant of type
   type, and store the appropriate bytes in *litP.  The number of LITTLENUMS
   emitted is stored in *sizeP .  An error message is returned, or NULL on OK.
   */
char *
md_atof (type, litP, sizeP)
     char type;
     char *litP;
     int *sizeP;
{
  int prec;
  LITTLENUM_TYPE words[MAX_LITTLENUMS];
  LITTLENUM_TYPE *wordP;
  char *t;
  char *atof_ieee ();

  switch (type)
    {

    case 'f':
    case 'F':
    case 's':
    case 'S':
      prec = 2;
      break;

    case 'd':
    case 'D':
    case 'r':
    case 'R':
      prec = 4;
      break;

    case 'x':
    case 'X':
      prec = 6;
      break;

    case 'p':
    case 'P':
      prec = 6;
      break;

    default:
      *sizeP = 0;
      return "Bad call to MD_ATOF()";
    }
  t = atof_ieee (input_line_pointer, type, words);
  if (t)
    input_line_pointer = t;
  *sizeP = prec * sizeof (LITTLENUM_TYPE);
  for (wordP = words; prec--;)
    {
      md_number_to_chars (litP, (valueT) (*wordP++), sizeof (LITTLENUM_TYPE));
      litP += sizeof (LITTLENUM_TYPE);
    }
  return 0;
}

/* We have already put the pipeline compensation in the instruction */

long
md_pcrel_from (fixP)
     fixS *fixP;
{
  if (fixP->fx_addsy && S_GET_SEGMENT (fixP->fx_addsy) == undefined_section
      && fixP->fx_subsy == NULL)
    return 0;	/* HACK */

  return fixP->fx_where + fixP->fx_frag->fr_address;
}

/* Round up a section size to the appropriate boundary. */
valueT
md_section_align (segment, size)
     segT segment;
     valueT size;
{
  /* Round all sects to multiple of 4 */
  return (size + 3) & ~3;
}

/* We have no need to default values of symbols.  */

/* ARGSUSED */
symbolS *
md_undefined_symbol (name)
     char *name;
{
  return 0;
}

/* arm_reg_parse () := if it looks like a register, return its token and 
   advance the pointer. */

static int
arm_reg_parse (ccp)
     register char **ccp;
{
  char *start = *ccp;
  char c;
  char *p;
  struct reg_entry *reg;

#ifdef REGISTER_PREFIX
  if (*start != REGISTER_PREFIX)
    return FAIL;
  p = start + 1;
#else
  p = start;
#ifdef OPTIONAL_REGISTER_PREFIX
  if (*p == OPTIONAL_REGISTER_PREFIX)
    p++, start++;
#endif
#endif
  if (!isalpha (*p) || !is_name_beginner (*p))
    return FAIL;

  c = *p++;
  while (isalpha (c) || isdigit (c) || c == '_')
    c = *p++;

  *--p = 0;
  reg = (struct reg_entry *) hash_find (arm_reg_hsh, start);
  *p = c;
  
  if (reg)
    {
      *ccp = p;
      return reg->number;
    }

  return FAIL;
}
    
/* Parse an operand that is machine-specific.
   We just return without modifying the expression if we have nothing
   to do.  */

/* ARGSUSED */
void
md_operand (expressionP)
     expressionS *expressionP;
{
}

int
md_apply_fix (fixP, val)
     fixS *fixP;
     valueT *val;
{
  offsetT value = *val;
  int sign;
  char *buf = fixP->fx_where + fixP->fx_frag->fr_literal;

  assert (fixP->fx_r_type < BFD_RELOC_UNUSED);

  fixP->fx_addnumber = value;	/* Remember value for emit_reloc */

  /* Note whether this will delete the relocation.  */
  if (fixP->fx_addsy == 0 && !fixP->fx_pcrel)
    fixP->fx_done = 1;

  switch (fixP->fx_r_type)
    {
    case BFD_RELOC_ARM_IMMEDIATE:
      value = validate_immediate (value);
      if (value == -1)
	{
	  as_bad_where (fixP->fx_file, fixP->fx_line,
			"invalid constant after fixup\n");
	  break;
	}
      buf[0] = value & 0xff;
      buf[1] = (buf[1] & 0xf0) | ((value >> 8) & 0x0f);
      break;

    case BFD_RELOC_ARM_OFFSET_IMM:
      sign = value >= 0;
      value = validate_offset_imm (value);
      if (value < 0)
	value = -value;
      buf[0] = value & 0xff;
      buf[1] = (buf[1] & 0xf0) | ((value >> 8) & 0x0f);
      buf[2] |= sign ? 0x80 : 0;
      break;

    case BFD_RELOC_ARM_SHIFT_IMM:
      if (value < 0 || value > 32
	  || (value == 32 
	      && (((*buf & 0x60) == 0) || (*buf & 0x60) == 0x60)))
	{
	  as_bad_where (fixP->fx_file, fixP->fx_line,
			"shift expression is too large");
	  break;
	}

      if (value == 32)
	{
	  *buf &= ~0x60;
	  break;
	}
      buf[0] |= value & 0x01 << 7;
      buf[1] |= (value >> 1) & 0x0f;
      break;

    case BFD_RELOC_ARM_SWI:
      if (value < 0 || value > 0x00ffffff)
	as_bad_where (fixP->fx_file, fixP->fx_line, "Invalid swi expression");
      buf[0] = value;
      buf[1] = value >> 8;
      buf[2] = value >> 16;
      break;

    case BFD_RELOC_ARM_MULTI:
      if (value < 0 || value > 0xffff)
	as_bad_where (fixP->fx_file, fixP->fx_line,
		      "Invalid expression in load/store multiple");
      buf[0] |= value;
      buf[1] |= value >> 8;
      break;

    case BFD_RELOC_ARM_PCREL_BRANCH:
      value = (value >> 2) & 0x00ffffff;
      value += ((unsigned int)(buf[0]) | ((unsigned int)(buf[1]) << 8) 
		| ((unsigned int)(buf[2]) << 16));
      buf[0] = value;
      buf[1] = value >> 8;
      buf[2] = value >> 16;
      break;

    case BFD_RELOC_8:
      if (fixP->fx_done || fixP->fx_pcrel)
	md_number_to_chars (buf, value, 1);
      break;

    case BFD_RELOC_16:
      if (fixP->fx_done || fixP->fx_pcrel)
	md_number_to_chars (buf, value, 2);
      break;

    case BFD_RELOC_32:
      if (fixP->fx_done || fixP->fx_pcrel)
	md_number_to_chars (buf, value, 4);
      break;

    case BFD_RELOC_ARM_CP_OFF_IMM:
      sign = value >= 0;
      if (value < -1023 || value > 1023 || (value & 3))
	as_bad_where (fixP->fx_file, fixP->fx_line,
		      "Illegal value for co-processor offset");
      if (value < 0)
	value = -value;
      buf[0] = value >> 2;
      if (sign)
	buf[2] |= 0x80;
      break;

    case BFD_RELOC_NONE:
    default:
      as_bad_where (fixP->fx_file, fixP->fx_line,
		    "Bad relocation fixup type (%d)\n", fixP->fx_r_type);
    }

  return 1;
}

/* Translate internal representation of relocation info to BFD target
   format.  */
arelent *
tc_gen_reloc (section, fixp)
     asection *section;
     fixS *fixp;
{
  arelent *reloc;
  bfd_reloc_code_real_type code;

  reloc = (arelent *) bfd_alloc_by_size_t (stdoutput, sizeof (arelent));
  assert (reloc != 0);

  reloc->sym_ptr_ptr = &fixp->fx_addsy->bsym;
  reloc->address = fixp->fx_frag->fr_address + fixp->fx_where;

  /* @@ Why fx_addnumber sometimes and fx_offset other times?  */
  if (fixp->fx_pcrel == 0)
    reloc->addend = fixp->fx_offset;
  else
    reloc->addend = fixp->fx_offset = reloc->address;

  /* @@ Why fx_addnumber sometimes and fx_offset other times?  */
  if (fixp->fx_pcrel == 0)
    reloc->addend = fixp->fx_offset;
  else
    reloc->addend = fixp->fx_offset = reloc->address;

  switch (fixp->fx_r_type)
    {
    case BFD_RELOC_8:
      if (fixp->fx_pcrel)
	{
	  code = BFD_RELOC_8_PCREL;
	  break;
	}
    case BFD_RELOC_16:
      if (fixp->fx_pcrel)
	{
	  code = BFD_RELOC_16_PCREL;
	  break;
	}
    case BFD_RELOC_32:
      if (fixp->fx_pcrel)
	{
	  code = BFD_RELOC_32_PCREL;
	  break;
	}
    case BFD_RELOC_ARM_PCREL_BRANCH:
      code = fixp->fx_r_type;
      break;
    case BFD_RELOC_ARM_IMMEDIATE:
    case BFD_RELOC_ARM_OFFSET_IMM:
    case BFD_RELOC_ARM_SHIFT_IMM:
    case BFD_RELOC_ARM_SWI:
    case BFD_RELOC_ARM_MULTI:
    case BFD_RELOC_ARM_CP_OFF_IMM:
      as_bad ("Internal_relocation (type %d) not fixed up\n", fixp->fx_r_type);
      return NULL;
    default:
      abort ();
    }
  reloc->howto = bfd_reloc_type_lookup (stdoutput, code);
  assert (reloc->howto != 0);

  return reloc;
}

CONST int md_short_jump_size = 4;
CONST int md_long_jump_size = 4;

/* These should never be called on the arm */
void
md_create_long_jump (ptr, from_addr, to_addr, frag, to_symbol)
     char *ptr;
     addressT from_addr, to_addr;
     fragS *frag;
     symbolS *to_symbol;
{
  as_fatal ("md_create_long_jump\n");
}

void
md_create_short_jump (ptr, from_addr, to_addr, frag, to_symbol)
     char *ptr;
     addressT from_addr, to_addr;
     fragS *frag;
     symbolS *to_symbol;
{
  as_fatal ("md_create_short_jump\n");
}

int
md_estimate_size_before_relax (fragP, segtype)
     fragS *fragP;
     segT segtype;
{
  as_fatal ("md_estimate_size_before_relax\n");
  return (1);
}

void
output_inst (str)
     char *str;
{
  char *to = NULL;
    
  if (inst.error)
    {
      as_bad ("%s -- statement `%s'\n", inst.error, str);
      return;
    }

  to = frag_more (INSN_SIZE);
  md_number_to_chars (to, inst.instruction, 4);

  if (inst.reloc.type != BFD_RELOC_NONE)
    fix_new_arm (frag_now, to - frag_now->fr_literal,
		 4, &inst.reloc.exp, inst.reloc.pc_rel,
		 inst.reloc.type);

  return;
}

void
md_assemble (str)
     char *str;
{
  char c;
  CONST struct asm_opcode *opcode;
  char *p, *q, *start;

  memset (&inst, '\0', sizeof (inst));
  inst.reloc.type = BFD_RELOC_NONE;

  if (*str == ' ')
    str++;			/* Skip leading white space */
    
  /* scan up to the end of the op-code, which must end in white space or
     end of string */
  for (start = p = str; *p != '\0'; p++)
    if (*p == ' ')
      break;
    
  if (p == str)
    {
      as_bad ("No operator -- statement `%s'\n", str);
      return;
    }

  /* p now points to the end of the opcode, probably white space, but we have
     to break the opcode up in case it contains condionals and flags;
     keep trying with progressively smaller basic instructions until one
     matches, or we run out of opcode. */
  q = (p - str > LONGEST_INST) ? str + LONGEST_INST : p;
  for (; q != str; q--)
    {
      c = *q;
      *q = '\0';
      opcode = (CONST struct asm_opcode *) hash_find (arm_ops_hsh, str);
      *q = c;
      if (opcode && opcode->template)
	{
	  unsigned long valid_flags = 0xffffffff;
	  unsigned long flag_bits = 0;
	  char *r;
	    
	  inst.instruction = opcode->value;
	  if (q == p)		/* Just a simple opcode */
	    {
	      if (opcode->comp_suffix != 0)
		as_bad ("Opcode `%s' must have suffix from <%s>\n", str,
			opcode->comp_suffix);
	      else
		{
		  inst.instruction |= COND_ALWAYS;
		  (*opcode->parms)(q, 0);
		}
	      output_inst (start);
	      return;
	    }

	  /* Now check for a conditional */
	  r = q;
	  if (p - r >= 2)
	    {
	      CONST struct asm_cond *cond;
	      char d = *(r + 2);
	      
	      *(r + 2) = '\0';
	      cond = (CONST struct asm_cond *) hash_find (arm_cond_hsh, r);
	      *(r + 2) = d;
	      if (cond)
		{
		  if (cond->value == 0xf0000000)
		    as_tsktsk 
		      ("Warning: Use of the 'nv' conditional is deprecated\n");
		  inst.instruction |= cond->value;
		  r += 2;
		}
	      else
		inst.instruction |= COND_ALWAYS;
	    }
	  else
	    inst.instruction |= COND_ALWAYS;

	  /* if there is a compulsory suffix, it should come here, before
	     any optional flags. */
	  if (opcode->comp_suffix)
	    {
	      CONST char *s = opcode->comp_suffix;

	      while (*s)
		{
		  inst.suffix++;
		  if (*r == *s)
		    break;
		  s++;
		}
	      
	      if (*s == '\0')
		{
		  as_bad ("Opcode `%s' must have suffix from <%s>\n", str,
			  opcode->comp_suffix);
		  return;
		}
		
	      r++;
	    }

	  /* The remainder, if any should now be flags for the instruction;
	     Scan these checking each one found with the opcode and any
	     flags previously found */
	  while (r != p)
	    {
	      char d;
	      char *s;
	      
	      s = (p - r > LONGEST_FLAG) ? r + LONGEST_FLAG : p;
	      for (; s != r; s--)
		{
		  CONST struct asm_flg *flag;
		    
		  d = *s;
		  *s = '\0';
		  flag = (CONST struct asm_flg *)hash_find (arm_flg_hsh, r);

		  *s = d;
		  if (flag 
		      && (valid_flags & (1 << (flag - flags)))
		      && opcode->flags[(flag - flags) - 1])
		    {
		      flag_bits |= opcode->flags[(flag - flags) - 1];
		      r = s;
		      valid_flags &= flag->more_flags;
		      break;
		    }
		}

	      if (!flag_bits)
		goto try_shorter;
	    }

	  (*opcode->parms) (p, flag_bits);
	  output_inst (start);
	  return;
	}

    try_shorter:
	;
    }
  /* It wasn't an instruction, but it might be a register alias of the form
     alias .req reg
     */
  q = p;
  while (*q == ' ')
    q++;

  c = *p;
  *p = '\0';
    
  if (*q && arm_reg_parse (&str) == FAIL && !strncmp (q, ".req ", 4))
    {
      char *r;
      
      q += 4;
      while (*q == ' ')
	q++;

      for (r = q; *r != '\0'; r++)
	if (*r == ' ')
	  break;

      if (r != q)
	{
	  int regnum;
	  char d = *r;

	  *r = '\0';
	  regnum = arm_reg_parse (&q);
	  *r = d;
	  if (regnum != FAIL)
	    {
	      insert_reg_alias (str, regnum);
	      *p = c;
	      return;
	    }
	}
    }
  *p = c;
  as_bad ("bad instruction `%s'", start);
}

/*
 * md_parse_option
 *      Invocation line includes a switch not recognized by the base assembler.
 *      See if it's a processor-specific option.  These are:
 *
 */

int
md_parse_option (c, arg)
     int c;
     char *arg;
{
  return 0;
}

void
md_show_usage (fp)
     FILE *fp;
{
}

/* We need to be able to fix up arbitrary expressions in some statements.
   This is so that we can handle symbols that are an arbitrary distance from
   the pc.  The most common cases are of the form ((+/-sym -/+ . - 8) & mask),
   which returns part of an address in a form which will be valid for
   a data instruction.  We do this by pushing the expression into a symbol
   in the expr_section, and creating a fix for that.  */

static void
fix_new_arm (frag, where, size, exp, pc_rel, reloc)
     fragS *frag;
     int where;
     short int size;
     expressionS *exp;
     int pc_rel;
     int reloc;
{
  fixS *new_fix;

  switch (exp->X_op)
    {
    case O_constant:
    case O_symbol:
    case O_add:
    case O_subtract:
      new_fix = fix_new_exp (frag, where, size, exp, pc_rel, reloc);
      break;

    default:
      {
	const char *fake;
	symbolS *symbolP;
	
	/* FIXME: This should be something which decode_local_label_name
	   will handle.  */
	fake = FAKE_LABEL_NAME;

	/* Putting constant symbols in absolute_section rather than
	   expr_section is convenient for the old a.out code, for which
	   S_GET_SEGMENT does not always retrieve the value put in by
	   S_SET_SEGMENT.  */
	symbolP = symbol_new (fake, expr_section, 0, &zero_address_frag);
	symbolP->sy_value = *exp;
	new_fix = fix_new (frag, where, size, symbolP, 0, pc_rel, reloc);
      }
      break;
    }

  return;
}
