/* tc-ss.c -- assemble code for a SimpleScalar virtual target.
   Copyright (C) 1993 Free Software Foundation, Inc.
   Contributed by the OSF and Ralph Campbell.
   Written by Keith Knowles and Ralph Campbell, working independently.
   Modified for ECOFF and R4000 support by Ian Lance Taylor of Cygnus
   Support.
   Heavily modified by Todd M. Austin.

   This file is part of GAS.

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

#include "as.h"
#include "config.h"
#include "subsegs.h"

#include <ctype.h>

#ifndef __STDC__
#ifndef NO_STDARG
#define NO_STDARG
#endif
#endif

#ifndef NO_STDARG
#include <stdarg.h>
#else
#ifndef NO_VARARGS
#include <varargs.h>
#endif /* NO_VARARGS */
#endif /* NO_STDARG */

#include "opcode/ss.h"

#define AT  1
#define GP  28
#define SP  29
#define FP  30
#define RA  31

/* Decide whether to do GP reference optimizations based on the object
   file format.  */
#define GPOPT

/* The default target format to use.  */
#ifdef TARGET_BYTES_BIG_ENDIAN
#define DEFAULT_TARGET_FORMAT "ss-coff-big"
#else
#define DEFAULT_TARGET_FORMAT "ss-coff-little"
#endif

const char *ss_target_format = DEFAULT_TARGET_FORMAT;

/* The name of the readonly data section.  */
#define RDATA_SECTION_NAME ".rdata"

/* These variables are filled in with the masks of registers used.
   The object format code reads them and puts them in the appropriate
   place.  */
unsigned long ss_gprmask;
unsigned long ss_cprmask[4];

static int ss_warn_about_macros;
static int ss_nomove;
static int ss_noat;
static int ss_nobopt;

#ifdef GPOPT
/* The size of the small data section.  */
static int g_switch_value = 8;
/* Whether the -G option was used.  */
static int g_switch_seen = 0;
#endif

#define N_RMASK 0xc4
#define N_VFP   0xd4

/* handle of the OPCODE hash table */
static struct hash_control *op_hash = NULL;

/* This array holds the chars that always start a comment.  If the
    pre-processor is disabled, these aren't very useful */
const char comment_chars[] = "#";

/* This array holds the chars that only start a comment at the beginning of
   a line.  If the line seems to have the form '# 123 filename'
   .line and .file directives will appear in the pre-processed output */
/* Note that input_file.c hand checks for '#' at the beginning of the
   first line of the input file.  This is because the compiler outputs
   #NO_APP at the beginning of its output. */
/* Also note that C style comments are always supported.  */
const char line_comment_chars[] = "#";

/* This array holds machine specific line separator characters. */
const char line_separator_chars[] = "";

/* Chars that can be used to separate mant from exp in floating point nums */
const char EXP_CHARS[] = "eE";

/* Chars that mean this number is a floating point constant */
/* As in 0f12.456 */
/* or    0d1.2345e12 */
const char FLT_CHARS[] = "rRsSfFdDxXpP";

/* Also be aware that MAXIMUM_NUMBER_OF_CHARS_FOR_FLOAT may have to be
   changed in read.c .  Ideally it shouldn't have to know about it at all,
   but nothing is ideal around here.
 */

static char *insn_error;

static int byte_order = BYTE_ORDER;

static int auto_align = 1;

/* This is the register which holds the stack frame, as set by the
   .frame pseudo-op.  This is needed to implement .cprestore.  */
static int ss_frame_reg = SP;

/* Since the SS does not have multiple forms of PC relative
   instructions, we do not have to do relaxing as is done on other
   platforms.  However, we do have to handle GP relative addressing
   correctly, which turns out to be a similar problem.

   Every macro that refers to a symbol can occur in (at least) two
   forms, one with GP relative addressing and one without.  For
   example, loading a global variable into a register generally uses
   a macro instruction like this:
     lw $4,i
   If i can be addressed off the GP register (this is true if it is in
   the .sbss or .sdata section, or if it is known to be smaller than
   the -G argument) this will generate the following instruction:
     lw $4,i($gp)
   This instruction will use a GPREL reloc.  If i can not be addressed
   off the GP register, the following instruction sequence will be used:
     lui $at,i
     lw $4,i($at)
   In this case the first instruction will have a HI16 reloc, and the
   second reloc will have a LO16 reloc.  Both relocs will be against
   the symbol i.

   The issue here is that we may not know whether i is GP addressable
   until after we see the instruction that uses it.  Therefore, we
   want to be able to choose the final instruction sequence only at
   the end of the assembly.  This is similar to the way other
   platforms choose the size of a PC relative instruction only at the
   end of assembly.

   When generating position independent code we do not use GP
   addressing in quite the same way, but the issue still arises as
   external symbols and local symbols must be handled differently.

   We handle these issues by actually generating both possible
   instruction sequences.  The longer one is put in a frag_var with
   type rs_machine_dependent.  We encode what to do with the frag in
   the subtype field.  We encode (1) the number of existing bytes to
   replace, (2) the number of new bytes to use, (3) the offset from
   the start of the existing bytes to the first reloc we must generate
   (that is, the offset is applied from the start of the existing
   bytes after they are replaced by the new bytes, if any), (4) the
   offset from the start of the existing bytes to the second reloc,
   (5) whether a third reloc is needed (the third reloc is always four
   bytes after the second reloc), and (6) whether to warn if this
   variant is used (this is sometimes needed if .set nomacro or .set
   noat is in effect).  All these numbers are reasonably small.

   Generating two instruction sequences must be handled carefully to
   ensure that delay slots are handled correctly.  Fortunately, there
   are a limited number of cases.  When the second instruction
   sequence is generated, append_insn is directed to maintain the
   existing delay slot information, so it continues to apply to any
   code after the second instruction sequence.  This means that the
   second instruction sequence must not impose any requirements not
   required by the first instruction sequence.

   These variant frags are then handled in functions called by the
   machine independent code.  md_estimate_size_before_relax returns
   the final size of the frag.  md_convert_frag sets up the final form
   of the frag.  tc_gen_reloc adjust the first reloc and adds a second
   one if needed.  */
#define RELAX_ENCODE(old, new, reloc1, reloc2, reloc3, warn) \
  ((relax_substateT) \
   (((old) << 24) \
    | ((new) << 16) \
    | (((reloc1) + 64) << 9) \
    | (((reloc2) + 64) << 2) \
    | ((reloc3) ? (1 << 1) : 0) \
    | ((warn) ? 1 : 0)))
#define RELAX_OLD(i) (((i) >> 24) & 0xff)
#define RELAX_NEW(i) (((i) >> 16) & 0xff)
#define RELAX_RELOC1(i) ((((i) >> 9) & 0x7f) - 64)
#define RELAX_RELOC2(i) ((((i) >> 2) & 0x7f) - 64)
#define RELAX_RELOC3(i) (((i) >> 1) & 1)
#define RELAX_WARN(i) ((i) & 1)


/* Prototypes for static functions.  */

#ifdef __STDC__
#define internalError() \
    as_fatal ("internal Error, line %d, %s", __LINE__, __FILE__)
#else
#define internalError() as_fatal ("SS internal Error");
#endif

static int insn_uses_reg PARAMS ((struct ss_cl_insn *ip,
				  unsigned int reg, int fpr));
static void append_insn PARAMS ((char *place,
				 struct ss_cl_insn * ip,
				 expressionS * p,
				 bfd_reloc_code_real_type r));
static void ss_no_prev_insn PARAMS ((void));
static void ss_emit_delays PARAMS ((void));
static void macro_build PARAMS ((char *place, int *counter, expressionS * ep,
				 const char *name, const char *fmt,
				 ...));
static void macro_build_lui PARAMS ((char *place, int *counter,
				     expressionS * ep, int regnum));
static void set_at PARAMS ((int *counter, int reg, int unsignedp));
static void check_absolute_expr PARAMS ((struct ss_cl_insn * ip,
					 expressionS *));
static void load_register PARAMS ((int *counter, int reg, expressionS * ep));
static void macro PARAMS ((struct ss_cl_insn * ip));
static void ss_ip PARAMS ((char *str, struct ss_cl_insn * ip));
static int my_getSmallExpression PARAMS ((expressionS * ep, char *str));
static void my_getExpression PARAMS ((expressionS * ep, char *str));
static symbolS *get_symbol PARAMS ((void));
static void ss_align PARAMS ((int to, int fill, symbolS *label));
static void s_align PARAMS ((int));
static void s_stringer PARAMS ((int));
static void s_change_sec PARAMS ((int));
static void s_cons PARAMS ((int));
static void s_err PARAMS ((int));
static void s_extern PARAMS ((int));
static void s_float_cons PARAMS ((int));
static void s_ss_globl PARAMS ((int));
static void s_option PARAMS ((int));
static void s_ssset PARAMS ((int));
static void s_ss_space PARAMS ((int));
static void s_abicalls PARAMS ((int));
static void s_cpload PARAMS ((int));
static void s_cprestore PARAMS ((int));
static void s_gpword PARAMS ((int));
static void s_cpadd PARAMS ((int));
#ifndef ECOFF_DEBUGGING
static void md_obj_begin PARAMS ((void));
static void md_obj_end PARAMS ((void));
static long get_number PARAMS ((void));
static void s_ent PARAMS ((int));
static void s_ssend PARAMS ((int));
static void s_file PARAMS ((int));
#endif

/* Pseudo-op table.

   The following pseudo-ops from the Kane and Heinrich MIPS book
   should be defined here, but are currently unsupported: .alias,
   .galive, .gjaldef, .gjrlive, .livereg, .noalias.

   The following pseudo-ops from the Kane and Heinrich MIPS book are
   specific to the type of debugging information being generated, and
   should be defined by the object format: .aent, .begin, .bend,
   .bgnb, .end, .endb, .ent, .fmask, .frame, .loc, .mask, .verstamp,
   .vreg.

   The following pseudo-ops from the Kane and Heinrich MIPS book are
   not MIPS CPU specific, but are also not specific to the object file
   format.  This file is probably the best place to define them, but
   they are not currently supported: .asm0, .endr, .lab, .repeat,
   .struct, .weakext.  */

const pseudo_typeS md_pseudo_table[] =
{
 /* SS specific pseudo-ops.  */
  {"option", s_option, 0},
  {"set", s_ssset, 0},
  {"rdata", s_change_sec, 'r'},
  {"sdata", s_change_sec, 's'},
  {"livereg", s_ignore, 0},
  { "abicalls", s_abicalls, 0},
  { "cpload", s_cpload, 0},
  { "cprestore", s_cprestore, 0},
  { "gpword", s_gpword, 0},
  { "cpadd", s_cpadd, 0},

 /* Relatively generic pseudo-ops that happen to be used on SS
     chips.  */
  {"asciiz", s_stringer, 1},
  {"bss", s_change_sec, 'b'},
  {"err", s_err, 0},
  {"half", s_cons, 1},
  {"dword", s_cons, 3},

 /* These pseudo-ops are defined in read.c, but must be overridden
     here for one reason or another.  */
  {"align", s_align, 0},
  {"ascii", s_stringer, 0},
  {"asciz", s_stringer, 1},
  {"byte", s_cons, 0},
  {"data", s_change_sec, 'd'},
  {"double", s_float_cons, 'd'},
  {"extern", s_extern, 0},
  {"float", s_float_cons, 'f'},
  {"globl", s_ss_globl, 0},
  {"global", s_ss_globl, 0},
  {"hword", s_cons, 1},
  {"int", s_cons, 2},
  {"long", s_cons, 2},
  {"octa", s_cons, 4},
  {"quad", s_cons, 3},
  {"short", s_cons, 1},
  {"single", s_float_cons, 'f'},
  {"space", s_ss_space, 0},
  {"text", s_change_sec, 't'},
  {"word", s_cons, 2},

#ifndef ECOFF_DEBUGGING
 /* These pseudo-ops should be defined by the object file format.
    However, a.out doesn't support them, so we have versions here.  */
  {"aent", s_ent, 1},
  {"bgnb", s_ignore, 0},
  {"end", s_ssend, 0},
  {"endb", s_ignore, 0},
  {"ent", s_ent, 0},
  {"file", s_file, 0},
  {"fmask", s_ignore, 'F'},
  {"frame", s_ignore, 0},
  {"loc", s_ignore, 0},
  {"mask", s_ignore, 'R'},
  {"verstamp", s_ignore, 0},
#endif

 /* Sentinel.  */
  {NULL}
};

static char *expr_end;

static expressionS imm_expr;
static expressionS offset_expr;
static bfd_reloc_code_real_type imm_reloc;
static bfd_reloc_code_real_type offset_reloc;

/* FIXME: This should be handled in a different way.  */
extern int target_big_endian;

/*
 * This function is called once, at assembler startup time.  It should
 * set up all the tables, etc. that the MD part of the assembler will need.
 */
void
md_begin ()
{
  register const char *retval = NULL;
  register unsigned int i = 0;

  if (!bfd_set_arch_mach (stdoutput, bfd_arch_ss, 3000))
    as_warn ("Could not set architecture and machine");

  op_hash = hash_new ();

  for (i = 0; i < NUMOPCODES;)
    {
      const char *name = ss_opcodes[i].name;

      retval = hash_insert (op_hash, name, (PTR) &ss_opcodes[i]);
      if (retval != NULL)
	{
	  fprintf (stderr, "internal error: can't hash `%s': %s\n",
		   ss_opcodes[i].name, retval);
	  as_fatal ("Broken assembler.  No assembly attempted.");
	}
      do
	{
	  if (ss_opcodes[i].pinfo != INSN_MACRO
	      && ((ss_opcodes[i].match & ss_opcodes[i].mask)
		  != ss_opcodes[i].match))
	    {
	      fprintf (stderr, "internal error: bad opcode: `%s' \"%s\"\n",
		       ss_opcodes[i].name, ss_opcodes[i].args);
	      as_fatal ("Broken assembler.  No assembly attempted.");
	    }
	  ++i;
	}
      while ((i < NUMOPCODES) && !strcmp (ss_opcodes[i].name, name));
    }

  ss_gprmask = 0;
  ss_cprmask[0] = 0;
  ss_cprmask[1] = 0;
  ss_cprmask[2] = 0;
  ss_cprmask[3] = 0;

  /* set the default alignment for the text section (2**2) */
  record_alignment (text_section, 2);

  /* FIXME: This should be handled in a different way.  */
  target_big_endian = byte_order == BYTE_ORDER;

#ifdef GPOPT
  bfd_set_gp_size (stdoutput, g_switch_value);
#endif

#ifndef ECOFF_DEBUGGING
  md_obj_begin ();
#endif
}

void
md_ss_end ()
{
#ifndef ECOFF_DEBUGGING
  md_obj_end ();
#endif
}

void
md_assemble (str)
     char *str;
{
  struct ss_cl_insn insn;

  imm_expr.X_op = O_absent;
  offset_expr.X_op = O_absent;

  ss_ip (str, &insn);
  if (insn_error)
    {
      as_bad ("%s `%s'", insn_error, str);
      return;
    }
  if (insn.insn_mo->pinfo == INSN_MACRO)
    {
      macro (&insn);
    }
  else
    {
      if (imm_expr.X_op != O_absent)
	append_insn ((char *) NULL, &insn, &imm_expr, imm_reloc);
      else if (offset_expr.X_op != O_absent)
	append_insn ((char *) NULL, &insn, &offset_expr, offset_reloc);
      else
	append_insn ((char *) NULL, &insn, NULL, BFD_RELOC_UNUSED);
    }
}

/* Output an instruction.  PLACE is where to put the instruction; if
   it is NULL, this uses frag_more to get room.  IP is the instruction
   information.  ADDRESS_EXPR is an operand of the instruction to be
   used with RELOC_TYPE.  */

static void
append_insn (place, ip, address_expr, reloc_type)
     char *place;
     struct ss_cl_insn *ip;
     expressionS *address_expr;
     bfd_reloc_code_real_type reloc_type;
{
  char *f;
  fixS *fixp;

  if (place == NULL)
    f = frag_more (8);
  else
    f = place;
  fixp = NULL;
  if (address_expr != NULL)
    {
      if (address_expr->X_op == O_constant)
	{
	  switch (reloc_type)
	    {
	    case BFD_RELOC_32:
	      ip->insn_opcode.b |= address_expr->X_add_number;
	      break;

	    case BFD_RELOC_LO16:
	      ip->insn_opcode.b |= address_expr->X_add_number & 0xffff;
	      break;

	    case BFD_RELOC_MIPS_JMP:
	    case BFD_RELOC_16_PCREL_S2:
	      goto need_reloc;

	    default:
	      internalError ();
	    }
	}
      else
	{
	  assert (reloc_type != BFD_RELOC_UNUSED);
	need_reloc:
	  /* Don't generate a reloc if we are writing into a variant
	     frag.  */
	  if (place == NULL)
	    fixp = fix_new_exp (frag_now, f - frag_now->fr_literal+4, 4,
				address_expr,
				reloc_type == BFD_RELOC_16_PCREL_S2,
				reloc_type);
	}
    }

  md_number_to_chars (f, ip->insn_opcode.a, 4);
  md_number_to_chars (f+4, ip->insn_opcode.b, 4);
}

/* Build an instruction created by a macro expansion.  This is passed
   a pointer to the count of instructions created so far, an
   expression, the name of the instruction to build, an operand format
   string, and corresponding arguments.  */

#ifndef NO_STDARG
static void
macro_build (char *place,
	     int *counter,
	     expressionS * ep,
	     const char *name,
	     const char *fmt,
	     ...)
#else /* ! defined (NO_STDARG) */
static void
macro_build (place, counter, ep, name, fmt, va_alist)
     char *place;
     int *counter;
     expressionS *ep;
     const char *name;
     const char *fmt;
     va_dcl
#endif /* ! defined (NO_STDARG) */
{
  struct ss_cl_insn insn;
  bfd_reloc_code_real_type r;
  va_list args;

#ifndef NO_STDARG
  va_start (args, fmt);
#else
  va_start (args);
#endif

  /*
   * If the macro is about to expand into a second instruction,
   * print a warning if needed. We need to pass ip as a parameter
   * to generate a better warning message here...
   */
  if (ss_warn_about_macros && place == NULL && *counter == 1)
    as_warn ("Macro instruction expanded into multiple instructions");

  if (place == NULL)
    *counter += 1;		/* bump instruction counter */

  r = BFD_RELOC_UNUSED;
  insn.insn_mo = (struct ss_opcode *) hash_find (op_hash, name);
  assert (insn.insn_mo);
  assert (strcmp (name, insn.insn_mo->name) == 0);

  while (strcmp (fmt, insn.insn_mo->args) != 0
	 || insn.insn_mo->pinfo == INSN_MACRO)
    {
      ++insn.insn_mo;
      assert (insn.insn_mo->name);
      assert (strcmp (name, insn.insn_mo->name) == 0);
    }
  insn.insn_opcode.a = insn.insn_mo->match;
  insn.insn_opcode.b = 0;
  for (;;)
    {
      switch (*fmt++)
	{
	case '\0':
	  break;

	case ',':
	case '(':
	case ')':
	case '+':
	case '-':
	case '^':
	  continue;

	case 't':
	case 'w':
	  insn.insn_opcode.b |= va_arg (args, int) << 16;
	  continue;

	case 'T':
	case 'W':
	  insn.insn_opcode.b |= va_arg (args, int) << 16;
	  continue;

	case 'c':
	  insn.insn_opcode.b |= va_arg (args, int);
	  continue;

	case 'd':
	  insn.insn_opcode.b |= va_arg (args, int) << 8;
	  continue;

	case 'V':
	case 'S':
	  insn.insn_opcode.b |= va_arg (args, int) << 24;
	  continue;

	case 'z':
	  continue;

	case '<':
	  insn.insn_opcode.b |= va_arg (args, int);
	  continue;

	case 'D':
	  insn.insn_opcode.b |= va_arg (args, int) << 8;
	  continue;

	case 'b':
	case 's':
	case 'r':
	case 'v':
	  insn.insn_opcode.b |= va_arg (args, int) << 24;
	  continue;

	case 'i':
	case 'j':
	case 'o':
	  r = (bfd_reloc_code_real_type) va_arg (args, int);
	  assert (r == BFD_RELOC_MIPS_GPREL
		  || r == BFD_RELOC_MIPS_LITERAL
		  || r == BFD_RELOC_LO16
		  || r == BFD_RELOC_MIPS_GOT16
		  || r == BFD_RELOC_MIPS_CALL16
		  || (ep->X_op == O_subtract
		      && now_seg == text_section
		      && S_GET_SEGMENT (ep->X_op_symbol) == text_section
		      && r == BFD_RELOC_PCREL_LO16));
	  continue;

	case 'u':
	  r = (bfd_reloc_code_real_type) va_arg (args, int);
	  assert (ep != NULL
		  && (ep->X_op == O_constant
		      || (ep->X_op == O_symbol
			  && (r == BFD_RELOC_HI16_S
			      || r == BFD_RELOC_HI16))
		      || (ep->X_op == O_subtract
			  && now_seg == text_section
			  && S_GET_SEGMENT (ep->X_op_symbol) == text_section
			  && r == BFD_RELOC_PCREL_HI16_S)));
	  if (ep->X_op == O_constant)
	    {
	      insn.insn_opcode.b |= (ep->X_add_number >> 16) & 0xffff;
	      ep = NULL;
	      r = BFD_RELOC_UNUSED;
	    }
	  continue;

	case 'p':
	  assert (ep != NULL);
	  /*
	   * This allows macro() to pass an immediate expression for
	   * creating short branches without creating a symbol.
	   * Note that the expression still might come from the assembly
	   * input, in which case the value is not checked for range nor
	   * is a relocation entry generated (yuck).
	   */
	  if (ep->X_op == O_constant)
	    {
	      insn.insn_opcode.b |= (ep->X_add_number >> 2) & 0xffff;
	      ep = NULL;
	    }
	  else
	    r = BFD_RELOC_16_PCREL_S2;
	  continue;

	case 'a':
	  assert (ep != NULL);
	  r = BFD_RELOC_MIPS_JMP;
	  continue;

	default:
	  internalError ();
	}
      break;
    }
  va_end (args);
  assert (r == BFD_RELOC_UNUSED ? ep == NULL : ep != NULL);

  append_insn (place, &insn, ep, r);
}

/*
 * Generate a "lui" instruction.
 */
static void
macro_build_lui (place, counter, ep, regnum)
     char *place;
     int *counter;
     expressionS *ep;
     int regnum;
{
  expressionS high_expr;
  struct ss_cl_insn insn;
  bfd_reloc_code_real_type r;
  CONST char *name = "lui";
  CONST char *fmt = "t,u";

  if (place == NULL)
    high_expr = *ep;
  else
    {
      high_expr.X_op = O_constant;
      high_expr.X_add_number = 0;
    }

  if (high_expr.X_op == O_constant)
    {
      /* we can compute the instruction now without a relocation entry */
      if (high_expr.X_add_number & 0x8000)
	high_expr.X_add_number += 0x10000;
      high_expr.X_add_number =
	((unsigned long) high_expr.X_add_number >> 16) & 0xffff;
      r = BFD_RELOC_UNUSED;
    }
  else
    {
      assert (ep->X_op == O_symbol);
      /* _gp_disp is a special case, used from s_cpload.  */
      r = BFD_RELOC_HI16_S;
    }

  /*
   * If the macro is about to expand into a second instruction,
   * print a warning if needed. We need to pass ip as a parameter
   * to generate a better warning message here...
   */
  if (ss_warn_about_macros && place == NULL && *counter == 1)
    as_warn ("Macro instruction expanded into multiple instructions");

  if (place == NULL)
    *counter += 1;		/* bump instruction counter */

  insn.insn_mo = (struct ss_opcode *) hash_find (op_hash, name);
  assert (insn.insn_mo);
  assert (strcmp (name, insn.insn_mo->name) == 0);
  assert (strcmp (fmt, insn.insn_mo->args) == 0);

  insn.insn_opcode.a = insn.insn_mo->match;
  insn.insn_opcode.b = (regnum << 16);
  if (r == BFD_RELOC_UNUSED)
    {
      insn.insn_opcode.b |= high_expr.X_add_number;
      append_insn (place, &insn, NULL, r);
    }
  else
    append_insn (place, &insn, &high_expr, r);
}

/*			set_at()
 * Generates code to set the $at register to true (one)
 * if reg is less than the immediate expression.
 */
static void
set_at (counter, reg, unsignedp)
     int *counter;
     int reg;
     int unsignedp;
{
  if (imm_expr.X_add_number >= -0x8000 && imm_expr.X_add_number < 0x8000)
    macro_build ((char *) NULL, counter, &imm_expr,
		 unsignedp ? "sltiu" : "slti",
		 "t,r,j", AT, reg, (int) BFD_RELOC_LO16);
  else
    {
      load_register (counter, AT, &imm_expr);
      macro_build ((char *) NULL, counter, NULL,
		   unsignedp ? "sltu" : "slt",
		   "d,v,t", AT, reg, AT);
    }
}

/* Warn if an expression is not a constant.  */

static void
check_absolute_expr (ip, ex)
     struct ss_cl_insn *ip;
     expressionS *ex;
{
  if (ex->X_op != O_constant)
    as_warn ("Instruction %s requires absolute expression", ip->insn_mo->name);
}

/*			load_register()
 *  This routine generates the least number of instructions neccessary to load
 *  an absolute expression value into a register.
 */
static void
load_register (counter, reg, ep)
     int *counter;
     int reg;
     expressionS *ep;
{
  assert (ep->X_op == O_constant);
  if (ep->X_add_number >= -0x8000 && ep->X_add_number < 0x8000)
    {
      /* No need to ever use daddiu here, since we are adding in
         register $zero.  */
      macro_build ((char *) NULL, counter, ep, "addiu", "t,r,j", reg, 0,
		   (int) BFD_RELOC_LO16);
    }
  else if (ep->X_add_number >= 0 && ep->X_add_number < 0x10000)
    macro_build ((char *) NULL, counter, ep, "ori", "t,r,i", reg, 0,
		 (int) BFD_RELOC_LO16);
  else if ((ep->X_add_number &~ (offsetT) 0x7fffffff) == 0
	   || ((ep->X_add_number &~ (offsetT) 0x7fffffff)
	       == ~ (offsetT) 0x7fffffff))
    {
      macro_build ((char *) NULL, counter, ep, "lui", "t,u", reg,
		   (int) BFD_RELOC_HI16);
      if ((ep->X_add_number & 0xffff) != 0)
	macro_build ((char *) NULL, counter, ep, "ori", "t,r,i", reg, reg,
		     (int) BFD_RELOC_LO16);
    }
  else
    {
      as_bad ("Number larger than 32 bits");
      macro_build ((char *) NULL, counter, ep, "addiu", "t,r,j", reg, 0,
		   (int) BFD_RELOC_LO16);
    }
}

/*
 *			Build macros
 *   This routine implements the seemingly endless macro or synthesized
 * instructions and addressing modes in the ss assembly language. Many
 * of these macros are simple and are similar to each other. These could
 * probably be handled by some kind of table or grammer aproach instead of
 * this verbose method. Others are not simple macros but are more like
 * optimizing code generation.
 *   One interesting optimization is when several store macros appear
 * consecutivly that would load AT with the upper half of the same address.
 * The ensuing load upper instructions are ommited. This implies some kind
 * of global optimization. We currently only optimize within a single macro.
 *   For many of the load and store macros if the address is specified as a
 * constant expression in the first 64k of memory (ie ld $2,0x4000c) we
 * first load register 'at' with zero and use it as the base register. The
 * ss assembler simply uses register $zero. Just one tiny optimization
 * we're missing.
 */
static void
macro (ip)
     struct ss_cl_insn *ip;
{
  register int treg, sreg, dreg, breg;
  int tempreg;
  int mask;
  int icnt = 0;
  int used_at = 0;
  expressionS expr1;
  const char *s = NULL;
  const char *s2 = NULL;
  const char *fmt = NULL;
  int dbl = 0;
  int coproc = 0;
  offsetT maxnum;
  bfd_reloc_code_real_type r;
  char *p;
  int zero = 0;

  treg = (ip->insn_opcode.b >> 16) & 0xff;
  dreg = (ip->insn_opcode.b >> 8) & 0xff;
  sreg = breg = (ip->insn_opcode.b >> 24) & 0xff;
  mask = ip->insn_mo->mask;

  expr1.X_op = O_constant;
  expr1.X_op_symbol = NULL;
  expr1.X_add_symbol = NULL;
  expr1.X_add_number = 1;

  switch (mask)
    {
    case M_MOVE_M1:
      macro_build ((char *)NULL, &icnt, NULL, "addu", "d,v,t", dreg, 0, sreg);
      return;

    case M_ADD_I:
      s = "addi";
      s2 = "add";
      goto do_addi;
    case M_ADDU_I:
      s = "addiu";
      s2 = "addu";
      goto do_addi;
    do_addi:
      if (imm_expr.X_add_number >= -0x8000 && imm_expr.X_add_number < 0x8000)
	{
	  macro_build ((char *) NULL, &icnt, &imm_expr, s, "t,r,j", treg, sreg,
		       (int) BFD_RELOC_LO16);
	  return;
	}
      load_register (&icnt, AT, &imm_expr);
      macro_build ((char *) NULL, &icnt, NULL, s2, "d,v,t", treg, sreg, AT);
      break;

    case M_AND_I:
      s = "andi";
      s2 = "and";
      goto do_bit;
    case M_OR_I:
      s = "ori";
      s2 = "or";
      goto do_bit;
    case M_NOR_I:
      s = "";
      s2 = "nor";
      goto do_bit;
    case M_XOR_I:
      s = "xori";
      s2 = "xor";
    do_bit:
      if (imm_expr.X_add_number >= 0 && imm_expr.X_add_number < 0x10000)
	{
	  if (mask != M_NOR_I)
	    macro_build ((char *) NULL, &icnt, &imm_expr, s, "t,r,i", treg,
			 sreg, (int) BFD_RELOC_LO16);
	  else
	    {
	      macro_build ((char *) NULL, &icnt, &imm_expr, "ori", "t,r,i",
			   treg, sreg, (int) BFD_RELOC_LO16);
	      macro_build ((char *) NULL, &icnt, NULL, "nor", "d,v,t",
			   treg, treg, 0);
	    }
	  return;
	}

      load_register (&icnt, AT, &imm_expr);
      macro_build ((char *) NULL, &icnt, NULL, s2, "d,v,t", treg, sreg, AT);
      break;

    case M_BEQ_I:
      s = "beq";
      goto beq_i;
    case M_BNE_I:
      s = "bne";
      goto beq_i;
    beq_i:
      if (imm_expr.X_add_number == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr, s, "s,t,p", sreg,
		       0);
	  return;
	}
      load_register (&icnt, AT, &imm_expr);
      macro_build ((char *) NULL, &icnt, &offset_expr, s, "s,t,p", sreg, AT);
      break;

    case M_BGE:
      if (treg == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "bgez", "s,p", sreg);
	  return;
	}
      if (sreg == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "blez", "s,p", treg);
	  return;
	}
      macro_build ((char *) NULL, &icnt, NULL, "slt", "d,v,t", AT, sreg, treg);
      macro_build ((char *) NULL, &icnt, &offset_expr,
		   "beq", "s,t,p", AT, 0);
      break;

    case M_BGT_I:
      /* check for > max integer */
      maxnum = 0x7fffffff;
      if (imm_expr.X_add_number >= maxnum && sizeof (maxnum) > 4)
	{
	do_false:
	  /* result is always false */
	  as_warn ("Branch %s is always false (nop)", ip->insn_mo->name);
	  macro_build ((char *) NULL, &icnt, NULL, "nop", "", 0);
	  return;
	}
      imm_expr.X_add_number++;
      /* FALLTHROUGH */
    case M_BGE_I:
      if (imm_expr.X_add_number == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "bgez", "s,p", sreg);
	  return;
	}
      if (imm_expr.X_add_number == 1)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "bgtz", "s,p", sreg);
	  return;
	}
      maxnum = 0x7fffffff;
      maxnum = - maxnum - 1;
      if (imm_expr.X_add_number <= maxnum && sizeof (maxnum) > 4)
	{
	do_true:
	  /* result is always true */
	  as_warn ("Branch %s is always true", ip->insn_mo->name);
	  macro_build ((char *) NULL, &icnt, &offset_expr, "beq", "s,t,p",
		       0, 0);
	  return;
	}
      set_at (&icnt, sreg, 0);
      macro_build ((char *) NULL, &icnt, &offset_expr,
		   "beq", "s,t,p", AT, 0);
      break;

    case M_BGEU:
      if (treg == 0)
	goto do_true;
      if (sreg == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "beq", "s,t,p", 0, treg);
	  return;
	}
      macro_build ((char *) NULL, &icnt, NULL, "sltu", "d,v,t", AT, sreg,
		   treg);
      macro_build ((char *) NULL, &icnt, &offset_expr,
		   "beq", "s,t,p", AT, 0);
      break;

    case M_BGTU_I:
      if (sreg == 0 || imm_expr.X_add_number == -1)
	goto do_false;
      imm_expr.X_add_number++;
      /* FALLTHROUGH */
    case M_BGEU_I:
      if (imm_expr.X_add_number == 0)
	goto do_true;
      if (imm_expr.X_add_number == 1)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "bne", "s,t,p", sreg, 0);
	  return;
	}
      set_at (&icnt, sreg, 1);
      macro_build ((char *) NULL, &icnt, &offset_expr,
		   "beq", "s,t,p", AT, 0);
      break;

    case M_BGT:
      if (treg == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "bgtz", "s,p", sreg);
	  return;
	}
      if (sreg == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "bltz", "s,p", treg);
	  return;
	}
      macro_build ((char *) NULL, &icnt, NULL, "slt", "d,v,t", AT, treg, sreg);
      macro_build ((char *) NULL, &icnt, &offset_expr,
		   "bne", "s,t,p", AT, 0);
      break;

    case M_BGTU:
      if (treg == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "bne", "s,t,p", sreg, 0);
	  return;
	}
      if (sreg == 0)
	goto do_false;
      macro_build ((char *) NULL, &icnt, NULL, "sltu", "d,v,t", AT, treg,
		   sreg);
      macro_build ((char *) NULL, &icnt, &offset_expr,
		   "bne", "s,t,p", AT, 0);
      break;

    case M_BLE:
      if (treg == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "blez", "s,p", sreg);
	  return;
	}
      if (sreg == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "bgez", "s,p", treg);
	  return;
	}
      macro_build ((char *) NULL, &icnt, NULL, "slt", "d,v,t", AT, treg, sreg);
      macro_build ((char *) NULL, &icnt, &offset_expr,
		   "beq", "s,t,p", AT, 0);
      break;

    case M_BLE_I:
      maxnum = 0x7fffffff;
      if (imm_expr.X_add_number >= maxnum && sizeof (maxnum) > 4)
	goto do_true;
      imm_expr.X_add_number++;
      /* FALLTHROUGH */
    case M_BLT_I:
      if (imm_expr.X_add_number == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "bltz", "s,p", sreg);
	  return;
	}
      if (imm_expr.X_add_number == 1)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "blez", "s,p", sreg);
	  return;
	}
      set_at (&icnt, sreg, 0);
      macro_build ((char *) NULL, &icnt, &offset_expr,
		   "bne", "s,t,p", AT, 0);
      break;

    case M_BLEU:
      if (treg == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "beq", "s,t,p", sreg, 0);
	  return;
	}
      if (sreg == 0)
	goto do_true;
      macro_build ((char *) NULL, &icnt, NULL, "sltu", "d,v,t", AT, treg,
		   sreg);
      macro_build ((char *) NULL, &icnt, &offset_expr,
		   "beq", "s,t,p", AT, 0);
      break;

    case M_BLEU_I:
      if (sreg == 0 || imm_expr.X_add_number == -1)
	goto do_true;
      imm_expr.X_add_number++;
      /* FALLTHROUGH */
    case M_BLTU_I:
      if (imm_expr.X_add_number == 0)
	goto do_false;
      if (imm_expr.X_add_number == 1)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "beq", "s,t,p", sreg, 0);
	  return;
	}
      set_at (&icnt, sreg, 1);
      macro_build ((char *) NULL, &icnt, &offset_expr,
		   "bne", "s,t,p", AT, 0);
      break;

    case M_BLT:
      if (treg == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "bltz", "s,p", sreg);
	  return;
	}
      if (sreg == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "bgtz", "s,p", treg);
	  return;
	}
      macro_build ((char *) NULL, &icnt, NULL, "slt", "d,v,t", AT, sreg, treg);
      macro_build ((char *) NULL, &icnt, &offset_expr,
		   "bne", "s,t,p", AT, 0);
      break;

    case M_BLTU:
      if (treg == 0)
	goto do_false;
      if (sreg == 0)
	{
	  macro_build ((char *) NULL, &icnt, &offset_expr,
		       "bne", "s,t,p", 0, treg);
	  return;
	}
      macro_build ((char *) NULL, &icnt, NULL, "sltu", "d,v,t", AT, sreg,
		   treg);
      macro_build ((char *) NULL, &icnt, &offset_expr,
		   "bne", "s,t,p", AT, 0);
      break;

    case M_DIV_3:
      s = "mflo";
      goto do_div3;
    case M_REM_3:
      s = "mfhi";
    do_div3:
      if (treg == 0)
	{
	  as_warn ("Divide by zero.");
	  macro_build ((char *) NULL, &icnt, NULL, "break", "c", 7);
	  return;
	}

      macro_build ((char *) NULL, &icnt, NULL, "div", "z,s,t", sreg, treg);
      expr1.X_add_number = 8;
      macro_build ((char *) NULL, &icnt, &expr1, "bne", "s,t,p", treg, 0);
      macro_build ((char *) NULL, &icnt, NULL, "break", "c", 7);

      expr1.X_add_number = -1;
      macro_build ((char *) NULL, &icnt, &expr1,
		   "addiu", "t,r,j", AT, 0, (int) BFD_RELOC_LO16);
      expr1.X_add_number = 24;
      macro_build ((char *) NULL, &icnt, &expr1, "bne", "s,t,p", treg, AT);
      expr1.X_add_number = 0x80000000;
      macro_build ((char *) NULL, &icnt, &expr1, "lui", "t,u", AT,
		   (int) BFD_RELOC_HI16);
      expr1.X_add_number = 8;
      macro_build ((char *) NULL, &icnt, &expr1, "bne", "s,t,p", sreg, AT);
      macro_build ((char *) NULL, &icnt, NULL, "break", "c", 6);
      macro_build ((char *) NULL, &icnt, NULL, s, "d", dreg);
      break;

    case M_DIV_3I:
      s = "div";
      s2 = "mflo";
      goto do_divi;
    case M_DIVU_3I:
      s = "divu";
      s2 = "mflo";
      goto do_divi;
    case M_REM_3I:
      s = "div";
      s2 = "mfhi";
      goto do_divi;
    case M_REMU_3I:
      s = "divu";
      s2 = "mfhi";
      goto do_divi;
    do_divi:
      if (imm_expr.X_add_number == 0)
	{
	  as_warn ("Divide by zero.");
	  macro_build ((char *) NULL, &icnt, NULL, "break", "c", 7);
	  return;
	}
      if (imm_expr.X_add_number == 1)
	{
	  if (strcmp (s2, "mflo") == 0)
	    macro_build ((char *) NULL, &icnt, NULL, "move", "d,s", dreg,
			 sreg);
	  else
	    macro_build ((char *) NULL, &icnt, NULL, "move", "d,s", dreg, 0);
	  return;
	}
      if (imm_expr.X_add_number == -1
	  && s[strlen (s) - 1] != 'u')
	{
	  if (strcmp (s2, "mflo") == 0)
	    {
	      macro_build ((char *) NULL, &icnt, NULL, "neg", "d,w", dreg,
			   sreg);
	    }
	  else
	    macro_build ((char *) NULL, &icnt, NULL, "move", "d,s", dreg, 0);
	  return;
	}

      load_register (&icnt, AT, &imm_expr);
      macro_build ((char *) NULL, &icnt, NULL, s, "z,s,t", sreg, AT);
      macro_build ((char *) NULL, &icnt, NULL, s2, "d", dreg);
      break;

    case M_DIVU_3:
      s = "divu";
      s2 = "mflo";
      goto do_divu3;
    case M_REMU_3:
      s = "divu";
      s2 = "mfhi";
      goto do_divu3;
    do_divu3:
      macro_build ((char *) NULL, &icnt, NULL, s, "z,s,t", sreg, treg);
      expr1.X_add_number = 8;
      macro_build ((char *) NULL, &icnt, &expr1, "bne", "s,t,p", treg, 0);
      macro_build ((char *) NULL, &icnt, NULL, "break", "c", 7);
      macro_build ((char *) NULL, &icnt, NULL, s2, "d", dreg);
      return;

    case M_LA_AB:
      /* Load the address of a symbol into a register.  If breg is not
	 zero, we then add a base register to it.  */

      if (offset_expr.X_op != O_symbol
	  && offset_expr.X_op != O_constant)
	{
	  as_bad ("expression too complex");
	  offset_expr.X_op = O_constant;
	}

      if (treg == breg)
	{
	  tempreg = AT;
	  used_at = 1;
	}
      else
	{
	  tempreg = treg;
	  used_at = 0;
	}

      if (offset_expr.X_op == O_constant)
	load_register (&icnt, tempreg, &offset_expr);
      else
	{
	  /* If this is a reference to an GP relative symbol, we want
	     addiu	$tempreg,$gp,<sym>	(BFD_RELOC_MIPS_GPREL)
	     Otherwise we want
	     lui	$tempreg,<sym>		(BFD_RELOC_HI16_S)
	     addiu	$tempreg,$tempreg,<sym>	(BFD_RELOC_LO16)
	     If we have a constant, we need two instructions anyhow,
	     so we may as well always use the latter form.  */
	  if (offset_expr.X_add_number != 0)
	    p = NULL;
	  else
	    {
	      frag_grow (40);
	      macro_build ((char *) NULL, &icnt, &offset_expr, "addiu",
			   "t,r,j", tempreg, GP, (int) BFD_RELOC_MIPS_GPREL);
	      p = frag_var (rs_machine_dependent, 16, 0,
			    RELAX_ENCODE (8, 16, 4, 12, 0,
					  ss_warn_about_macros),
			    offset_expr.X_add_symbol, (long) 0,
			    (char *) NULL);
	    }
	  macro_build_lui (p, &icnt, &offset_expr, tempreg);
	  if (p != NULL)
	    p += 8;
	  macro_build (p, &icnt, &offset_expr, "addiu",
		       "t,r,j", tempreg, tempreg, (int) BFD_RELOC_LO16);
	}

      if (breg != 0)
	macro_build ((char *) NULL, &icnt, (expressionS *) NULL,
		     "addu", "d,v,t", treg, tempreg, breg);

      if (! used_at)
	return;

      break;

      /* The jal instructions must be handled as macros because when
	 generating PIC code they expand to multi-instruction
	 sequences.  Normally they are simple instructions.  */
    case M_JAL_1:
      dreg = RA;
      /* Fall through.  */
    case M_JAL_2:
      macro_build ((char *) NULL, &icnt, (expressionS *) NULL, "jalr",
		   "d,s", dreg, sreg);
      return;

    case M_JAL_A:
      macro_build ((char *) NULL, &icnt, &offset_expr, "jal", "a");
      return;

    case M_LB_AB:
      s = "lb";
      fmt = "t,o(b)";
      goto ld;
    case M_LB_AB_PLUS:
      s = "lb";
      fmt = "t,o(b)+";
      goto ld;
    case M_LB_AB_MINUS:
      s = "lb";
      fmt = "t,o(b)-";
      goto ld;
    case M_LB_AB_PRE_PLUS:
      s = "lb";
      fmt = "t,o(b)^+";
      goto ld;
    case M_LB_AB_PRE_MINUS:
      s = "lb";
      fmt = "t,o(b)^-";
      goto ld;

    case M_LBU_AB:
      s = "lbu";
      fmt = "t,o(b)";
      goto ld;
    case M_LBU_AB_PLUS:
      s = "lbu";
      fmt = "t,o(b)+";
      goto ld;
    case M_LBU_AB_MINUS:
      s = "lbu";
      fmt = "t,o(b)-";
      goto ld;
    case M_LBU_AB_PRE_PLUS:
      s = "lbu";
      fmt = "t,o(b)^+";
      goto ld;
    case M_LBU_AB_PRE_MINUS:
      s = "lbu";
      fmt = "t,o(b)^-";
      goto ld;

    case M_LH_AB:
      s = "lh";
      fmt = "t,o(b)";
      goto ld;
    case M_LH_AB_PLUS:
      s = "lh";
      fmt = "t,o(b)+";
      goto ld;
    case M_LH_AB_MINUS:
      s = "lh";
      fmt = "t,o(b)-";
      goto ld;
    case M_LH_AB_PRE_PLUS:
      s = "lh";
      fmt = "t,o(b)^+";
      goto ld;
    case M_LH_AB_PRE_MINUS:
      s = "lh";
      fmt = "t,o(b)^-";
      goto ld;

    case M_LHU_AB:
      s = "lhu";
      fmt = "t,o(b)";
      goto ld;
    case M_LHU_AB_PLUS:
      s = "lhu";
      fmt = "t,o(b)+";
      goto ld;
    case M_LHU_AB_MINUS:
      s = "lhu";
      fmt = "t,o(b)-";
      goto ld;
    case M_LHU_AB_PRE_PLUS:
      s = "lhu";
      fmt = "t,o(b)^+";
      goto ld;
    case M_LHU_AB_PRE_MINUS:
      s = "lhu";
      fmt = "t,o(b)^-";
      goto ld;

    case M_LW_AB:
      s = "lw";
      fmt = "t,o(b)";
      goto ld;
    case M_LW_AB_PLUS:
      s = "lw";
      fmt = "t,o(b)+";
      goto ld;
    case M_LW_AB_MINUS:
      s = "lw";
      fmt = "t,o(b)-";
      goto ld;
    case M_LW_AB_PRE_PLUS:
      s = "lw";
      fmt = "t,o(b)^+";
      goto ld;
    case M_LW_AB_PRE_MINUS:
      s = "lw";
      fmt = "t,o(b)^-";
      goto ld;

    case M_L_S_AB:
      coproc = 1;
      s = "l.s";
      fmt = "T,o(b)";
      goto ld;
    case M_L_S_AB_PLUS:
      coproc = 1;
      s = "l.s";
      fmt = "T,o(b)+";
      goto ld;
    case M_L_S_AB_MINUS:
      coproc = 1;
      s = "l.s";
      fmt = "T,o(b)-";
      goto ld;
    case M_L_S_AB_PRE_PLUS:
      coproc = 1;
      s = "l.s";
      fmt = "T,o(b)^+";
      goto ld;
    case M_L_S_AB_PRE_MINUS:
      coproc = 1;
      s = "l.s";
      fmt = "T,o(b)^-";
      goto ld;

    case M_LWL_AB:
      s = "lwl";
      fmt = "t,o(b)";
      goto ld;
    case M_LWR_AB:
      s = "lwr";
      fmt = "t,o(b)";
      goto ld;

    ld:
      if (breg == treg || coproc)
	{
	  tempreg = AT;
	  used_at = 1;
	}
      else
	{
	  tempreg = treg;
	  used_at = 0;
	}
      goto ld_st;

    case M_SB_AB:
      s = "sb";
      fmt = "t,o(b)";
      goto st;
    case M_SB_AB_PLUS:
      s = "sb";
      fmt = "t,o(b)+";
      goto st;
    case M_SB_AB_MINUS:
      s = "sb";
      fmt = "t,o(b)-";
      goto st;
    case M_SB_AB_PRE_PLUS:
      s = "sb";
      fmt = "t,o(b)^+";
      goto st;
    case M_SB_AB_PRE_MINUS:
      s = "sb";
      fmt = "t,o(b)^-";
      goto st;

    case M_SH_AB:
      s = "sh";
      fmt = "t,o(b)";
      goto st;
    case M_SH_AB_PLUS:
      s = "sh";
      fmt = "t,o(b)+";
      goto st;
    case M_SH_AB_MINUS:
      s = "sh";
      fmt = "t,o(b)-";
      goto st;
    case M_SH_AB_PRE_PLUS:
      s = "sh";
      fmt = "t,o(b)^+";
      goto st;
    case M_SH_AB_PRE_MINUS:
      s = "sh";
      fmt = "t,o(b)^-";
      goto st;

    case M_SW_AB:
      s = "sw";
      fmt = "t,o(b)";
      goto st;
    case M_SW_AB_PLUS:
      s = "sw";
      fmt = "t,o(b)+";
      goto st;
    case M_SW_AB_MINUS:
      s = "sw";
      fmt = "t,o(b)-";
      goto st;
    case M_SW_AB_PRE_PLUS:
      s = "sw";
      fmt = "t,o(b)^+";
      goto st;
    case M_SW_AB_PRE_MINUS:
      s = "sw";
      fmt = "t,o(b)^-";
      goto st;

    case M_S_S_AB:
      coproc = 1;
      s = "s.s";
      fmt = "T,o(b)";
      goto st;
    case M_S_S_AB_PLUS:
      coproc = 1;
      s = "s.s";
      fmt = "T,o(b)+";
      goto st;
    case M_S_S_AB_MINUS:
      coproc = 1;
      s = "s.s";
      fmt = "T,o(b)-";
      goto st;
    case M_S_S_AB_PRE_PLUS:
      coproc = 1;
      s = "s.s";
      fmt = "T,o(b)^+";
      goto st;
    case M_S_S_AB_PRE_MINUS:
      coproc = 1;
      s = "s.s";
      fmt = "T,o(b)^-";
      goto st;

    case M_SWL_AB:
      s = "swl";
      fmt = "t,o(b)";
      goto st;
    case M_SWR_AB:
      s = "swr";
      fmt = "t,o(b)";
      goto st;

    st:
      tempreg = AT;
      used_at = 1;

    ld_st:
      if (offset_expr.X_op != O_constant
	  && offset_expr.X_op != O_symbol)
	{
	  as_bad ("expression too complex");
	  offset_expr.X_op = O_constant;
	}

      /* If this is a reference to a GP relative symbol, and there
	 is no base register, we want
	 <op>	$treg,<sym>($gp)	(BFD_RELOC_MIPS_GPREL)
	 Otherwise, if there is no base register, we want
	 lui	$tempreg,<sym>		(BFD_RELOC_HI16_S)
	 <op>	$treg,<sym>($tempreg)	(BFD_RELOC_LO16)
	 If we have a constant, we need two instructions anyhow,
	 so we always use the latter form.

	 If we have a base register, and this is a reference to a
	 GP relative symbol, we want
	     addu	$tempreg,$breg,$gp
	     <op>	$treg,<sym>($tempreg)	(BFD_RELOC_MIPS_GPREL)
	 Otherwise we want
	     lui	$tempreg,<sym>		(BFD_RELOC_HI16_S)
	     addu	$tempreg,$tempreg,$breg
	     <op>	$treg,<sym>($tempreg)	(BFD_RELOC_LO16)
	 With a constant we always use the latter case.  */
      if (breg == 0)
	{
	  if (offset_expr.X_add_number != 0)
	    p = NULL;
	  else
	    {
	      frag_grow (40);
	      macro_build ((char *) NULL, &icnt, &offset_expr, s, fmt,
			   treg, (int) BFD_RELOC_MIPS_GPREL, GP);
	      p = frag_var (rs_machine_dependent, 16, 0,
			    RELAX_ENCODE (8, 16, 4, 12, 0,
					  (ss_warn_about_macros
					   || (used_at && ss_noat))),
			    offset_expr.X_add_symbol, (long) 0,
			    (char *) NULL);
	      used_at = 0;
	    }
	  macro_build_lui (p, &icnt, &offset_expr, tempreg);
	  if (p != NULL)
	    p += 8;
	  macro_build (p, &icnt, &offset_expr, s, fmt, treg,
		       (int) BFD_RELOC_LO16, tempreg);
	}
      else
	{
	  if (offset_expr.X_add_number != 0)
	    p = NULL;
	  else
	    {
	      frag_grow (56);
	      macro_build ((char *) NULL, &icnt, (expressionS *) NULL,
			   "addu", "d,v,t", tempreg, breg, GP);
	      macro_build ((char *) NULL, &icnt, &offset_expr, s, fmt,
			   treg, (int) BFD_RELOC_MIPS_GPREL, tempreg);
	      p = frag_var (rs_machine_dependent, 24, 0,
			    RELAX_ENCODE (16, 24, 4, 20, 0, 0),
			    offset_expr.X_add_symbol, (long) 0,
			    (char *) NULL);
	    }
	  macro_build_lui (p, &icnt, &offset_expr, tempreg);
	  if (p != NULL)
	    p += 8;
	  macro_build (p, &icnt, (expressionS *) NULL,
		       "addu", "d,v,t", tempreg, tempreg, breg);
	  if (p != NULL)
	    p += 8;
	  macro_build (p, &icnt, &offset_expr, s, fmt, treg,
		       (int) BFD_RELOC_LO16, tempreg);
	}

      if (! used_at)
	return;
      break;

    case M_LI:
    case M_LI_S:
      load_register (&icnt, treg, &imm_expr);
      return;

    case M_LI_SS:
      if (imm_expr.X_op == O_constant)
	{
	  load_register (&icnt, AT, &imm_expr);
	  macro_build ((char *) NULL, &icnt, (expressionS *) NULL,
		       "mtc1", "t,G", AT, treg);
	  break;
	}
      else
	{
	  assert (offset_expr.X_op == O_symbol
		  && strcmp (segment_name (S_GET_SEGMENT
					   (offset_expr.X_add_symbol)),
			     ".lit4") == 0
		  && offset_expr.X_add_number == 0);
	  macro_build ((char *) NULL, &icnt, &offset_expr, "l.s", "T,o(b)",
		       treg, (int) BFD_RELOC_MIPS_LITERAL, GP);
	  return;
	}

    case M_LI_D:
      /* We know that sym is in the .rdata section.  First we get the
	 upper 16 bits of the address.  */
      /* FIXME: This won't work for a 64 bit address.  */
      macro_build_lui ((char *) NULL, &icnt, &offset_expr, AT);
	
      /* Now we load the register(s).  */
      macro_build ((char *) NULL, &icnt, &offset_expr, "dlw", "t,o(b)",
		   treg, (int) BFD_RELOC_LO16, AT);

      /* To avoid confusion in tc_gen_reloc, we must ensure that this
	 does not become a variant frag.  */
      frag_wane (frag_now);
      frag_new (0);

      break;

    case M_LI_DD:
      assert (offset_expr.X_op == O_symbol
	      && offset_expr.X_add_number == 0);
      s = segment_name (S_GET_SEGMENT (offset_expr.X_add_symbol));
      if (strcmp (s, ".lit8") == 0)
	{
	  breg = GP;
	  r = BFD_RELOC_MIPS_LITERAL;
	  goto dob;
	}
      else
	{
	  assert (strcmp (s, RDATA_SECTION_NAME) == 0);
	  /* FIXME: This won't work for a 64 bit address.  */
	  macro_build_lui ((char *) NULL, &icnt, &offset_expr, AT);

	  breg = AT;
	  r = BFD_RELOC_LO16;
	  goto dob;
	}
    dob:
      macro_build ((char *) NULL, &icnt, &offset_expr, "l.d", "T,o(b)",
		   treg, (int) r, breg);

      /* To avoid confusion in tc_gen_reloc, we must ensure that this
	 does not become a variant frag.  */
      frag_wane (frag_now);
      frag_new (0);

      if (breg != AT)
	return;
      break;

    case M_DLW_AB:
      s = "dlw";
      fmt = "t,o(b)";
      goto ldd_std;
    case M_DLW_AB_PLUS:
      s = "dlw";
      fmt = "t,o(b)+";
      goto ldd_std;
    case M_DLW_AB_MINUS:
      s = "dlw";
      fmt = "t,o(b)-";
      goto ldd_std;
    case M_DLW_AB_PRE_PLUS:
      s = "dlw";
      fmt = "t,o(b)^+";
      goto ldd_std;
    case M_DLW_AB_PRE_MINUS:
      s = "dlw";
      fmt = "t,o(b)^-";
      goto ldd_std;

    case M_L_D_AB:
      s = "l.d";
      fmt = "T,o(b)";
      goto ldd_std;
    case M_L_D_AB_PLUS:
      s = "l.d";
      fmt = "T,o(b)+";
      goto ldd_std;
    case M_L_D_AB_MINUS:
      s = "l.d";
      fmt = "T,o(b)-";
      goto ldd_std;
    case M_L_D_AB_PRE_PLUS:
      s = "l.d";
      fmt = "T,o(b)^+";
      goto ldd_std;
    case M_L_D_AB_PRE_MINUS:
      s = "l.d";
      fmt = "T,o(b)^-";
      goto ldd_std;

    case M_DSW_AB:
      s = "dsw";
      fmt = "t,o(b)";
      goto ldd_std;
    case M_DSW_AB_PLUS:
      s = "dsw";
      fmt = "t,o(b)+";
      goto ldd_std;
    case M_DSW_AB_MINUS:
      s = "dsw";
      fmt = "t,o(b)-";
      goto ldd_std;
    case M_DSW_AB_PRE_PLUS:
      s = "dsw";
      fmt = "t,o(b)^+";
      goto ldd_std;
    case M_DSW_AB_PRE_MINUS:
      s = "dsw";
      fmt = "t,o(b)^-";
      goto ldd_std;

    case M_DSZ_AB:
      s = "dsz";
      zero = 1;
      fmt = "o(b)";
      goto ldd_std;
    case M_DSZ_AB_PLUS:
      s = "dsz";
      zero = 1;
      fmt = "o(b)+";
      goto ldd_std;
    case M_DSZ_AB_MINUS:
      s = "dsz";
      zero = 1;
      fmt = "o(b)-";
      goto ldd_std;
    case M_DSZ_AB_PRE_PLUS:
      s = "dsz";
      zero = 1;
      fmt = "o(b)^+";
      goto ldd_std;
    case M_DSZ_AB_PRE_MINUS:
      s = "dsz";
      zero = 1;
      fmt = "o(b)^-";
      goto ldd_std;

    case M_S_D_AB:
      s = "s.d";
      fmt = "T,o(b)";
      goto ldd_std;
    case M_S_D_AB_PLUS:
      s = "s.d";
      fmt = "T,o(b)+";
      goto ldd_std;
    case M_S_D_AB_MINUS:
      s = "s.d";
      fmt = "T,o(b)-";
      goto ldd_std;
    case M_S_D_AB_PRE_PLUS:
      s = "s.d";
      fmt = "T,o(b)^+";
      goto ldd_std;
    case M_S_D_AB_PRE_MINUS:
      s = "s.d";
      fmt = "T,o(b)^-";
      goto ldd_std;

    ldd_std:
      if (offset_expr.X_op != O_symbol
	  && offset_expr.X_op != O_constant)
	{
	  as_bad ("expression too complex");
	  offset_expr.X_op = O_constant;
	}

      /* If this is a reference to a GP relative symbol, we want
	       <op>	$treg,<sym>($gp)	(BFD_RELOC_MIPS_GPREL)
	       <op>	$treg+1,<sym>+4($gp)	(BFD_RELOC_MIPS_GPREL)
	 If we have a base register, we use this
	       addu	$at,$breg,$gp
	       <op>	$treg,<sym>($at)	(BFD_RELOC_MIPS_GPREL)
	       <op>	$treg+1,<sym>+4($at)	(BFD_RELOC_MIPS_GPREL)
	 If this is not a GP relative symbol, we want
	       lui	$at,<sym>		(BFD_RELOC_HI16_S)
	       <op>	$treg,<sym>($at)	(BFD_RELOC_LO16)
	       <op>	$treg+1,<sym>+4($at)	(BFD_RELOC_LO16)
	 If there is a base register, we add it to $at after the
	 lui instruction.  If there is a constant, we always use
	 the last case.  */
      if (offset_expr.X_add_number != 0)
	{
	  p = NULL;
	  used_at = 1;
	}
      else
	{
	  int off;

	  if (breg == 0)
	    {
	      frag_grow (56);
	      tempreg = GP;
	      off = 4;
	      used_at = 0;
	    }
	  else
	    {
	      frag_grow (72);
	      macro_build ((char *) NULL, &icnt, (expressionS *) NULL,
			   "addu", "d,v,t", AT, breg, GP);
	      tempreg = AT;
	      off = 12;
	      used_at = 1;
	    }

	  if (zero)
	    macro_build ((char *) NULL, &icnt, &offset_expr, s, fmt,
			 (int) BFD_RELOC_MIPS_GPREL, tempreg);
	  else
	    macro_build ((char *) NULL, &icnt, &offset_expr, s, fmt,
			 treg, (int) BFD_RELOC_MIPS_GPREL, tempreg);

	  p = frag_var (rs_machine_dependent, 24 + (off - 4), 0,
			RELAX_ENCODE (8 + (off - 4), 16 + (off - 4),
				      0 + 4, 8 + off, 0,
				      used_at && ss_noat),
			offset_expr.X_add_symbol, (long) 0,
			(char *) NULL);

	  /* We just generated two relocs.  When tc_gen_reloc handles
	     this case, it will skip the first reloc and handle the
	     second.  The second reloc already has an extra addend of
	     4, which we added above.  We must subtract it out, and
	     then subtract another 4 to make the first reloc come out
	     right.  The second reloc will come out right because we
	     are going to add 4 to offset_expr when we build its
	     instruction below.  */
	  /* offset_expr.X_add_number -= 8; */
	  offset_expr.X_op = O_constant;
	}
      macro_build_lui (p, &icnt, &offset_expr, AT);
      if (p != NULL)
	p += 8;
      if (breg != 0)
	{
	  macro_build (p, &icnt, (expressionS *) NULL,
		       "addu", "d,v,t", AT, breg, AT);
	  if (p != NULL)
	    p += 8;
	}
      if (zero)
	macro_build (p, &icnt, &offset_expr, s, fmt,
		     (int) BFD_RELOC_LO16, AT);
      else
	macro_build (p, &icnt, &offset_expr, s, fmt,
		     treg, (int) BFD_RELOC_LO16, AT);

      if (! used_at)
	return;
      break;

    case M_MUL:
      macro_build ((char *) NULL, &icnt, NULL,
		   "multu", "s,t", sreg, treg);
      macro_build ((char *) NULL, &icnt, NULL, "mflo", "d", dreg);
      return;

    case M_MUL_I:
      /* The MIPS assembler some times generates shifts and adds.  I'm
	 not trying to be that fancy. GCC should do this for us
	 anyway.  */
      load_register (&icnt, AT, &imm_expr);
      macro_build ((char *) NULL, &icnt, NULL,
		   "mult", "s,t", sreg, AT);
      macro_build ((char *) NULL, &icnt, NULL, "mflo", "d", dreg);
      break;

    case M_MULO:
      macro_build ((char *) NULL, &icnt, NULL,
		   "mult", "s,t", sreg, treg);
      macro_build ((char *) NULL, &icnt, NULL, "mflo", "d", dreg);
      macro_build ((char *) NULL, &icnt, NULL,
		   "sra", "d,w,<", dreg, dreg, 31);
      macro_build ((char *) NULL, &icnt, NULL, "mfhi", "d", AT);
      expr1.X_add_number = 8;
      macro_build ((char *) NULL, &icnt, &expr1, "beq", "s,t,p", dreg, AT);
      macro_build ((char *) NULL, &icnt, NULL, "break", "c", 6);
      macro_build ((char *) NULL, &icnt, NULL, "mflo", "d", dreg);
      break;

    case M_MULOU:
      macro_build ((char *) NULL, &icnt, NULL,
		   "multu", "s,t", sreg, treg);
      macro_build ((char *) NULL, &icnt, NULL, "mfhi", "d", AT);
      macro_build ((char *) NULL, &icnt, NULL, "mflo", "d", dreg);
      expr1.X_add_number = 8;
      macro_build ((char *) NULL, &icnt, &expr1, "beq", "s,t,p", AT, 0);
      macro_build ((char *) NULL, &icnt, NULL, "break", "c", 6);
      break;

    case M_ROL:
      macro_build ((char *) NULL, &icnt, NULL, "subu", "d,v,t", AT, 0, treg);
      macro_build ((char *) NULL, &icnt, NULL, "srlv", "d,t,s", AT, sreg, AT);
      macro_build ((char *) NULL, &icnt, NULL, "sllv", "d,t,s", dreg, sreg,
		   treg);
      macro_build ((char *) NULL, &icnt, NULL, "or", "d,v,t", dreg, dreg, AT);
      break;

    case M_ROL_I:
      macro_build ((char *) NULL, &icnt, NULL, "sll", "d,w,<", AT, sreg,
		   imm_expr.X_add_number & 0x1f);
      macro_build ((char *) NULL, &icnt, NULL, "srl", "d,w,<", dreg, sreg,
		   (0 - imm_expr.X_add_number) & 0x1f);
      macro_build ((char *) NULL, &icnt, NULL, "or", "d,v,t", dreg, dreg, AT);
      break;

    case M_ROR:
      macro_build ((char *) NULL, &icnt, NULL, "subu", "d,v,t", AT, 0, treg);
      macro_build ((char *) NULL, &icnt, NULL, "sllv", "d,t,s", AT, sreg, AT);
      macro_build ((char *) NULL, &icnt, NULL, "srlv", "d,t,s", dreg, sreg,
		   treg);
      macro_build ((char *) NULL, &icnt, NULL, "or", "d,v,t", dreg, dreg, AT);
      break;

    case M_ROR_I:
      macro_build ((char *) NULL, &icnt, NULL, "srl", "d,w,<", AT, sreg,
		   imm_expr.X_add_number & 0x1f);
      macro_build ((char *) NULL, &icnt, NULL, "sll", "d,w,<", dreg, sreg,
		   (0 - imm_expr.X_add_number) & 0x1f);
      macro_build ((char *) NULL, &icnt, NULL, "or", "d,v,t", dreg, dreg, AT);
      break;

    case M_SEQ:
      if (sreg == 0)
	macro_build ((char *) NULL, &icnt, &expr1, "sltiu", "t,r,j", dreg,
		     treg, (int) BFD_RELOC_LO16);
      else if (treg == 0)
	macro_build ((char *) NULL, &icnt, &expr1, "sltiu", "t,r,j", dreg,
		     sreg, (int) BFD_RELOC_LO16);
      else
	{
	  macro_build ((char *) NULL, &icnt, NULL, "xor", "d,v,t", dreg,
		       sreg, treg);
	  macro_build ((char *) NULL, &icnt, &expr1, "sltiu", "t,r,j", dreg,
		       dreg, (int) BFD_RELOC_LO16);
	}
      return;

    case M_SEQ_I:
      if (imm_expr.X_add_number == 0)
	{
	  macro_build ((char *) NULL, &icnt, &expr1, "sltiu", "t,r,j", dreg,
		       sreg, (int) BFD_RELOC_LO16);
	  return;
	}
      if (sreg == 0)
	{
	  as_warn ("Instruction %s: result is always false",
		   ip->insn_mo->name);
	  macro_build ((char *) NULL, &icnt, NULL, "move", "d,s", dreg, 0);
	  return;
	}
      if (imm_expr.X_add_number >= 0 && imm_expr.X_add_number < 0x10000)
	{
	  macro_build ((char *) NULL, &icnt, &imm_expr, "xori", "t,r,i", dreg,
		       sreg, (int) BFD_RELOC_LO16);
	  used_at = 0;
	}
      else if (imm_expr.X_add_number > -0x8000 && imm_expr.X_add_number < 0)
	{
	  imm_expr.X_add_number = -imm_expr.X_add_number;
	  macro_build ((char *) NULL, &icnt, &imm_expr,
		       "addiu", "t,r,j", dreg, sreg,
		       (int) BFD_RELOC_LO16);
	  used_at = 0;
	}
      else
	{
	  load_register (&icnt, AT, &imm_expr);
	  macro_build ((char *) NULL, &icnt, NULL, "xor", "d,v,t", dreg,
		       sreg, AT);
	  used_at = 1;
	}
      macro_build ((char *) NULL, &icnt, &expr1, "sltiu", "t,r,j", dreg, dreg,
		   (int) BFD_RELOC_LO16);
      if (used_at)
	break;
      return;

    case M_SGE:		/* sreg >= treg <==> not (sreg < treg) */
      s = "slt";
      goto sge;
    case M_SGEU:
      s = "sltu";
    sge:
      macro_build ((char *) NULL, &icnt, NULL, s, "d,v,t", dreg, sreg, treg);
      macro_build ((char *) NULL, &icnt, &expr1, "xori", "t,r,i", dreg, dreg,
		   (int) BFD_RELOC_LO16);
      return;

    case M_SGE_I:		/* sreg >= I <==> not (sreg < I) */
    case M_SGEU_I:
      if (imm_expr.X_add_number >= -0x8000 && imm_expr.X_add_number < 0x8000)
	{
	  macro_build ((char *) NULL, &icnt, &expr1,
		       mask == M_SGE_I ? "slti" : "sltiu",
		       "t,r,j", dreg, sreg, (int) BFD_RELOC_LO16);
	  used_at = 0;
	}
      else
	{
	  load_register (&icnt, AT, &imm_expr);
	  macro_build ((char *) NULL, &icnt, NULL,
		       mask == M_SGE_I ? "slt" : "sltu",
		       "d,v,t", dreg, sreg, AT);
	  used_at = 1;
	}
      macro_build ((char *) NULL, &icnt, &expr1, "xori", "t,r,i", dreg, dreg,
		   (int) BFD_RELOC_LO16);
      if (used_at)
	break;
      return;

    case M_SGT:		/* sreg > treg  <==>  treg < sreg */
      s = "slt";
      goto sgt;
    case M_SGTU:
      s = "sltu";
    sgt:
      macro_build ((char *) NULL, &icnt, NULL, s, "d,v,t", dreg, treg, sreg);
      return;

    case M_SGT_I:		/* sreg > I  <==>  I < sreg */
      s = "slt";
      goto sgti;
    case M_SGTU_I:
      s = "sltu";
    sgti:
      load_register (&icnt, AT, &imm_expr);
      macro_build ((char *) NULL, &icnt, NULL, s, "d,v,t", dreg, AT, sreg);
      break;

    case M_SLE:		/* sreg<=treg <==> treg>=sreg <==> not (treg<sreg) */
      s = "slt";
      goto sle;
    case M_SLEU:
      s = "sltu";
    sle:
      macro_build ((char *) NULL, &icnt, NULL, s, "d,v,t", dreg, treg, sreg);
      macro_build ((char *) NULL, &icnt, &expr1, "xori", "t,r,i", dreg, dreg,
		   (int) BFD_RELOC_LO16);
      return;

    case M_SLE_I:	/* sreg <= I <==> I >= sreg <==> not (I < sreg) */
      s = "slt";
      goto slei;
    case M_SLEU_I:
      s = "sltu";
    slei:
      load_register (&icnt, AT, &imm_expr);
      macro_build ((char *) NULL, &icnt, NULL, s, "d,v,t", dreg, AT, sreg);
      macro_build ((char *) NULL, &icnt, &expr1, "xori", "t,r,i", dreg, dreg,
		   (int) BFD_RELOC_LO16);
      break;

    case M_SLT_I:
      if (imm_expr.X_add_number >= -0x8000 && imm_expr.X_add_number < 0x8000)
	{
	  macro_build ((char *) NULL, &icnt, &imm_expr, "slti", "t,r,j",
		       dreg, sreg, (int) BFD_RELOC_LO16);
	  return;
	}
      load_register (&icnt, AT, &imm_expr);
      macro_build ((char *) NULL, &icnt, NULL, "slt", "d,v,t", dreg, sreg, AT);
      break;

    case M_SLTU_I:
      if (imm_expr.X_add_number >= -0x8000 && imm_expr.X_add_number < 0x8000)
	{
	  macro_build ((char *) NULL, &icnt, &imm_expr, "sltiu", "t,r,j",
		       dreg, sreg, (int) BFD_RELOC_LO16);
	  return;
	}
      load_register (&icnt, AT, &imm_expr);
      macro_build ((char *) NULL, &icnt, NULL, "sltu", "d,v,t", dreg, sreg,
		   AT);
      break;

    case M_SNE:
      if (sreg == 0)
	macro_build ((char *) NULL, &icnt, NULL, "sltu", "d,v,t", dreg, 0,
		     treg);
      else if (treg == 0)
	macro_build ((char *) NULL, &icnt, NULL, "sltu", "d,v,t", dreg, 0,
		     sreg);
      else
	{
	  macro_build ((char *) NULL, &icnt, NULL, "xor", "d,v,t", dreg,
		       sreg, treg);
	  macro_build ((char *) NULL, &icnt, NULL, "sltu", "d,v,t", dreg, 0,
		       dreg);
	}
      return;

    case M_SNE_I:
      if (imm_expr.X_add_number == 0)
	{
	  macro_build ((char *) NULL, &icnt, NULL, "sltu", "d,v,t", dreg, 0,
		       sreg);
	  return;
	}
      if (sreg == 0)
	{
	  as_warn ("Instruction %s: result is always true",
		   ip->insn_mo->name);
	  macro_build ((char *) NULL, &icnt, &expr1,
		       "addiu", "t,r,j", dreg, 0, (int) BFD_RELOC_LO16);
	  return;
	}
      if (imm_expr.X_add_number >= 0 && imm_expr.X_add_number < 0x10000)
	{
	  macro_build ((char *) NULL, &icnt, &imm_expr, "xori", "t,r,i",
		       dreg, sreg, (int) BFD_RELOC_LO16);
	  used_at = 0;
	}
      else if (imm_expr.X_add_number > -0x8000 && imm_expr.X_add_number < 0)
	{
	  imm_expr.X_add_number = -imm_expr.X_add_number;
	  macro_build ((char *) NULL, &icnt, &imm_expr,
		       "addiu", "t,r,j", dreg, sreg, (int) BFD_RELOC_LO16);
	  used_at = 0;
	}
      else
	{
	  load_register (&icnt, AT, &imm_expr);
	  macro_build ((char *) NULL, &icnt, NULL, "xor", "d,v,t", dreg,
		       sreg, AT);
	  used_at = 1;
	}
      macro_build ((char *) NULL, &icnt, NULL, "sltu", "d,v,t", dreg, 0, dreg);
      if (used_at)
	break;
      return;

    case M_SUB_I:
      if (imm_expr.X_add_number > -0x8000 && imm_expr.X_add_number <= 0x8000)
	{
	  imm_expr.X_add_number = -imm_expr.X_add_number;
	  macro_build ((char *) NULL, &icnt, &imm_expr,
		       "addi", "t,r,j", dreg, sreg, (int) BFD_RELOC_LO16);
	  return;
	}
      load_register (&icnt, AT, &imm_expr);
      macro_build ((char *) NULL, &icnt, NULL,
		   "sub", "d,v,t", dreg, sreg, AT);
      break;

    case M_SUBU_I:
      if (imm_expr.X_add_number > -0x8000 && imm_expr.X_add_number <= 0x8000)
	{
	  imm_expr.X_add_number = -imm_expr.X_add_number;
	  macro_build ((char *) NULL, &icnt, &imm_expr,
		       "addiu", "t,r,j", dreg, sreg, (int) BFD_RELOC_LO16);
	  return;
	}
      load_register (&icnt, AT, &imm_expr);
      macro_build ((char *) NULL, &icnt, NULL,
		   "subu", "d,v,t", dreg, sreg, AT);
      break;

    case M_TRUNCWD:
    case M_TRUNCWS:
      sreg = (ip->insn_opcode.b >> 24) & 0xff;	/* floating reg */
      dreg = (ip->insn_opcode.b >> 8) & 0xff;	/* floating reg */

      /*
       * Is the double cfc1 instruction a bug in the ss assembler;
       * or is there a reason for it?
       */
      macro_build ((char *) NULL, &icnt, NULL,
	      mask == M_TRUNCWD ? "cvt.w.d" : "cvt.w.s", "D,S", dreg, sreg);
      break;

    default:
      as_bad ("Macro %s not implemented yet", ip->insn_mo->name);
      break;
    }
  if (ss_noat)
    as_warn ("Macro used $at after \".set noat\"");
}

/* parse instruction flags, of the form:
     /a-z	- set bit flag-0
     /N:M(X)	- set bits M-N with value X
   returns when the first white space is encountered
*/
static int insn_flags;

static void
ProcessInsnFlag(s)
char *s;
{
  char c;
  int bits, start, end, value;

  if (sscanf(s, "/%c/", &c) == 1) {
    if (!islower(c))
      as_fatal ("Unknown flag: `/%c'", c);
    insn_flags |= 1 << (c - 'a');
  }
  else if (sscanf(s, "/%d:%d(%d)/", &end, &start, &value) == 3) {
    if (start >= end)
      as_fatal ("Bad flag format: start >= end, '%s' (ignore trailing /)", s);
    bits = (end - start) + 1;
    if ((1 << bits) <= value)
      as_fatal ("Bad flag value: too large, '%s' (ignore trailing /)", s);
    if (value < 0)
      as_fatal ("Bad flag value: negative, '%s' (ignore trailing /)", s);
    insn_flags |= value << start;
  }
  else
      as_fatal ("Unknown flag: `%s' (ignore trailing /)", s);
}

static char *
ParseInsnFlags(s)
char *s;
{
  char arg[256], *p = arg;
  for (;;) {
    /* / */
    if (*s != '/')
      return s;
    p = arg;
    *p++ = *s++;

    /* flag */
    while (*s != '/' && *s != ' ' && *s != '\t' & *s != '\0')
      *p++ = *s++;
    *p++ = '/';

    /* real flag */
    if (arg[0] != '/' || !arg[1] || arg[1] == '/')
      as_fatal ("Unknown flag: `%s' (ignore trailing /)", arg);

    ProcessInsnFlag(arg);

    /* done? */
    if (*s != '/')
      return s;
  }
}

/*
This routine assembles an instruction into its binary format.  As a side
effect it sets one of the global variables imm_reloc or offset_reloc to the
type of relocation to do if one of the operands is an address expression.
*/
static void
ss_ip (str, ip)
     char *str;
     struct ss_cl_insn *ip;
{
  char *s, *t, opc[256];
  const char *args;
  char c;
  struct ss_opcode *insn;
  char *argsStart;
  unsigned int regno;
  unsigned int lastregno = 0;
  char *s_reset;

  insn_error = NULL;

  for (t=opc,s=str; islower (*s) || (*s>='0' && *s<='3') || *s=='.'; ++s) {
    *t++ = *s;
    continue;
  }
  switch (*s)
    {
    case '\0':
      *t = '\0';
      break;

    case ' ':
      *t = '\0';
      s++;
      break;

    default:
      as_fatal ("Unknown opcode: `%s'", opc);
    }
  if ((insn = (struct ss_opcode *) hash_find (op_hash, opc)) == NULL)
    {
      as_warn ("`%s' not in hash table.", opc);
      insn_error = "ERROR: Unrecognized opcode";
      return;
    }
  /* parse any instruction flags */
  insn_flags = 0;
  s = ParseInsnFlags(s);

  argsStart = s;
  for (;;)
    {
      assert (strcmp (insn->name, opc) == 0);

      ip->insn_mo = insn;
      ip->insn_opcode.a = (insn_flags << 16) | insn->match;
      ip->insn_opcode.b = 0;
      for (args = insn->args;; ++args)
	{
	  if (*s == ' ')
	    ++s;
	  switch (*args)
	    {
	    case '\0':		/* end of args */
	      if (*s == '\0')
		return;
	      break;

	    case ',':
	      if (*s++ == *args)
		continue;
	      s--;
	      switch (*++args)
		{
		case 'r':
		case 'v':
		  ip->insn_opcode.b |= lastregno << 24;
		  continue;

		case 'w':
		case 'W':
		  ip->insn_opcode.b |= lastregno << 16;
		  continue;

		case 'V':
		  ip->insn_opcode.b |= lastregno << 8;
		  continue;
		}
	      break;

	    case '(':
	      /* handle optional base register.
		 Either the base register is omitted or
		 we must have a left paren. */
	      /* this is dependent on the next operand specifier
		 is a 'b' for base register */
	      assert (args[1] == 'b');
	      if (*s == '\0')
		return;

	    case ')':		/* these must match exactly */
	      if (*s++ == *args)
		continue;
	      break;

	    case '<':		/* must be at least one digit */
	      /*
	       * According to the manual, if the shift amount is greater
	       * than 31 or less than 0 the the shift amount should be
	       * mod 32. In reality the ss assembler issues an error.
	       * We issue a warning and mask out all but the low 5 bits.
	       */
	      my_getExpression (&imm_expr, s);
	      check_absolute_expr (ip, &imm_expr);
	      if ((unsigned long) imm_expr.X_add_number > 31)
		{
		  as_warn ("Improper shift amount (%ld)",
			   (long) imm_expr.X_add_number);
		  imm_expr.X_add_number = imm_expr.X_add_number & 0x1f;
		}
	      ip->insn_opcode.b |= imm_expr.X_add_number;
	      imm_expr.X_op = O_absent;
	      s = expr_end;
	      continue;

	    case 'c':		/* break code */
	      my_getExpression (&imm_expr, s);
	      check_absolute_expr (ip, &imm_expr);
	      if ((unsigned) imm_expr.X_add_number > 1023)
		as_warn ("Illegal break code (%ld)",
			 (long) imm_expr.X_add_number);
	      ip->insn_opcode.b |= (imm_expr.X_add_number && 0xfffff);
	      imm_expr.X_op = O_absent;
	      s = expr_end;
	      continue;

	    case 'b':		/* base register */
	    case 'd':		/* destination register */
	    case 's':		/* source register */
	    case 't':		/* target register */
	    case 'r':		/* both target and source */
	    case 'v':		/* both dest and source */
	    case 'w':		/* both dest and target */
	    case 'E':		/* coprocessor target register */
	    case 'G':		/* coprocessor destination register */
	    case 'x':		/* ignore register name */
	    case 'z':		/* must be zero register */
	      s_reset = s;
	      if (s[0] == '$')
		{
		  if (isdigit (s[1]))
		    {
		      ++s;
		      regno = 0;
		      do
			{
			  regno *= 10;
			  regno += *s - '0';
			  ++s;
			}
		      while (isdigit (*s));
		      if (regno > 31)
			as_bad ("Invalid register number (%d)", regno);
		    }
		  else if (*args == 'E' || *args == 'G')
		    goto notreg;
		  else
		    {
		      if (s[1] == 'f' && s[2] == 'p')
			{
			  s += 3;
			  regno = FP;
			}
		      else if (s[1] == 's' && s[2] == 'p')
			{
			  s += 3;
			  regno = SP;
			}
		      else if (s[1] == 'g' && s[2] == 'p')
			{
			  s += 3;
			  regno = GP;
			}
		      else if (s[1] == 'a' && s[2] == 't')
			{
			  s += 3;
			  regno = AT;
			}
		      else
			goto notreg;
		    }
		  if (regno == AT && ! ss_noat)
		    as_warn ("Used $at without \".set noat\"");
		  c = *args;
		  if (*s == ' ')
		    s++;
		  if (args[1] != *s)
		    {
		      if (c == 'r' || c == 'v' || c == 'w')
			{
			  regno = lastregno;
			  s = s_reset;
			  args++;
			}
		    }
		  /* 'z' only matches $0.  */
		  if (c == 'z' && regno != 0)
		    break;
		  switch (c)
		    {
		    case 'r':
		    case 's':
		    case 'v':
		    case 'b':
		      ip->insn_opcode.b |= regno << 24;
		      break;
		    case 't':
		      if ((regno & 1) != 0
			  && (strcmp (opc, "dlw") == 0 ||
			      strcmp (opc, "dsw") == 0 ||
			      strcmp (opc, "dmtc1") == 0 ||
			      strcmp (opc, "dmfc1") == 0))
			{
			  as_warn ("Integer register should be even, was %d",
				   regno);
			  regno &= ~1;
			}
		      /* FALL THROUGH */
		    case 'w':
		    case 'E':
		      ip->insn_opcode.b |= regno << 16;
		      break;
		    case 'd':
		    case 'G':
		      ip->insn_opcode.b |= regno << 8;
		      break;
		    case 'x':
		      /* This case exists because on the r3000 trunc
			 expands into a macro which requires a gp
			 register.  On the r6000 or r4000 it is
			 assembled into a single instruction which
			 ignores the register.  Thus the insn version
			 is MIPS_ISA2 and uses 'x', and the macro
			 version is MIPS_ISA1 and uses 't'.  */
		      break;
		    case 'z':
		      /* This case is for the div instruction, which
			 acts differently if the destination argument
			 is $0.  This only matches $0, and is checked
			 outside the switch.  */
		      break;
		    }
		  lastregno = regno;
		  continue;
		}
	    notreg:
	      switch (*args++)
		{
		case 'r':
		case 'v':
		  ip->insn_opcode.b |= lastregno << 24;
		  continue;
		case 'w':
		  ip->insn_opcode.b |= lastregno << 16;
		  continue;
		}
	      break;

	    case 'D':		/* floating point destination register */
	    case 'S':		/* floating point source register */
	    case 'T':		/* floating point target register */
	    case 'V':
	    case 'W':
	      s_reset = s;
	      if (s[0] == '$' && s[1] == 'f' && isdigit (s[2]))
		{
		  s += 2;
		  regno = 0;
		  do
		    {
		      regno *= 10;
		      regno += *s - '0';
		      ++s;
		    }
		  while (isdigit (*s));

		  if (regno > 31)
		    as_bad ("Invalid float register number (%d)", regno);

		  if ((regno & 1) != 0
		      && ! (strcmp (opc, "mtc1") == 0 ||
			    strcmp (opc, "mfc1") == 0 ||
			    strcmp (opc, "l.s") == 0 ||
			    strcmp (opc, "s.s") == 0))
		    as_warn ("Float register should be even, was %d",
			     regno);

		  c = *args;
		  if (*s == ' ')
		    s++;
		  if (args[1] != *s)
		    {
		      if (c == 'V' || c == 'W')
			{
			  regno = lastregno;
			  s = s_reset;
			  args++;
			}
		    }
		  switch (c)
		    {
		    case 'D':
		      ip->insn_opcode.b |= regno << 8;
		      break;
		    case 'V':
		    case 'S':
		      ip->insn_opcode.b |= regno << 24;
		      break;
		    case 'W':
		    case 'T':
		      ip->insn_opcode.b |= regno << 16;
		    }
		  lastregno = regno;
		  continue;
		}
	      switch (*args++)
		{
		case 'V':
		  ip->insn_opcode.b |= lastregno << 24;
		  continue;
		case 'W':
		  ip->insn_opcode.b |= lastregno << 16;
		  continue;
		}
	      break;

	    case 'I':
	      my_getExpression (&imm_expr, s);
	      check_absolute_expr (ip, &imm_expr);
	      s = expr_end;
	      continue;

	    case 'A':
	      my_getExpression (&offset_expr, s);
	      imm_reloc = BFD_RELOC_32;
	      s = expr_end;
	      continue;

	    case 'F':
	    case 'L':
	    case 'f':
	    case 'l':
	      {
		int f64;
		char *save_in;
		char *err;
		unsigned char temp[8];
		int len;
		unsigned int length;
		segT seg;
		subsegT subseg;
		char *p;

		/* These only appear as the last operand in an
		   instruction, and every instruction that accepts
		   them in any variant accepts them in all variants.
		   This means we don't have to worry about backing out
		   any changes if the instruction does not match.

		   The difference between them is the size of the
		   floating point constant and where it goes.  For 'F'
		   and 'L' the constant is 64 bits; for 'f' and 'l' it
		   is 32 bits.  Where the constant is placed is based
		   on how the MIPS assembler does things:
		    F -- .rdata
		    L -- .lit8
		    f -- immediate value
		    l -- .lit4

		    The .lit4 and .lit8 sections are only used if
		    permitted by the -G argument.

		    When generating embedded PIC code, we use the
		    .lit8 section but not the .lit4 section (we can do
		    .lit4 inline easily; we need to put .lit8
		    somewhere in the data segment, and using .lit8
		    permits the linker to eventually combine identical
		    .lit8 entries).  */

		f64 = *args == 'F' || *args == 'L';

		save_in = input_line_pointer;
		input_line_pointer = s;
		err = md_atof (f64 ? 'd' : 'f', (char *) temp, &len);
		length = len;
		s = input_line_pointer;
		input_line_pointer = save_in;
		if (err != NULL && *err != '\0')
		  {
		    as_bad ("Bad floating point constant: %s", err);
		    memset (temp, '\0', sizeof temp);
		    length = f64 ? 8 : 4;
		  }

		assert (length == (f64 ? 8 : 4));

		if (*args == 'f'
		    || (*args == 'l'
			&& (1
#ifdef GPOPT
			    || g_switch_value < 4
#endif
			    )))
		  {
		    imm_expr.X_op = O_constant;
		    if (byte_order == LITTLE_ENDIAN)
		      imm_expr.X_add_number =
			(((((((int) temp[3] << 8)
			     | temp[2]) << 8)
			   | temp[1]) << 8)
			 | temp[0]);
		    else
		      imm_expr.X_add_number =
			(((((((int) temp[0] << 8)
			     | temp[1]) << 8)
			   | temp[2]) << 8)
			 | temp[3]);
		  }
		else
		  {
		    const char *newname;
		    segT new_seg;

		    /* Switch to the right section.  */
		    seg = now_seg;
		    subseg = now_subseg;
		    switch (*args)
		      {
		      default: /* unused default case avoids warnings.  */
		      case 'L':
			newname = ".lit8";
#ifdef GPOPT
			if (g_switch_value < 8)
			  newname = RDATA_SECTION_NAME;
#endif
			break;
		      case 'F':
			newname = RDATA_SECTION_NAME;
			break;
		      case 'l':
#ifdef GPOPT
			assert (g_switch_value >= 4);
#endif
			newname = ".lit4";
			break;
		      }
		    new_seg = subseg_new (newname, (subsegT) 0);
		    frag_align (*args == 'l' ? 2 : 3, 0);
#ifdef OBJ_ELF
		    record_alignment (new_seg, 4);
#else
		    record_alignment (new_seg, *args == 'l' ? 2 : 3);
#endif
		    if (seg == now_seg)
		      as_bad ("Can't use floating point insn in this section");

		    /* Set the argument to the current address in the
		       section.  */
		    offset_expr.X_op = O_symbol;
		    offset_expr.X_add_symbol =
		      symbol_new ("L0\001", now_seg,
				  (valueT) frag_now_fix (), frag_now);
		    offset_expr.X_add_number = 0;

		    /* Put the floating point number into the section.  */
		    p = frag_more ((int) length);
		    memcpy (p, temp, length);

		    /* Switch back to the original section.  */
		    subseg_set (seg, subseg);
		  }
	      }
	      continue;

	    case 'i':		/* 16 bit unsigned immediate */
	    case 'j':		/* 16 bit signed immediate */
	      imm_reloc = BFD_RELOC_LO16;
	      c = my_getSmallExpression (&imm_expr, s);
	      if (c)
		{
		  if (c != 'l')
		    {
		      if (imm_expr.X_op == O_constant)
			imm_expr.X_add_number =
			  (imm_expr.X_add_number >> 16) & 0xffff;
		      else if (c == 'h')
			imm_reloc = BFD_RELOC_HI16_S;
		      else
			imm_reloc = BFD_RELOC_HI16;
		    }
		}
	      else
		check_absolute_expr (ip, &imm_expr);
	      if (*args == 'i')
		{
		  if (imm_expr.X_add_number < 0
		      || imm_expr.X_add_number >= 0x10000)
		    {
		      if (insn + 1 < &ss_opcodes[NUMOPCODES] &&
			  !strcmp (insn->name, insn[1].name))
			break;
		      as_bad ("16 bit expression not in range 0..65535");
		    }
		}
	      else
		{
		  int more;
		  offsetT max;

		  /* The upper bound should be 0x8000, but
		     unfortunately the MIPS assembler accepts numbers
		     from 0x8000 to 0xffff and sign extends them, and
		     we want to be compatible.  We only permit this
		     extended range for an instruction which does not
		     provide any further alternates, since those
		     alternates may handle other cases.  People should
		     use the numbers they mean, rather than relying on
		     a mysterious sign extension.  */
		  more = (insn + 1 < &ss_opcodes[NUMOPCODES] &&
			  strcmp (insn->name, insn[1].name) == 0);
		  if (more)
		    max = 0x8000;
		  else
		    max = 0x10000;
		  if (imm_expr.X_add_number < -0x8000 ||
		      imm_expr.X_add_number >= max)
		    {
		      if (more)
			break;
		      as_bad ("16 bit expression not in range -32768..32767");
		    }
		}
	      s = expr_end;
	      continue;

	    case 'o':		/* 16 bit offset */
	      c = my_getSmallExpression (&offset_expr, s);

	      /* If this value won't fit into a 16 bit offset, then go
		 find a macro that will generate the 32 bit offset
		 code pattern.  As a special hack, we accept the
		 difference of two local symbols as a constant.  This
		 is required to suppose embedded PIC switches, which
		 use an instruction which looks like
		     lw $4,$L12-$LS12($4)
		 The problem with handling this in a more general
		 fashion is that the macro function doesn't expect to
		 see anything which can be handled in a single
		 constant instruction.  */
	      if (c == 0
		  && (offset_expr.X_op != O_constant
		      || offset_expr.X_add_number >= 0x8000
		      || offset_expr.X_add_number < -0x8000)
		  && (1 /* ss_pic != EMBEDDED_PIC */
		      || offset_expr.X_op != O_subtract
		      || now_seg != text_section
		      || (S_GET_SEGMENT (offset_expr.X_op_symbol)
			  != text_section)))
		break;

	      offset_reloc = BFD_RELOC_LO16;
	      if (c == 'h' || c == 'H')
		{
		  assert (offset_expr.X_op == O_constant);
		  offset_expr.X_add_number =
		    (offset_expr.X_add_number >> 16) & 0xffff;
		}
	      s = expr_end;
	      continue;

	    case 'p':		/* pc relative offset */
	      offset_reloc = BFD_RELOC_16_PCREL_S2;
	      my_getExpression (&offset_expr, s);
	      s = expr_end;
	      continue;

	    case 'u':		/* upper 16 bits */
	      c = my_getSmallExpression (&imm_expr, s);
	      if (imm_expr.X_op == O_constant
		  && (imm_expr.X_add_number < 0
		      || imm_expr.X_add_number >= 0x10000))
		as_bad ("lui expression not in range 0..65535");
	      imm_reloc = BFD_RELOC_LO16;
	      if (c)
		{
		  if (c != 'l')
		    {
		      if (imm_expr.X_op == O_constant)
			imm_expr.X_add_number =
			  (imm_expr.X_add_number >> 16) & 0xffff;
		      else if (c == 'h')
			imm_reloc = BFD_RELOC_HI16_S;
		      else
			imm_reloc = BFD_RELOC_HI16;
		    }
		}
	      s = expr_end;
	      continue;

	    case 'a':		/* 26 bit address */
	      my_getExpression (&offset_expr, s);
	      s = expr_end;
	      offset_reloc = BFD_RELOC_MIPS_JMP;
	      continue;

	    case '+':           /* these must match exactly */
            case '-':
            case '^':
              if (*s++ == *args)
                continue;
              break;

	    default:
	      fprintf (stderr, "bad char = '%c'\n", *args);
	      internalError ();
	    }
	  break;
	}
      /* Args don't match.  */
      if (insn + 1 < &ss_opcodes[NUMOPCODES] &&
	  !strcmp (insn->name, insn[1].name))
	{
	  ++insn;
	  s = argsStart;
	  continue;
	}
      insn_error = "ERROR: Illegal operands";
      return;
    }
}

#define LP '('
#define RP ')'

static int
my_getSmallExpression (ep, str)
     expressionS *ep;
     char *str;
{
  char *sp;
  int c = 0;

  if (*str == ' ')
    str++;
  if (*str == LP
      || (*str == '%' &&
	  ((str[1] == 'h' && str[2] == 'i')
	   || (str[1] == 'H' && str[2] == 'I')
	   || (str[1] == 'l' && str[2] == 'o'))
	  && str[3] == LP))
    {
      if (*str == LP)
	c = 0;
      else
	{
	  c = str[1];
	  str += 3;
	}

      /*
       * A small expression may be followed by a base register.
       * Scan to the end of this operand, and then back over a possible
       * base register.  Then scan the small expression up to that
       * point.  (Based on code in sparc.c...)
       */
      for (sp = str; *sp && *sp != ','; sp++)
	;
      if (sp - 4 >= str && sp[-1] == RP)
	{
	  if (isdigit (sp[-2]))
	    {
	      for (sp -= 3; sp >= str && isdigit (*sp); sp--)
		;
	      if (*sp == '$' && sp > str && sp[-1] == LP)
		{
		  sp--;
		  goto do_it;
		}
	    }
	  else if (sp - 5 >= str
		   && sp[-5] == LP
		   && sp[-4] == '$'
		   && ((sp[-3] == 'f' && sp[-2] == 'p')
		       || (sp[-3] == 's' && sp[-2] == 'p')
		       || (sp[-3] == 'g' && sp[-2] == 'p')
		       || (sp[-3] == 'a' && sp[-2] == 't')))
	    {
	      sp -= 5;
	    do_it:
	      if (sp == str)
		{
		  /* no expression means zero offset */
		  if (c)
		    {
		      /* %xx(reg) is an error */
		      ep->X_op = O_absent;
		      expr_end = str - 3;
		    }
		  else
		    {
		      ep->X_op = O_constant;
		      expr_end = sp;
		    }
		  ep->X_add_symbol = NULL;
		  ep->X_op_symbol = NULL;
		  ep->X_add_number = 0;
		}
	      else
		{
		  *sp = '\0';
		  my_getExpression (ep, str);
		  *sp = LP;
		}
	      return c;
	    }
	}
    }
  my_getExpression (ep, str);
  return c;			/* => %hi or %lo encountered */
}

static void
my_getExpression (ep, str)
     expressionS *ep;
     char *str;
{
  char *save_in;

  save_in = input_line_pointer;
  input_line_pointer = str;
  expression (ep);
  expr_end = input_line_pointer;
  input_line_pointer = save_in;
}

/* Turn a string in input_line_pointer into a floating point constant
   of type type, and store the appropriate bytes in *litP.  The number
   of LITTLENUMS emitted is stored in *sizeP .  An error message is
   returned, or NULL on OK.  */

char *
md_atof (type, litP, sizeP)
     int type;
     char *litP;
     int *sizeP;
{
  int prec;
  LITTLENUM_TYPE words[4];
  char *t;
  int i;

  switch (type)
    {
    case 'f':
      prec = 2;
      break;

    case 'd':
      prec = 4;
      break;

    default:
      *sizeP = 0;
      return "bad call to md_atof";
    }

  t = atof_ieee (input_line_pointer, type, words);
  if (t)
    input_line_pointer = t;

  *sizeP = prec * 2;

  if (byte_order == LITTLE_ENDIAN)
    {
      for (i = prec - 1; i >= 0; i--)
	{
	  md_number_to_chars (litP, (valueT) words[i], 2);
	  litP += 2;
	}
    }
  else
    {
      for (i = 0; i < prec; i++)
	{
	  md_number_to_chars (litP, (valueT) words[i], 2);
	  litP += 2;
	}
    }
     
  return NULL;
}

void
md_number_to_chars (buf, val, n)
     char *buf;
     valueT val;
     int n;
{
  switch (byte_order)
    {
    case LITTLE_ENDIAN:
      number_to_chars_littleendian (buf, val, n);
      break;

    case BIG_ENDIAN:
      number_to_chars_bigendian (buf, val, n);
      break;

    default:
      internalError ();
    }
}

#ifdef GPOPT
CONST char *md_shortopts = "O::g::G:";
#else
CONST char *md_shortopts = "O::g::";
#endif
struct option md_longopts[] = {
#define OPTION_SS1 (OPTION_MD_BASE + 1)
  {"ss0", no_argument, NULL, OPTION_SS1},
  {"ss1", no_argument, NULL, OPTION_SS1},
#define OPTION_SS2 (OPTION_MD_BASE + 2)
  {"ss2", no_argument, NULL, OPTION_SS2},
#define OPTION_SS3 (OPTION_MD_BASE + 3)
  {"ss3", no_argument, NULL, OPTION_SS3},
#define OPTION_MCPU (OPTION_MD_BASE + 4)
  {"mcpu", required_argument, NULL, OPTION_MCPU},
#define OPTION_MEMBEDDED_PIC (OPTION_MD_BASE + 5)
  {"membedded-pic", no_argument, NULL, OPTION_MEMBEDDED_PIC},
#define OPTION_TRAP (OPTION_MD_BASE + 8)
  {"trap", no_argument, NULL, OPTION_TRAP},
  {"no-break", no_argument, NULL, OPTION_TRAP},
#define OPTION_BREAK (OPTION_MD_BASE + 9)
  {"break", no_argument, NULL, OPTION_BREAK},
  {"no-trap", no_argument, NULL, OPTION_BREAK},
#define OPTION_EB (OPTION_MD_BASE + 10)
  {"EB", no_argument, NULL, OPTION_EB},
#define OPTION_EL (OPTION_MD_BASE + 11)
  {"EL", no_argument, NULL, OPTION_EL},

#ifdef OBJ_ELF
#define OPTION_CALL_SHARED (OPTION_MD_BASE + 6)
  {"KPIC", no_argument, NULL, OPTION_CALL_SHARED},
  {"call_shared", no_argument, NULL, OPTION_CALL_SHARED},
#define OPTION_NON_SHARED (OPTION_MD_BASE + 7)
  {"non_shared", no_argument, NULL, OPTION_NON_SHARED},
#endif

  {NULL, no_argument, NULL, 0}
};
size_t md_longopts_size = sizeof(md_longopts);

int
md_parse_option (c, arg)
     int c;
     char *arg;
{
  switch (c)
    {
    case OPTION_EB:
      byte_order = BIG_ENDIAN;
      ss_target_format = "ss-coff-big";
      break;

    case OPTION_EL:
      byte_order = LITTLE_ENDIAN;
      ss_target_format = "ss-coff-little";
      break;

    case 'O':
      /* ignored, always assemble the same way */
      break;

    case 'g':
      /* ignored, always assemble the same way */
      break;

#ifdef GPOPT
    case 'G':
      g_switch_value = atoi (arg);
      g_switch_seen = 1;
      break;
#endif

    default:
      return 0;
    }

  return 1;
}

void
md_show_usage (stream)
     FILE *stream;
{
  fprintf(stream, "\
SS options:\n\
-membedded-pic		generate embedded position independent code\n\
-EB			generate big endian output\n\
-EL			generate little endian output\n\
-g, -g2			do not remove uneeded NOPs or swap branches\n\
-G NUM			allow referencing objects up to NUM bytes\n\
			implicitly with the gp register [default 8]\n");
#ifdef OBJ_ELF
  fprintf(stream, "\
-KPIC, -call_shared	generate SVR4 position independent code\n\
-non_shared		do not generate position independent code\n");
#endif
}

long
md_pcrel_from (fixP)
     fixS *fixP;
{
#ifndef OBJ_AOUT
  if (fixP->fx_addsy != (symbolS *) NULL
      && ! S_IS_DEFINED (fixP->fx_addsy))
    {
      /* This makes a branch to an undefined symbol be a branch to the
	 current location.  */
      return 4;
    }
#endif

  /* return the address of the delay slot */
  return fixP->fx_size + fixP->fx_where + fixP->fx_frag->fr_address;
}

/* This is called by emit_expr via TC_CONS_FIX_NEW when creating a
   reloc for a cons.  We could use the definition there, except that
   we want to handle 64 bit relocs specially.  */

void
cons_fix_new_ss (frag, where, nbytes, exp)
     fragS *frag;
     int where;
     unsigned int nbytes;
     expressionS *exp;
{
  /* If we are assembling in 32 bit mode, turn an 8 byte reloc into a
     4 byte reloc.  
     FIXME: There is no way to select anything but 32 bit mode right
     now.  */
  if (nbytes == 8)
    {
      if (byte_order == BIG_ENDIAN)
	where += 4;
      nbytes = 4;
    }

  if (nbytes != 2 && nbytes != 4)
    as_bad ("Unsupported reloc size %d", nbytes);

  fix_new_exp (frag_now, where, (int) nbytes, exp, 0,
	       nbytes == 2 ? BFD_RELOC_16 : BFD_RELOC_32);
}

/* When generating embedded PIC code we need to use a special
   relocation to represent the difference of two symbols in the .text
   section (switch tables use a difference of this sort).  See
   include/coff/ss.h for details.  This macro checks whether this
   fixup requires the special reloc.  */
#define SWITCH_TABLE(fixp) \
  ((fixp)->fx_r_type == BFD_RELOC_32 \
   && (fixp)->fx_addsy != NULL \
   && (fixp)->fx_subsy != NULL \
   && S_GET_SEGMENT ((fixp)->fx_addsy) == text_section \
   && S_GET_SEGMENT ((fixp)->fx_subsy) == text_section)

/* When generating embedded PIC code we must keep all PC relative
   relocations, in case the linker has to relax a call.  We also need
   to keep relocations for switch table entries.  */

/*ARGSUSED*/
int
ss_force_relocation (fixp)
     fixS *fixp;
{
  return (0 /* ss_pic == EMBEDDED_PIC */
	  && (fixp->fx_pcrel
	      || SWITCH_TABLE (fixp)
	      || fixp->fx_r_type == BFD_RELOC_PCREL_HI16_S
	      || fixp->fx_r_type == BFD_RELOC_PCREL_LO16));
}

/* Apply a fixup to the object file.  */

int
md_apply_fix (fixP, valueP)
     fixS *fixP;
     valueT *valueP;
{
  unsigned char *buf;
  long insn, value;

  assert (fixP->fx_size == 4);

  value = *valueP;
  fixP->fx_addnumber = value;	/* Remember value for tc_gen_reloc */

  if (fixP->fx_addsy == NULL && ! fixP->fx_pcrel)
    fixP->fx_done = 1;

  switch (fixP->fx_r_type)
    {
    case BFD_RELOC_MIPS_JMP:
    case BFD_RELOC_HI16:
    case BFD_RELOC_HI16_S:
    case BFD_RELOC_MIPS_GPREL:
    case BFD_RELOC_MIPS_LITERAL:
    case BFD_RELOC_MIPS_CALL16:
    case BFD_RELOC_MIPS_GOT16:
    case BFD_RELOC_MIPS_GPREL32:
      if (fixP->fx_pcrel)
	as_bad_where (fixP->fx_file, fixP->fx_line,
		      "Invalid PC relative reloc");
      /* Nothing needed to do. The value comes from the reloc entry */
      break;

    case BFD_RELOC_PCREL_HI16_S:
      /* The addend for this is tricky if it is internal, so we just
	 do everything here rather than in bfd_perform_relocation.  */
      if ((fixP->fx_addsy->bsym->flags & BSF_SECTION_SYM) == 0)
	{
	  /* For an external symbol adjust by the address to make it
	     pcrel_offset.  We use the address of the RELLO reloc
	     which follows this one.  */
	  value += (fixP->fx_next->fx_frag->fr_address
		    + fixP->fx_next->fx_where);
	}
      if (value & 0x8000)
	value += 0x10000;
      value >>= 16;
      buf = (unsigned char *) fixP->fx_frag->fr_literal + fixP->fx_where;
      if (byte_order == BIG_ENDIAN)
	buf += 2;
      md_number_to_chars (buf, value, 2);
      break;

    case BFD_RELOC_PCREL_LO16:
      /* The addend for this is tricky if it is internal, so we just
	 do everything here rather than in bfd_perform_relocation.  */
      if ((fixP->fx_addsy->bsym->flags & BSF_SECTION_SYM) == 0)
	value += fixP->fx_frag->fr_address + fixP->fx_where;
      buf = (unsigned char *) fixP->fx_frag->fr_literal + fixP->fx_where;
      if (byte_order == BIG_ENDIAN)
	buf += 2;
      md_number_to_chars (buf, value, 2);
      break;

    case BFD_RELOC_32:
      /* If we are deleting this reloc entry, we must fill in the
	 value now.  This can happen if we have a .word which is not
	 resolved when it appears but is later defined.  We also need
	 to fill in the value if this is an embedded PIC switch table
	 entry.  */
      if (fixP->fx_done
	  || (0 /* ss_pic == EMBEDDED_PIC */ && SWITCH_TABLE (fixP)))
	md_number_to_chars (fixP->fx_frag->fr_literal + fixP->fx_where,
			    value, 4);
      break;

    case BFD_RELOC_LO16:
      /* When handling an embedded PIC switch statement, we can wind
	 up deleting a LO16 reloc.  See the 'o' case in ss_ip.  */
      if (fixP->fx_done)
	{
	  if (value < -0x8000 || value > 0x7fff)
	    as_bad_where (fixP->fx_file, fixP->fx_line,
			  "relocation overflow");
	  buf = (unsigned char *) fixP->fx_frag->fr_literal + fixP->fx_where;
	  if (byte_order == BIG_ENDIAN)
	    buf += 2;
	  md_number_to_chars (buf, value, 2);
	}
      break;

    case BFD_RELOC_16_PCREL_S2:
      /*
       * We need to save the bits in the instruction since fixup_segment()
       * might be deleting the relocation entry (i.e., a branch within
       * the current segment).
       */
      if (value & 0x3)
	as_warn_where (fixP->fx_file, fixP->fx_line,
		       "Branch to odd address (%lx)", value);
      value >>= 2;

      /* update old instruction data */
      buf = (unsigned char *) (fixP->fx_where + fixP->fx_frag->fr_literal);
      switch (byte_order)
	{
	case LITTLE_ENDIAN:
	  insn = (buf[3] << 24) | (buf[2] << 16) | (buf[1] << 8) | buf[0];
	  break;

	case BIG_ENDIAN:
	  insn = (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];
	  break;

	default:
	  internalError ();
	  return 0;
	}

      if (value >= -0x8000 && value < 0x8000)
	insn |= value & 0xffff;
      else
	{
	  /* The branch offset is too large.  If this is an
             unconditional branch, and we are not generating PIC code,
             we can convert it to an absolute jump instruction.  */
	  if (1 /* ss_pic == NO_PIC */
	      && fixP->fx_done
	      && fixP->fx_frag->fr_address >= text_section->vma
	      && (fixP->fx_frag->fr_address
		  < text_section->vma + text_section->_raw_size)
	      && ((insn & 0xffff0000) == 0x10000000	 /* beq $0,$0 */
		  || (insn & 0xffff0000) == 0x04010000	 /* bgez $0 */
		  || (insn & 0xffff0000) == 0x04110000)) /* bgezal $0 */
	    {
	      if ((insn & 0xffff0000) == 0x04110000)	 /* bgezal $0 */
		insn = 0x0c000000;	/* jal */
	      else
		insn = 0x08000000;	/* j */
	      fixP->fx_r_type = BFD_RELOC_MIPS_JMP;
	      fixP->fx_done = 0;
	      fixP->fx_addsy = section_symbol (text_section);
	      fixP->fx_addnumber = (value << 2) + md_pcrel_from (fixP);
	    }
	  else
	    {
	      /* FIXME.  It would be possible in principle to handle
                 conditional branches which overflow.  They could be
                 transformed into a branch around a jump.  This would
                 require setting up variant frags for each different
                 branch type.  The native MIPS assembler attempts to
                 handle these cases, but it appears to do it
                 incorrectly.  */
	      as_bad_where (fixP->fx_file, fixP->fx_line,
			    "Relocation overflow");
	    }
	}

      md_number_to_chars ((char *) buf, (valueT) insn, 4);
      break;

    default:
      internalError ();
    }

  return 1;
}

#if 0
void
printInsn (oc)
     unsigned long oc;
{
  const struct ss_opcode *p;
  int treg, sreg, dreg, shamt;
  short imm;
  const char *args;
  int i;

  for (i = 0; i < NUMOPCODES; ++i)
    {
      p = &ss_opcodes[i];
      if (((oc & p->mask) == p->match) && (p->pinfo != INSN_MACRO))
	{
	  printf ("%08lx %s\t", oc, p->name);
	  treg = (oc >> 16) & 0x1f;
	  sreg = (oc >> 21) & 0x1f;
	  dreg = (oc >> 11) & 0x1f;
	  shamt = (oc >> 6) & 0x1f;
	  imm = oc;
	  for (args = p->args;; ++args)
	    {
	      switch (*args)
		{
		case '\0':
		  printf ("\n");
		  break;

		case ',':
		case '(':
		case ')':
		  printf ("%c", *args);
		  continue;

		case 'r':
		  assert (treg == sreg);
		  printf ("$%d,$%d", treg, sreg);
		  continue;

		case 'd':
		case 'G':
		  printf ("$%d", dreg);
		  continue;

		case 't':
		case 'E':
		  printf ("$%d", treg);
		  continue;

		case 'k':
		  printf ("0x%x", treg);
		  continue;

		case 'b':
		case 's':
		  printf ("$%d", sreg);
		  continue;

		case 'a':
		  printf ("0x%08lx", oc & 0x1ffffff);
		  continue;

		case 'i':
		case 'j':
		case 'o':
		case 'u':
		  printf ("%d", imm);
		  continue;

		case '<':
		case '>':
		  printf ("$%d", shamt);
		  continue;

		default:
		  internalError ();
		}
	      break;
	    }
	  return;
	}
    }
  printf ("%08lx  UNDEFINED\n", oc);
}
#endif

static symbolS *
get_symbol ()
{
  int c;
  char *name;
  symbolS *p;

  name = input_line_pointer;
  c = get_symbol_end ();
  p = (symbolS *) symbol_find_or_make (name);
  *input_line_pointer = c;
  return p;
}

/* Align the current frag to a given power of two.  The MIPS assembler
   also automatically adjusts any preceding label.  */

static void
ss_align (to, fill, label)
     int to;
     int fill;
     symbolS *label;
{
  frag_align (to, fill);
  record_alignment (now_seg, to);
}

/* Align to a given power of two.  .align 0 turns off the automatic
   alignment used by the data creating pseudo-ops.  */

static void
s_align (x)
     int x;
{
  register int temp;
  register long temp_fill;
  long max_alignment = 15;

  /*

    o  Note that the assembler pulls down any immediately preceeding label
       to the aligned address.
    o  It's not documented but auto alignment is reinstated by
       a .align pseudo instruction.
    o  Note also that after auto alignment is turned off the ss assembler
       issues an error on attempt to assemble an improperly aligned data item.
       We don't.

    */

  temp = get_absolute_expression ();
  if (temp > max_alignment)
    as_bad ("Alignment too large: %d. assumed.", temp = max_alignment);
  else if (temp < 0)
    {
      as_warn ("Alignment negative: 0 assumed.");
      temp = 0;
    }
  if (*input_line_pointer == ',')
    {
      input_line_pointer++;
      temp_fill = get_absolute_expression ();
    }
  else
    temp_fill = 0;
  if (temp)
    {
      auto_align = 1;
      ss_align (temp, (int) temp_fill, NULL);
    }
  else
    {
      auto_align = 0;
    }

  demand_empty_rest_of_line ();
}

/* Handle .ascii and .asciiz.  This just calls stringer and forgets
   that there was a previous instruction.  */

static void
s_stringer (append_zero)
     int append_zero;
{
  stringer (append_zero);
}

static void
s_change_sec (sec)
     int sec;
{
#ifdef GPOPT
  segT seg;
#endif

  /* When generating embedded PIC code, we only use the .text, .lit8,
     .sdata and .sbss sections.  We change the .data and .rdata
     pseudo-ops to use .sdata.  */
  if (0 /* ss_pic == EMBEDDED_PIC */
      && (sec == 'd' || sec == 'r'))
    sec = 's';

  switch (sec)
    {
    case 't':
      s_text (0);
      break;
    case 'd':
      s_data (0);
      break;
    case 'b':
      subseg_set (bss_section, (subsegT) get_absolute_expression ());
      demand_empty_rest_of_line ();
      break;

    case 'r':
#ifdef GPOPT
      seg = subseg_new (RDATA_SECTION_NAME,
			(subsegT) get_absolute_expression ());
#ifdef OBJ_ELF
      bfd_set_section_flags (stdoutput, seg,
			     (SEC_ALLOC
			      | SEC_LOAD
			      | SEC_READONLY
			      | SEC_RELOC
			      | SEC_DATA));
      bfd_set_section_alignment (stdoutput, seg, 4);
#endif
      demand_empty_rest_of_line ();
      break;
#else
      as_bad ("no read-only data section for this object file format");
      demand_empty_rest_of_line ();
      return;
#endif

    case 's':
#ifdef GPOPT
      seg = subseg_new (".sdata", (subsegT) get_absolute_expression ());
#ifdef OBJ_ELF
      bfd_set_section_flags (stdoutput, seg,
			     SEC_ALLOC | SEC_LOAD | SEC_RELOC | SEC_DATA);
      bfd_set_section_alignment (stdoutput, seg, 4);
#endif
      demand_empty_rest_of_line ();
      break;
#else /* ! defined (GPOPT) */
      as_bad ("Global pointers not supported; recompile -G 0");
      demand_empty_rest_of_line ();
      return;
#endif /* ! defined (GPOPT) */
    }

  auto_align = 1;
}

#ifdef OBJ_ELF

/* Handle the ELF .section pseudo-op.  This is a wrapper around
   obj_elf_section.  */

static void
s_elf_section (x)
     int x;
{
  obj_elf_section (x);
  auto_align = 1;
}

#endif /* OBJ_ELF */

static void
s_cons (log_size)
     int log_size;
{
  symbolS *label;

  if (log_size > 0 && auto_align)
    ss_align (log_size, 0, NULL);
  cons (1 << log_size);
}

static void
s_err (x)
     int x;
{
  as_fatal ("Encountered `.err', aborting assembly");
}

static void
s_extern (x)
     int x;
{
  valueT size;
  symbolS *symbolP;

  symbolP = get_symbol ();
  if (*input_line_pointer == ',')
    input_line_pointer++;
  size = get_absolute_expression ();
  S_SET_EXTERNAL (symbolP);

#ifdef ECOFF_DEBUGGING
  symbolP->ecoff_extern_size = size;
#endif
}

static void
s_float_cons (type)
     int type;
{
  symbolS *label;

  if (auto_align)
    if (type == 'd')
      ss_align (3, 0, NULL);
    else
      ss_align (2, 0, NULL);

  float_cons (type);
}

/* Handle .globl.  We need to override it because on Irix 5 you are
   permitted to say
       .globl foo .text
   where foo is an undefined symbol, to mean that foo should be
   considered to be the address of a function.  */

static void
s_ss_globl (x)
     int x;
{
  char *name;
  int c;
  symbolS *symbolP;

  name = input_line_pointer;
  c = get_symbol_end ();
  symbolP = symbol_find_or_make (name);
  *input_line_pointer = c;
  SKIP_WHITESPACE ();
  if (! is_end_of_line[(unsigned char) *input_line_pointer])
    {
      char *secname;
      asection *sec;

      secname = input_line_pointer;
      c = get_symbol_end ();
      sec = bfd_get_section_by_name (stdoutput, secname);
      if (sec == NULL)
	as_bad ("%s: no such section", secname);
      *input_line_pointer = c;

      if (sec != NULL && (sec->flags & SEC_CODE) != 0)
	symbolP->bsym->flags |= BSF_FUNCTION;
    }

  S_SET_EXTERNAL (symbolP);
  demand_empty_rest_of_line ();
}

static void
s_option (x)
     int x;
{
  char *opt;
  char c;

  opt = input_line_pointer;
  c = get_symbol_end ();

  if (*opt == 'O')
    {
      /* FIXME: What does this mean?  */
    }
  else
    as_warn ("Unrecognized option \"%s\"", opt);

  *input_line_pointer = c;
  demand_empty_rest_of_line ();
}

static void
s_ssset (x)
     int x;
{
  char *name = input_line_pointer, ch;

  while (!is_end_of_line[(unsigned char) *input_line_pointer])
    input_line_pointer++;
  ch = *input_line_pointer;
  *input_line_pointer = '\0';

  if (strcmp (name, "reorder") == 0)
    {
    }
  else if (strcmp (name, "noreorder") == 0)
    {
    }
  else if (strcmp (name, "at") == 0)
    {
      ss_noat = 0;
    }
  else if (strcmp (name, "noat") == 0)
    {
      ss_noat = 1;
    }
  else if (strcmp (name, "macro") == 0)
    {
      ss_warn_about_macros = 0;
    }
  else if (strcmp (name, "nomacro") == 0)
    {
      ss_warn_about_macros = 1;
    }
  else if (strcmp (name, "move") == 0 || strcmp (name, "novolatile") == 0)
    {
      ss_nomove = 0;
    }
  else if (strcmp (name, "nomove") == 0 || strcmp (name, "volatile") == 0)
    {
      ss_nomove = 1;
    }
  else if (strcmp (name, "bopt") == 0)
    {
      ss_nobopt = 0;
    }
  else if (strcmp (name, "nobopt") == 0)
    {
      ss_nobopt = 1;
    }
  else
    {
      as_warn ("Tried to set unrecognized symbol: %s\n", name);
    }
  *input_line_pointer = ch;
  demand_empty_rest_of_line ();
}

/* The same as the usual .space directive, except that we have to
   forget about any previous instruction.  */

static void
s_ss_space (param)
     int param;
{
  s_space (param);
}

/* Handle the .abicalls pseudo-op.  I believe this is equivalent to
   .option pic2.  It means to generate SVR4 PIC calls.  */

static void
s_abicalls (ignore)
     int ignore;
{
  abort();
#if 0
  ss_pic = SVR4_PIC;
#ifdef GPOPT
  if (g_switch_seen && g_switch_value != 0)
    as_warn ("-G may not be used with SVR4 PIC code");
  g_switch_value = 0;
#endif
  bfd_set_gp_size (stdoutput, 0);
  demand_empty_rest_of_line ();
#endif
}

/* Handle the .cpload pseudo-op.  This is used when generating SVR4
   PIC code.  It sets the $gp register for the function based on the
   function address, which is in the register named in the argument.
   This uses a relocation against _gp_disp, which is handled specially
   by the linker.  The result is:
	lui	$gp,%hi(_gp_disp)
	addiu	$gp,$gp,%lo(_gp_disp)
	addu	$gp,$gp,.cpload argument
   The .cpload argument is normally $25 == $t9.  */

static void
s_cpload (ignore)
     int ignore;
{
  expressionS ex;
  int icnt = 0;

  /* If we are not generating SVR4 PIC code, .cpload is ignored.  */
  if (1 /* ss_pic != SVR4_PIC */)
    {
      s_ignore (0);
      return;
    }

  ex.X_op = O_symbol;
  ex.X_add_symbol = symbol_find_or_make ("_gp_disp");
  ex.X_op_symbol = NULL;
  ex.X_add_number = 0;

  macro_build_lui ((char *) NULL, &icnt, &ex, GP);
  macro_build ((char *) NULL, &icnt, &ex, "addiu", "t,r,j", GP, GP,
	       (int) BFD_RELOC_LO16);

  macro_build ((char *) NULL, &icnt, (expressionS *) NULL, "addu", "d,v,t",
	       GP, GP, tc_get_register (0));

  demand_empty_rest_of_line ();
}

/* Handle the .cprestore pseudo-op.  This stores $gp into a given
   offset from $sp.  The offset is remembered, and after making a PIC
   call $gp is restored from that location.  */

static void
s_cprestore (ignore)
     int ignore;
{
  expressionS ex;
  int icnt = 0;

  /* If we are not generating SVR4 PIC code, .cprestore is ignored.  */
  if (1 /* ss_pic != SVR4_PIC */)
    {
      s_ignore (0);
      return;
    }

  abort();
#if 0
  ss_cprestore_offset = get_absolute_expression ();

  ex.X_op = O_constant;
  ex.X_add_symbol = NULL;
  ex.X_op_symbol = NULL;
  ex.X_add_number = ss_cprestore_offset;

  macro_build ((char *) NULL, &icnt, &ex,
	       "sw", "t,o(b)", GP, (int) BFD_RELOC_LO16, SP);

  demand_empty_rest_of_line ();
#endif
}

/* Handle the .gpword pseudo-op.  This is used when generating PIC
   code.  It generates a 32 bit GP relative reloc.  */

static void
s_gpword (ignore)
     int ignore;
{
  symbolS *label;
  expressionS ex;
  char *p;

  /* When not generating PIC code, this is treated as .word.  */
  if (1 /* ss_pic != SVR4_PIC */)
    {
      s_cons (2);
      return;
    }

  if (auto_align)
    ss_align (2, 0, NULL);

  expression (&ex);

  if (ex.X_op != O_symbol || ex.X_add_number != 0)
    {
      as_bad ("Unsupported use of .gpword");
      ignore_rest_of_line ();
    }

  p = frag_more (4);
  md_number_to_chars (p, (valueT) 0, 4);
  fix_new_exp (frag_now, p - frag_now->fr_literal, 4, &ex, 0,
	       BFD_RELOC_MIPS_GPREL32);

  demand_empty_rest_of_line ();
}

/* Handle the .cpadd pseudo-op.  This is used when dealing with switch
   tables in SVR4 PIC code.  */

static void
s_cpadd (ignore)
     int ignore;
{
  int icnt = 0;
  int reg;

  /* This is ignored when not generating SVR4 PIC code.  */
  if (1 /* ss_pic != SVR4_PIC */)
    {
      s_ignore (0);
      return;
    }

  /* Add $gp to the register named as an argument.  */
  reg = tc_get_register (0);
  macro_build ((char *) NULL, &icnt, (expressionS *) NULL,
	       "addu", "d,v,t", reg, reg, GP);

  demand_empty_rest_of_line ();  
}

/* Parse a register string into a number.  Called from the ECOFF code
   to parse .frame.  The argument is non-zero if this is the frame
   register, so that we can record it in ss_frame_reg.  */

int
tc_get_register (frame)
     int frame;
{
  int reg;

  SKIP_WHITESPACE ();
  if (*input_line_pointer++ != '$')
    {
      as_warn ("expected `$'");
      reg = 0;
    }
  else if (isdigit ((unsigned char) *input_line_pointer))
    {
      reg = get_absolute_expression ();
      if (reg < 0 || reg >= 32)
	{
	  as_warn ("Bad register number");
	  reg = 0;
	}
    }
  else
    {
      if (strncmp (input_line_pointer, "fp", 2) == 0)
	reg = FP;
      else if (strncmp (input_line_pointer, "sp", 2) == 0)
	reg = SP;
      else if (strncmp (input_line_pointer, "gp", 2) == 0)
	reg = GP;
      else if (strncmp (input_line_pointer, "at", 2) == 0)
	reg = AT;
      else
	{
	  as_warn ("Unrecognized register name");
	  reg = 0;
	}
      input_line_pointer += 2;
    }
  if (frame)
    ss_frame_reg = reg != 0 ? reg : SP;
  return reg;
}

valueT
md_section_align (seg, addr)
     asection *seg;
     valueT addr;
{
  int align = bfd_get_section_alignment (stdoutput, seg);

  return ((addr + (1 << align) - 1) & (-1 << align));
}

/* Estimate the size of a frag before relaxing.  We are not really
   relaxing here, and the final size is encoded in the subtype
   information.  */

/*ARGSUSED*/
int
md_estimate_size_before_relax (fragp, segtype)
     fragS *fragp;
     asection *segtype;
{
  int change;

#ifdef GPOPT
  const char *symname;

  /* Find out whether this symbol can be referenced off the GP
     register.  It can be if it is smaller than the -G size or if
     it is in the .sdata or .sbss section.  Certain symbols can
     not be referenced off the GP, although it appears as though
     they can.  */
  symname = S_GET_NAME (fragp->fr_symbol);
  if (symname != (const char *) NULL
      && (strcmp (symname, "eprol") == 0
	  || strcmp (symname, "etext") == 0
	  || strcmp (symname, "_gp") == 0
	  || strcmp (symname, "edata") == 0
	  || strcmp (symname, "_fbss") == 0
	  || strcmp (symname, "_fdata") == 0
	  || strcmp (symname, "_ftext") == 0
	  || strcmp (symname, "end") == 0
	  || strcmp (symname, "_gp_disp") == 0))
    change = 1;
  else if (! S_IS_DEFINED (fragp->fr_symbol)
	   && ((fragp->fr_symbol->ecoff_extern_size != 0
		&& fragp->fr_symbol->ecoff_extern_size <= g_switch_value)
	       || (S_GET_VALUE (fragp->fr_symbol) != 0
		   && S_GET_VALUE (fragp->fr_symbol) <= g_switch_value)))
    change = 0;
  else
    {
      const char *segname;
      
      segname = segment_name (S_GET_SEGMENT (fragp->fr_symbol));
      assert (strcmp (segname, ".lit8") != 0
	      && strcmp (segname, ".lit4") != 0);
      change = (strcmp (segname, ".sdata") != 0
		&& strcmp (segname, ".sbss") != 0);
    }
#else /* ! defined (GPOPT) */
  /* We are not optimizing for the GP register.  */
  change = 1;
#endif /* ! defined (GPOPT) */  

  if (change)
    {
      /* Record the offset to the first reloc in the fr_opcode field.
	 This lets md_convert_frag and tc_gen_reloc know that the code
	 must be expanded.  */
      fragp->fr_opcode = (fragp->fr_literal
			  + fragp->fr_fix
			  - RELAX_OLD (fragp->fr_subtype)
			  + RELAX_RELOC1 (fragp->fr_subtype));
      /* FIXME: This really needs as_warn_where.  */
      if (RELAX_WARN (fragp->fr_subtype))
	as_warn ("AT used after \".set noat\" or macro used after \".set nomacro\"");
    }

  if (! change)
    return 0;
  else
    return RELAX_NEW (fragp->fr_subtype) - RELAX_OLD (fragp->fr_subtype);
}

/* Translate internal representation of relocation info to BFD target
   format.  */

arelent **
tc_gen_reloc (section, fixp)
     asection *section;
     fixS *fixp;
{
  static arelent *retval[4];
  arelent *reloc;

  reloc = retval[0] = (arelent *) xmalloc (sizeof (arelent));
  retval[1] = NULL;

  reloc->sym_ptr_ptr = &fixp->fx_addsy->bsym;
  reloc->address = fixp->fx_frag->fr_address + fixp->fx_where;

  if (fixp->fx_r_type == BFD_RELOC_PCREL_LO16)
    {
      /* We use a special addend for an internal RELLO reloc.  */
      if (fixp->fx_addsy->bsym->flags & BSF_SECTION_SYM)
	reloc->addend = reloc->address - S_GET_VALUE (fixp->fx_subsy);
      else
	reloc->addend = fixp->fx_addnumber + reloc->address;
    }
  else if (fixp->fx_r_type == BFD_RELOC_PCREL_HI16_S)
    {
      assert (fixp->fx_next != NULL
	      && fixp->fx_next->fx_r_type == BFD_RELOC_PCREL_LO16);
      /* We use a special addend for an internal RELHI reloc.  The
	 reloc is relative to the RELLO; adjust the addend
	 accordingly.  */
      if (fixp->fx_addsy->bsym->flags & BSF_SECTION_SYM)
	reloc->addend = (fixp->fx_next->fx_frag->fr_address
			 + fixp->fx_next->fx_where
			 - S_GET_VALUE (fixp->fx_subsy));
      else
	reloc->addend = (fixp->fx_addnumber
			 + fixp->fx_next->fx_frag->fr_address
			 + fixp->fx_next->fx_where);
    }
  else if (fixp->fx_pcrel == 0)
    reloc->addend = fixp->fx_addnumber;
  else
    {
#ifndef OBJ_AOUT
      /* A gruesome hack which is a result of the gruesome gas reloc
	 handling.  */
      reloc->addend = reloc->address;
#else
      reloc->addend = -reloc->address;
#endif
    }

  /* If this is a variant frag, we may need to adjust the existing
     reloc and generate a new one.  */
  if (fixp->fx_frag->fr_opcode != NULL
      && (fixp->fx_r_type == BFD_RELOC_MIPS_GPREL
	  || fixp->fx_r_type == BFD_RELOC_MIPS_GOT16
	  || fixp->fx_r_type == BFD_RELOC_MIPS_CALL16))
    {
      arelent *reloc2;

      /* If this is not the last reloc in this frag, then we have two
	 GPREL relocs, both of which are being replaced.  Let the
	 second one handle all of them.  */
      if (fixp->fx_next != NULL
	  && fixp->fx_frag == fixp->fx_next->fx_frag)
	{
	  assert (fixp->fx_r_type == BFD_RELOC_MIPS_GPREL
		  && fixp->fx_next->fx_r_type == BFD_RELOC_MIPS_GPREL);
	  retval[0] = NULL;
	  return retval;
	}

      fixp->fx_where = fixp->fx_frag->fr_opcode - fixp->fx_frag->fr_literal;
      reloc->address = fixp->fx_frag->fr_address + fixp->fx_where;
      reloc2 = retval[1] = (arelent *) xmalloc (sizeof (arelent));
      retval[2] = NULL;
      reloc2->sym_ptr_ptr = &fixp->fx_addsy->bsym;
      reloc2->address = (reloc->address
			 + (RELAX_RELOC2 (fixp->fx_frag->fr_subtype)
			    - RELAX_RELOC1 (fixp->fx_frag->fr_subtype)));
      reloc2->addend = fixp->fx_addnumber;
      reloc2->howto = bfd_reloc_type_lookup (stdoutput, BFD_RELOC_LO16);
      assert (reloc2->howto != NULL);

      if (RELAX_RELOC3 (fixp->fx_frag->fr_subtype))
	{
	  arelent *reloc3;

	  reloc3 = retval[2] = (arelent *) xmalloc (sizeof (arelent));
	  retval[3] = NULL;
	  *reloc3 = *reloc2;
	  reloc3->address += 8;
	}

      assert (fixp->fx_r_type == BFD_RELOC_MIPS_GPREL);
      fixp->fx_r_type = BFD_RELOC_HI16_S;
    }

  /* To support a PC relative reloc when generating embedded PIC code
     for ECOFF, we use a Cygnus extension.  We check for that here to
     make sure that we don't let such a reloc escape normally.  */
#ifdef OBJ_ECOFF
  if (fixp->fx_r_type == BFD_RELOC_16_PCREL_S2
      && 1 /* ss_pic != EMBEDDED_PIC */)
    reloc->howto = NULL;
  else
#endif
    reloc->howto = bfd_reloc_type_lookup (stdoutput, fixp->fx_r_type);

  if (reloc->howto == NULL)
    {
      as_bad_where (fixp->fx_file, fixp->fx_line,
		    "Can not represent relocation in this object file format");
      retval[0] = NULL;
    }

  return retval;
}

/* Convert a machine dependent frag.  */

void
md_convert_frag (abfd, asec, fragp)
     bfd *abfd;
     segT asec;
     fragS *fragp;
{
  int old, new;
  char *fixptr;

  if (fragp->fr_opcode == NULL)
    return;

  old = RELAX_OLD (fragp->fr_subtype);
  new = RELAX_NEW (fragp->fr_subtype);
  fixptr = fragp->fr_literal + fragp->fr_fix;

  if (new > 0)
    memcpy (fixptr - old, fixptr, new);

  fragp->fr_fix += new - old;
}

/* This function is called whenever a label is defined.  It is used
   when handling branch delays; if a branch has a label, we assume we
   can not move it.  */

void
ss_define_label (sym)
     symbolS *sym;
{
}

#ifndef ECOFF_DEBUGGING

/* These functions should really be defined by the object file format,
   since they are related to debugging information.  However, this
   code has to work for the a.out format, which does not define them,
   so we provide simple versions here.  These don't actually generate
   any debugging information, but they do simple checking and someday
   somebody may make them useful.  */

typedef struct loc
{
  struct loc *loc_next;
  unsigned long loc_fileno;
  unsigned long loc_lineno;
  unsigned long loc_offset;
  unsigned short loc_delta;
  unsigned short loc_count;
#if 0
  fragS *loc_frag;
#endif
}
locS;

typedef struct proc
  {
    struct proc *proc_next;
    struct symbol *proc_isym;
    struct symbol *proc_end;
    unsigned long proc_reg_mask;
    unsigned long proc_reg_offset;
    unsigned long proc_fpreg_mask;
    unsigned long proc_fpreg_offset;
    unsigned long proc_frameoffset;
    unsigned long proc_framereg;
    unsigned long proc_pcreg;
    locS *proc_iline;
    struct file *proc_file;
    int proc_index;
  }
procS;

typedef struct file
  {
    struct file *file_next;
    unsigned long file_fileno;
    struct symbol *file_symbol;
    struct symbol *file_end;
    struct proc *file_proc;
    int file_numprocs;
  }
fileS;

static struct obstack proc_frags;
static procS *proc_lastP;
static procS *proc_rootP;
static int numprocs;

static void
md_obj_begin ()
{
  obstack_begin (&proc_frags, 0x2000);
}

static void
md_obj_end ()
{
  /* check for premature end, nesting errors, etc */
  if (proc_lastP && proc_lastP->proc_end == NULL)
    as_warn ("missing `.end' at end of assembly");
}

extern char hex_value[];

static long
get_number ()
{
  int negative = 0;
  long val = 0;

  if (*input_line_pointer == '-')
    {
      ++input_line_pointer;
      negative = 1;
    }
  if (!isdigit (*input_line_pointer))
    as_bad ("Expected simple number.");
  if (input_line_pointer[0] == '0')
    {
      if (input_line_pointer[1] == 'x')
	{
	  input_line_pointer += 2;
	  while (isxdigit (*input_line_pointer))
	    {
	      val <<= 4;
	      val |= hex_value[(int) *input_line_pointer++];
	    }
	  return negative ? -val : val;
	}
      else
	{
	  ++input_line_pointer;
	  while (isdigit (*input_line_pointer))
	    {
	      val <<= 3;
	      val |= *input_line_pointer++ - '0';
	    }
	  return negative ? -val : val;
	}
    }
  if (!isdigit (*input_line_pointer))
    {
      printf (" *input_line_pointer == '%c' 0x%02x\n",
	      *input_line_pointer, *input_line_pointer);
      as_warn ("Invalid number");
      return -1;
    }
  while (isdigit (*input_line_pointer))
    {
      val *= 10;
      val += *input_line_pointer++ - '0';
    }
  return negative ? -val : val;
}

/* The .file directive; just like the usual .file directive, but there
   is an initial number which is the ECOFF file index.  */

static void
s_file (x)
     int x;
{
  int line;

  line = get_number ();
  s_app_file (0);
}


/* The .end directive.  */

static void
s_ssend (x)
     int x;
{
  symbolS *p;

  if (!is_end_of_line[(unsigned char) *input_line_pointer])
    {
      p = get_symbol ();
      demand_empty_rest_of_line ();
    }
  else
    p = NULL;
  if (now_seg != text_section)
    as_warn (".end not in text section");
  if (!proc_lastP)
    {
      as_warn (".end and no .ent seen yet.");
      return;
    }

  if (p != NULL)
    {
      assert (S_GET_NAME (p));
      if (strcmp (S_GET_NAME (p), S_GET_NAME (proc_lastP->proc_isym)))
	as_warn (".end symbol does not match .ent symbol.");
    }

  proc_lastP->proc_end = (symbolS *) 1;
}

/* The .aent and .ent directives.  */

static void
s_ent (aent)
     int aent;
{
  int number = 0;
  procS *procP;
  symbolS *symbolP;

  symbolP = get_symbol ();
  if (*input_line_pointer == ',')
    input_line_pointer++;
  SKIP_WHITESPACE ();
  if (isdigit (*input_line_pointer) || *input_line_pointer == '-')
    number = get_number ();
  if (now_seg != text_section)
    as_warn (".ent or .aent not in text section.");

  if (!aent && proc_lastP && proc_lastP->proc_end == NULL)
    as_warn ("missing `.end'");

  if (!aent)
    {
      procP = (procS *) obstack_alloc (&proc_frags, sizeof (*procP));
      procP->proc_isym = symbolP;
      procP->proc_reg_mask = 0;
      procP->proc_reg_offset = 0;
      procP->proc_fpreg_mask = 0;
      procP->proc_fpreg_offset = 0;
      procP->proc_frameoffset = 0;
      procP->proc_framereg = 0;
      procP->proc_pcreg = 0;
      procP->proc_end = NULL;
      procP->proc_next = NULL;
      if (proc_lastP)
	proc_lastP->proc_next = procP;
      else
	proc_rootP = procP;
      proc_lastP = procP;
      numprocs++;
    }
  demand_empty_rest_of_line ();
}

/* The .frame directive.  */

#if 0
static void
s_frame (x)
     int x;
{
  char str[100];
  symbolS *symP;
  int frame_reg;
  int frame_off;
  int pcreg;

  frame_reg = tc_get_register (1);
  if (*input_line_pointer == ',')
    input_line_pointer++;
  frame_off = get_absolute_expression ();
  if (*input_line_pointer == ',')
    input_line_pointer++;
  pcreg = tc_get_register (0);

  /* bob third eye */
  assert (proc_rootP);
  proc_rootP->proc_framereg = frame_reg;
  proc_rootP->proc_frameoffset = frame_off;
  proc_rootP->proc_pcreg = pcreg;
  /* bob macho .frame */

  /* We don't have to write out a frame stab for unoptimized code. */
  if (!(frame_reg == FP && frame_off == 0))
    {
      if (!proc_lastP)
	as_warn ("No .ent for .frame to use.");
      (void) sprintf (str, "R%d;%d", frame_reg, frame_off);
      symP = symbol_new (str, N_VFP, 0, frag_now);
      S_SET_TYPE (symP, N_RMASK);
      S_SET_OTHER (symP, 0);
      S_SET_DESC (symP, 0);
      symP->sy_forward = proc_lastP->proc_isym;
      /* bob perhaps I should have used pseudo set */
    }
  demand_empty_rest_of_line ();
}
#endif

/* The .fmask and .mask directives.  */

#if 0
static void
s_mask (reg_type)
     char reg_type;
{
  char str[100], *strP;
  symbolS *symP;
  int i;
  unsigned int mask;
  int off;

  mask = get_number ();
  if (*input_line_pointer == ',')
    input_line_pointer++;
  off = get_absolute_expression ();

  /* bob only for coff */
  assert (proc_rootP);
  if (reg_type == 'F')
    {
      proc_rootP->proc_fpreg_mask = mask;
      proc_rootP->proc_fpreg_offset = off;
    }
  else
    {
      proc_rootP->proc_reg_mask = mask;
      proc_rootP->proc_reg_offset = off;
    }

  /* bob macho .mask + .fmask */

  /* We don't have to write out a mask stab if no saved regs. */
  if (!(mask == 0))
    {
      if (!proc_lastP)
	as_warn ("No .ent for .mask to use.");
      strP = str;
      for (i = 0; i < 32; i++)
	{
	  if (mask % 2)
	    {
	      sprintf (strP, "%c%d,", reg_type, i);
	      strP += strlen (strP);
	    }
	  mask /= 2;
	}
      sprintf (strP, ";%d,", off);
      symP = symbol_new (str, N_RMASK, 0, frag_now);
      S_SET_TYPE (symP, N_RMASK);
      S_SET_OTHER (symP, 0);
      S_SET_DESC (symP, 0);
      symP->sy_forward = proc_lastP->proc_isym;
      /* bob perhaps I should have used pseudo set */
    }
}
#endif

/* The .loc directive.  */

#if 0
static void
s_loc (x)
     int x;
{
  symbolS *symbolP;
  int lineno;
  int addroff;

  assert (now_seg == text_section);

  lineno = get_number ();
  addroff = obstack_next_free (&frags) - frag_now->fr_literal;

  symbolP = symbol_new ("", N_SLINE, addroff, frag_now);
  S_SET_TYPE (symbolP, N_SLINE);
  S_SET_OTHER (symbolP, 0);
  S_SET_DESC (symbolP, lineno);
  symbolP->sy_segment = now_seg;
}
#endif

#endif /* ! defined (ECOFF_DEBUGGING) */
