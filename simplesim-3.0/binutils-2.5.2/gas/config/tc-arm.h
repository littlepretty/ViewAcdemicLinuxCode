/* This file is tc-arm.h
   Contributed by Richard Earnshaw (rwe@pegasus.esprit.ec.org)

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

#define TC_ARM 1

#define COFF_MAGIC ARMMAGIC
#define TARGET_ARCH bfd_arch_arm

#define TARGET_FORMAT "a.out-riscix"

#define AOUT_MACHTYPE 0

#define DIFF_EXPR_OK

#define md_convert_frag(b,s,f)		{as_fatal ("arm convert_frag\n");}

#define obj_fix_adjustable(fixP) 0

#if 0	/* It isn't as simple as this */
#define tc_frob_symbol(sym,punt)	\
{	if (S_IS_LOCAL (sym))		\
	  {				\
	    punt = 1;			\
	    sym->sy_used_in_reloc = 0;	\
	  }}
#endif 

#if 0
#define tc_crawl_symbol_chain(a)	{;}	/* not used */
#define tc_headers_hook(a)		{;}	/* not used */
#endif

#define tc_aout_pre_write_hook(x)	{;}	/* not used */

#define LISTING_HEADER "ARM GAS "

#define OPTIONAL_REGISTER_PREFIX '%'

/* end of tc-arm.h */
