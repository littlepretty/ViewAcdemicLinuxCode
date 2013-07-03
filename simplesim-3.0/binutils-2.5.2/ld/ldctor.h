/* ldctor.h - linker constructor support
   Copyright 1991, 1992, 1993 Free Software Foundation, Inc.
   
This file is part of GLD, the Gnu Linker.

GLD is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GLD is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GLD; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef LDCTOR_H
#define LDCTOR_H

/* List of statements needed to handle constructors */
extern lang_statement_list_type constructor_list;

extern void ldctor_add_set_entry PARAMS ((struct bfd_link_hash_entry *,
					  bfd_reloc_code_real_type,
					  asection *, bfd_vma));
extern void ldctor_build_sets PARAMS ((void));

#endif
