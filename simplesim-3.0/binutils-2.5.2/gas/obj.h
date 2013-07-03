/* obj.h - defines the object dependent hooks for all object
   format backends.

   Copyright (C) 1987, 1990, 1991, 1992 Free Software Foundation, Inc.

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

char *obj_default_output_file_name PARAMS ((void));
void obj_emit_relocations PARAMS ((char **where, fixS * fixP,
				   relax_addressT segment_address_in_file));
void obj_emit_strings PARAMS ((char **where));
void obj_emit_symbols PARAMS ((char **where, symbolS * symbols));
void obj_read_begin_hook PARAMS ((void));
#ifndef BFD_ASSEMBLER
void obj_crawl_symbol_chain PARAMS ((object_headers * headers));
void obj_header_append PARAMS ((char **where, object_headers * headers));
#ifndef obj_pre_write_hook
void obj_pre_write_hook PARAMS ((object_headers * headers));
#endif
#endif

#ifndef obj_symbol_new_hook
void obj_symbol_new_hook PARAMS ((symbolS * symbolP));
#endif

void obj_symbol_to_chars PARAMS ((char **where, symbolS * symbolP));

extern const pseudo_typeS obj_pseudo_table[];

/* end of obj.h */
