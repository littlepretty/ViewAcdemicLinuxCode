/* BFD ECOFF object file private structure.
   Copyright (C) 1993 Free Software Foundation, Inc.
   Written by Ian Lance Taylor, Cygnus Support.

This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "bfdlink.h"

#ifndef ECOFF_H
#include "coff/ecoff.h"
#endif

/* This is the backend information kept for ECOFF files.  This
   structure is constant for a particular backend.  The first element
   is the COFF backend data structure, so that ECOFF targets can use
   the generic COFF code.  */

#define ecoff_backend(abfd) \
  ((struct ecoff_backend_data *) (abfd)->xvec->backend_data)

struct ecoff_backend_data
{
  /* COFF backend information.  This must be the first field.  */
  bfd_coff_backend_data coff;
  /* Supported architecture.  */
  enum bfd_architecture arch;
  /* Initial portion of armap string.  */
  const char *armap_start;
  /* The page boundary used to align sections in a demand-paged
     executable file.  E.g., 0x1000.  */
  bfd_vma round;
  /* True if the .rdata section is part of the text segment, as on the
     Alpha.  False if .rdata is part of the data segment, as on the
     MIPS.  */
  boolean rdata_in_text;
  /* Bitsize of constructor entries.  */
  unsigned int constructor_bitsize;
  /* Reloc to use for constructor entries.  */
  CONST struct reloc_howto_struct *constructor_reloc;
  /* How to swap debugging information.  */
  struct ecoff_debug_swap debug_swap;
  /* External reloc size.  */
  bfd_size_type external_reloc_size;
  /* Reloc swapping functions.  */
  void (*swap_reloc_in) PARAMS ((bfd *, PTR, struct internal_reloc *));
  void (*swap_reloc_out) PARAMS ((bfd *, const struct internal_reloc *, PTR));
  /* Backend reloc tweaking.  */
  void (*adjust_reloc_in) PARAMS ((bfd *, const struct internal_reloc *,
				   arelent *));
  void (*adjust_reloc_out) PARAMS ((bfd *, const arelent *,
				    struct internal_reloc *));
  /* Relocate section contents while linking.  */
  boolean (*relocate_section) PARAMS ((bfd *output_bfd, struct bfd_link_info *,
				       bfd *input_bfd, asection *input_section,
				       bfd_byte *contents,
				       PTR external_relocs));
};

/* This is the target specific information kept for ECOFF files.  */

#define ecoff_data(abfd) ((abfd)->tdata.ecoff_obj_data)

typedef struct ecoff_tdata
{
  /* The reloc file position, set by
     ecoff_compute_section_file_positions.  */
  file_ptr reloc_filepos;

  /* The symbol table file position, set by _bfd_ecoff_mkobject_hook.  */
  file_ptr sym_filepos;

  /* The start and end of the text segment.  Only valid for an
     existing file, not for one we are creating.  */
  unsigned long text_start;
  unsigned long text_end;

  /* The cached gp value.  This is used when relocating.  */
  bfd_vma gp;

  /* The maximum size of objects to optimize using gp.  This is
     typically set by the -G option to the compiler, assembler or
     linker.  */
  int gp_size;

  /* The register masks.  When linking, all the masks found in the
     input files are combined into the masks of the output file.
     These are not all used for all targets, but that's OK, because
     the relevant ones are the only ones swapped in and out.  */
  unsigned long gprmask;
  unsigned long fprmask;
  unsigned long cprmask[4];

  /* The ECOFF symbolic debugging information.  */
  struct ecoff_debug_info debug_info;

  /* The unswapped ECOFF symbolic information.  */
  PTR raw_syments;

  /* The canonical BFD symbols.  */
  struct ecoff_symbol_struct *canonical_symbols;

  /* A mapping from external symbol numbers to entries in the linker
     hash table, used when linking.  */
  struct ecoff_link_hash_entry **sym_hashes;

  /* A mapping from reloc symbol indices to sections, used when
     linking.  */
  asection **symndx_to_section;

  /* True if this BFD was written by the backend linker.  */
  boolean linker;

  /* This buffers is used by the find_nearest_line entry point.  */
  char *find_buffer;

} ecoff_data_type;

/* Each canonical asymbol really looks like this.  */

typedef struct ecoff_symbol_struct
{
  /* The actual symbol which the rest of BFD works with */
  asymbol symbol;

  /* The fdr for this symbol.  */
  FDR *fdr;

  /* true if this is a local symbol rather than an external one.  */
  boolean local;

  /* A pointer to the unswapped hidden information for this symbol.
     This is either a struct sym_ext or a struct ext_ext, depending on
     the value of the local field above.  */
  PTR native;
} ecoff_symbol_type;

/* We take the address of the first element of a asymbol to ensure that the
   macro is only ever applied to an asymbol.  */
#define ecoffsymbol(asymbol) ((ecoff_symbol_type *) (&((asymbol)->the_bfd)))

/* This is a hack borrowed from coffcode.h; we need to save the index
   of an external symbol when we write it out so that can set the
   symbol index correctly when we write out the relocs.  */
#define ecoff_get_sym_index(symbol) ((unsigned long) (symbol)->udata)
#define ecoff_set_sym_index(symbol, idx) ((symbol)->udata = (PTR) (idx))

/* When generating MIPS embedded PIC code, the linker relaxes the code
   to turn PC relative branches into longer code sequences when the PC
   relative branch is out of range.  This involves reading the relocs
   in bfd_relax_section as well as in bfd_final_link, and requires the
   code to keep track of which relocs have been expanded.  A pointer
   to this structure is put in the used_by_bfd pointer of a section to
   keep track of this information.  The user_by_bfd pointer will be
   NULL if the information was not needed.  */

struct ecoff_section_tdata
{
  /* The unswapped relocs for this section.  These are stored in
     memory so the input file does not have to be read twice.  */
  PTR external_relocs;

  /* The contents of the section.  These bytes may or may not be saved
     in memory, but if it is this is a pointer to them.  */
  bfd_byte *contents;

  /* Offset adjustments for PC relative branches.  A number other than
     1 is an addend for a PC relative branch, or a switch table entry
     which is the difference of two .text locations; this addend
     arises because the branch or difference crosses one or more
     branches which were expanded into a larger code sequence.  A 1
     means that this branch was itself expanded into a larger code
     sequence.  1 is not a possible offset, since all offsets must be
     multiples of the instruction size, which is 4; also, the only
     relocs with non-zero offsets will be PC relative branches or
     switch table entries within the same object file.  If this field
     is NULL, no branches were expanded and no offsets are required.
     Otherwise there are as many entries as there are relocs in the
     section, and the entry for any reloc that is not PC relative is
     zero.  */
  long *offsets;
};

/* An accessor macro for the ecoff_section_tdata structure.  */
#define ecoff_section_data(abfd, sec) \
  ((struct ecoff_section_tdata *) (sec)->used_by_bfd)

/* ECOFF linker hash table entries.  */

struct ecoff_link_hash_entry
{
  struct bfd_link_hash_entry root;
  /* Symbol index in output file.  */
  long indx;
  /* BFD that ext field value came from.  */
  bfd *abfd;
  /* ECOFF external symbol information.  */
  EXTR esym;
  /* Nonzero if this symbol has been written out.  */
  char written;
  /* Nonzero if this symbol was referred to as small undefined.  */
  char small;
};

/* ECOFF linker hash table.  */

struct ecoff_link_hash_table
{
  struct bfd_link_hash_table root;
};

/* Make an ECOFF object.  */
extern boolean _bfd_ecoff_mkobject PARAMS ((bfd *));

/* Read in the ECOFF symbolic debugging information.  */
extern boolean _bfd_ecoff_slurp_symbolic_info
  PARAMS ((bfd *, asection *, struct ecoff_debug_info *));

/* Generic ECOFF BFD backend vectors.  */

extern boolean _bfd_ecoff_write_object_contents PARAMS ((bfd *abfd));
extern const bfd_target *_bfd_ecoff_archive_p PARAMS ((bfd *abfd));

#define	_bfd_ecoff_close_and_cleanup _bfd_generic_close_and_cleanup
#define _bfd_ecoff_bfd_free_cached_info _bfd_generic_bfd_free_cached_info
extern boolean _bfd_ecoff_new_section_hook
  PARAMS ((bfd *, asection *));
extern boolean _bfd_ecoff_get_section_contents
  PARAMS ((bfd *, asection *, PTR location, file_ptr, bfd_size_type));

extern boolean _bfd_ecoff_bfd_copy_private_bfd_data PARAMS ((bfd *, bfd *));
#define _bfd_ecoff_bfd_copy_private_section_data \
  _bfd_generic_bfd_copy_private_section_data

extern boolean _bfd_ecoff_slurp_armap PARAMS ((bfd *abfd));
#define _bfd_ecoff_slurp_extended_name_table _bfd_slurp_extended_name_table
#define _bfd_ecoff_construct_extended_name_table \
  _bfd_archive_bsd_construct_extended_name_table
#define _bfd_ecoff_truncate_arname bfd_dont_truncate_arname
extern boolean _bfd_ecoff_write_armap
  PARAMS ((bfd *, unsigned int, struct orl *, unsigned int, int));
#define _bfd_ecoff_openr_next_archived_file \
  bfd_generic_openr_next_archived_file
#define _bfd_ecoff_generic_stat_arch_elt bfd_generic_stat_arch_elt
#define _bfd_ecoff_update_armap_timestamp bfd_true

extern long _bfd_ecoff_get_symtab_upper_bound PARAMS ((bfd *abfd));
extern long _bfd_ecoff_get_symtab PARAMS ((bfd *abfd, asymbol **alocation));
extern asymbol *_bfd_ecoff_make_empty_symbol PARAMS ((bfd *abfd));
extern void _bfd_ecoff_print_symbol
  PARAMS ((bfd *, PTR filep, asymbol *, bfd_print_symbol_type));
extern void _bfd_ecoff_get_symbol_info
  PARAMS ((bfd *, asymbol *, symbol_info *));
#define _bfd_ecoff_bfd_is_local_label bfd_generic_is_local_label
#define _bfd_ecoff_get_lineno _bfd_nosymbols_get_lineno
extern boolean _bfd_ecoff_find_nearest_line
  PARAMS ((bfd *, asection *, asymbol **, bfd_vma offset,
	   const char **filename_ptr, const char **fnname_ptr,
	   unsigned int *retline_ptr));
#define _bfd_ecoff_bfd_make_debug_symbol _bfd_nosymbols_bfd_make_debug_symbol

#define _bfd_ecoff_get_reloc_upper_bound coff_get_reloc_upper_bound
extern long _bfd_ecoff_canonicalize_reloc
  PARAMS ((bfd *, asection *, arelent **, asymbol **symbols));
/* ecoff_bfd_reloc_type_lookup defined by backend. */

extern boolean _bfd_ecoff_set_arch_mach
  PARAMS ((bfd *, enum bfd_architecture, unsigned long machine));
extern boolean _bfd_ecoff_set_section_contents
  PARAMS ((bfd *, asection *, PTR location, file_ptr, bfd_size_type));

extern int _bfd_ecoff_sizeof_headers PARAMS ((bfd *abfd, boolean reloc));
/* ecoff_bfd_get_relocated_section_contents defined by backend.  */
/* ecoff_bfd_relax_section defined by backend.  */
extern struct bfd_link_hash_table *_bfd_ecoff_bfd_link_hash_table_create
  PARAMS ((bfd *));
extern boolean _bfd_ecoff_bfd_link_add_symbols
  PARAMS ((bfd *, struct bfd_link_info *));
extern boolean _bfd_ecoff_bfd_final_link
  PARAMS ((bfd *, struct bfd_link_info *));

/* Hook functions for the generic COFF section reading code.  */

extern PTR _bfd_ecoff_mkobject_hook PARAMS ((bfd *, PTR filehdr, PTR aouthdr));
extern asection *_bfd_ecoff_make_section_hook PARAMS ((bfd *abfd, char *name));
#define _bfd_ecoff_set_alignment_hook \
  ((void (*) PARAMS ((bfd *, asection *, PTR))) bfd_void)
extern boolean _bfd_ecoff_set_arch_mach_hook PARAMS ((bfd *abfd, PTR filehdr));
extern flagword _bfd_ecoff_styp_to_sec_flags
  PARAMS ((bfd *abfd, PTR hdr, const char *name));
extern boolean _bfd_ecoff_slurp_symbol_table PARAMS ((bfd *abfd));