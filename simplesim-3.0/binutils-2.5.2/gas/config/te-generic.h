/*
 * This file is te-generic.h and is intended to be a template for
 * target environment specific header files.
 *
 * It is my intent that this file will evolve into a file suitable for config,
 * compile, and copying as an aid for testing and porting.  xoxorich.
 */

#define TE_GENERIC 1

/* Added these, because if we don't know what we're targetting we may
   need an assembler version of libgcc, and that will use local
   labels.  */
#define LOCAL_LABELS_DOLLAR
#define LOCAL_LABELS_FB

/* these define interfaces */
#include "obj-format.h"

/*
 * Local Variables:
 * comment-column: 0
 * fill-column: 131
 * End:
 */

/* end of te-generic.h */
