# compiler flags
CC		= gcc
RM		= rm -f
LIBS	= -lm
#OFLAGS	= -O0 -g -Wall
OFLAGS	= -O3 -Wall

# Verbosity level [0-3]
ifndef VERBOSE
VERBOSE	= 1
endif

CFLAGS	= $(OFLAGS)	-D VERBOSE=$(VERBOSE)

# sources, headers and inputs
SRC	= flp.c flp_desc.c npe.c RCutil.c shape.c temperature.c util.c wire.c
HDR	= flp.h hotfloorplan.h hotspot.h temperature.h util.h wire.h
IN	= ev6.desc ev6.flp hotspot.config avg.p gcc.ptrace gcc.init
DOC = LICENSE README-1.0 README-2.0 jilp.pdf

# objects
OBJ =  flp.o flp_desc.o npe.o RCutil.o shape.o temperature.o util.o wire.o

# targets
all:	hotspot hotfloorplan

hotspot:	hotspot.o	$(OBJ)
	$(CC) $(CFLAGS) -o hotspot hotspot.c $(OBJ) $(LIBS)

hotfloorplan:	hotfloorplan.o	$(OBJ)
	$(CC) $(CFLAGS) -o hotfloorplan hotfloorplan.c $(OBJ) $(LIBS)

.c.$(OEXT):
	$(CC) $(CFLAGS) -c $*.c

filelist:
	@echo hotspot.c hotfloorplan.c sim-template.c $(SRC) $(HDR) $(IN) $(DOC) Makefile tofig.pl

clean:
	$(RM) *.o *.obj core *~ Makefile.bak hotspot hotfloorplan

cleano:
	$(RM) *.o *.obj

