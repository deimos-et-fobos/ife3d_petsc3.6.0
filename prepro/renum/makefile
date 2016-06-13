.SUFFIXES: .o .f
LIBDIR=
PROGRAM=renum
OBJS=searstr.o reaeltyp.o
LIBS=
FFLAGS= -Wall -g -c 
.f.o:
	gfortran $(FFLAGS) $<

all:     program	

program	:   $(PROGRAM).o $(OBJS)
	gfortran -g -o $(PROGRAM) $(PROGRAM).o $(OBJS) $(LIBDIR) $(LIBS) 

wipe:	delprog clean

delprog:
	rm -f $(PROGRAM)
clean:
	rm -f *~ *.o *.out $(PROGRAM) core
