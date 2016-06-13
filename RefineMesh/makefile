#.SUFFIXES: .o .f
LIBDIR=
PROGRAM=RefineMesh
OBJS= bordeg.o  conel3.o  dgmap34.o  dparmap.o  drefine.o\
LgcSpMat.o 
LIBS=
FFLAGS= -Wall -fdollar-ok -g -c 
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
