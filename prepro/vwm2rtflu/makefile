.SUFFIXES: .o .f
LIBDIR=
PROGRAM=vwm2rtflu
OBJS=searstr.o reaeltyp.o trasim.o conel4.o conel4_mpi.o
LIBS=
FFLAGS= -Wall -g -c 
.f.o:
	mpif77 $(FFLAGS) $<

all: vwm2rtflu vwm2rtflu_mpi	

vwm2rtflu: vwm2rtflu.o $(OBJS)
	mpif77 -g -o vwm2rtflu vwm2rtflu.o $(OBJS) $(LIBDIR) $(LIBS) 

vwm2rtflu_mpi: vwm2rtflu_mpi.o $(OBJS)
	mpif77 -g -o vwm2rtflu_mpi vwm2rtflu_mpi.o $(OBJS) $(LIBDIR) $(LIBS) 

wipe:	delprog clean

delprog:
	rm -f vwm2rtflu vwm2rtflu_mpi

clean:
	rm -f *~ *.o *.out vwm2rtflu vwm2rtflu_mpi core
