.SUFFIXES: .o .f
LIBDIR=
OBJS=searstr.o reaeltyp.o trasim.o conel4.o conel4_mpi.o
LIBS=
FFLAGS= -Wall -g -c 
.f.o:
	mpif77 $(FFLAGS) $<

all: vwm2rtsol vwm2rtsol_mpi	

vwm2rtsol: vwm2rtsol.o $(OBJS)
	mpif77 -g -o vwm2rtsol vwm2rtsol.o $(OBJS) $(LIBDIR) $(LIBS) 

vwm2rtsol_mpi: vwm2rtsol_mpi.o $(OBJS)
	mpif77 -g -o vwm2rtsol_mpi vwm2rtsol_mpi.o $(OBJS) $(LIBDIR) $(LIBS) 

wipe:	delprog clean

delprog:
	rm -f vwm2rtsol vwm2rtsol_mpi
clean:
	rm -f *~ *.o *.out vwm2rtsol vwm2rtsol_mpi core
