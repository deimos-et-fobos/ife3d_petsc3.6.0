.SUFFIXES: .o .f
LIBDIR=
OBJS=searstr.o reaeltyp.o trasim.o conel3.o
LIBS=
FFLAGS= -Wall -g -c 
.f.o:
	mpif77 $(FFLAGS) $<

all: vwm2rtpla

vwm2rtpla: vwm2rtpla.o $(OBJS)
	mpif77 -g -o vwm2rtpla vwm2rtpla.o $(OBJS) $(LIBDIR) $(LIBS) 

wipe:	delprog clean

delprog:
	rm -f vwm2rtpla
clean:
	rm -f *~ *.o *.out vwm2rtpla core
