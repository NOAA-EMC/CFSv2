SHELL	=/bin/sh
INCMOD =${INC}

SRCM	=tripo2reg.f
SRCS	=
OBJS	=interp.o tripo_intp.o amoc.o
FC	=ifort
FC90    =ifort

FFLAGS	= ${FFLAGSM} 
FFLAGS2 = ${FFLAGSM2}

NCDFLIB = ${NCDF}
LIBS	= ${LIBSM} ${NCDFLIB}

CMD	=cfs_tripo2reg
$(CMD):	$(SRCM) $(OBJS)
	$(FC90) $(FFLAGS) -o $(CMD) $(LDFLAGS) $(SRCM) $(OBJS) $(LIBS)

interp.o:      interp.f
	$(FC) $(FFLAGS2) -c interp.f

tripo_intp.o:      tripo_intp.f
	$(FC90) $(FFLAGS) $(INCMOD) -c tripo_intp.f

amoc.o:      amoc.f
	$(FC90) $(FFLAGS) $(INCMOD) -c amoc.f

clean:
	-rm -f $(OBJS) *.mod
clobber:        clean
	-rm -f $(CMD)
void:   clobber
	-rm -f $(SRCS) $(SRCM) makefile
