SHELL=/bin/sh

INC	=${INCMOD}
LIBS	= ${LIBSM}
FFLAGS = ${FFLAGSM}

cfs_sigavg:	sigavg.o getopts.o
	${FC} $(FFLAGS) sigavg.o getopts.o $(LIBS) -o cfs_sigavg
sigavg.o:	sigavg.f
	${FC} $(FFLAGS) -I${INC} -c sigavg.f
getopts.o:	getopts.f
	${FC} $(FFLAGS) -I${INC} -c getopts.f
