SHELL=	/bin/ksh
NAME=cfs_mpi_timavg_3d
LIBS=	$(LIBSM)
SRCS=	${NAME}.f90
CMD=	${NAME}
FFLAGS=${FFLAGSM}

$(CMD):	$(SRCS)
	$(FC) $(SRCS) $(FFLAGS) -o $(@) $(LIBS) 
clean:
	/bin/rm -f $(CMD) *.o

