SHELL	=/bin/sh
SRCM	=sighdr.f
INCM	=
SRCS	=
OBJS	=
MODS	=
INCM  =${INCMOD}
FFLAGS	=${FFLAGSM}
LDFLAGS	=   
LIBS    =   ${LIBSM}
CMD	=cfs_sighdr
$(CMD):	$(SRCM) $(OBJS)
	$(FC) $(FFLAGS) -I${INCM} $(SRCM) -o ${CMD} $(OBJS) $(LIBS)
clean:
	-rm -f $(OBJS) $(MODS)
clobber:        clean
	-rm -f $(CMD)
void:   clobber
	-rm -f $(SRCS) $(SRCM) $(INCM) makefile
