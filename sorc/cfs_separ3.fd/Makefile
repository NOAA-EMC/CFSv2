NAME=cfs_separ3
SRCS=	${NAME}.f
CMD=	${NAME}
FC=	$(FCMP)
 FFLAGS= $(FFLAGSM)

$(CMD):	$(SRCS)
	$(FC) $(SRCS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)
