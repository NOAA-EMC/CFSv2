SHELL=	/bin/sh
FFLAGS=${FFLAGSM}
LIBS=	${LIBSM}
SRCS=	avg_pke.f
CMD=	cfs_cdas_avg_pke

$(CMD):	$(SRCS)
	$(FC) $(SRCS) $(FFLAGS) -o $(@) $(OBJS) $(LIBS) ${LDFLAGSM}
