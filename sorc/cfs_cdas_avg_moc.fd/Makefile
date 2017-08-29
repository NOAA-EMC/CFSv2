SHELL	=/bin/sh

SRCM	=av_moc.f
SRCS	=
OBJS	=av_moc.o

FFLAGS	=${FFLAGSM}

CMD	=cfs_cdas_avg_moc

$(CMD):	$(OBJS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS)  $(LIBS)

clean:
	-rm -f $(OBJS) *.mod
clobber:        clean
	-rm -f $(CMD)
void:   clobber
	-rm -f $(SRCS) $(SRCM) makefile_avmoc
