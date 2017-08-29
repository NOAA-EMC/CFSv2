SHELL=  /bin/sh
SRCS = genpsiandchi.f num_parthds.f
OBJS = genpsiandchi.o num_parthds.o
LIBS    = ${LIBSM}
FFLAGS  =  ${FFLAGSM}

CMD=cfs_genpsiandchi

$(CMD): $(OBJS)
	$(FC) $(FFLAGS) -o $(CMD) $(OBJS) $(LIBS)

.f.o : 
	$(FC) $(FFLAGS) -c $<

clean:
	rm -rf $(OBJS) $(CMD)
