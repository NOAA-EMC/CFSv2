SHELL=  /bin/sh
FFLAGS  =$(FFLAGSM)
F77=$(FCMP)
PROG=coupler
                                                                               
cfs_mlc_$(PROG):	coupler_module.o $(PROG).o
	$(F77) $(FFLAGS)  coupler_module.o $(PROG).o -o cfs_mlc_$(PROG)

coupler_module.o:	coupler_module.f
	$(F77) $(FFLAGS) -c coupler_module.f
$(PROG).o:	$(PROG).f
	$(F77) $(FFLAGS) -c $(PROG).f
