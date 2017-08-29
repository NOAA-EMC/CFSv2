SHELL=/bin/sh

INCS=${INCSM}
FOPTS=${FFLAGSM}
LOPTS=${LDFLAGSM}
LIBS=${LIBSM}
PROG	= cfs_cdas_gldas_gldas2gdas
$(PROG):	$(PROG).f
	ifort $(INCS) $(FOPTS) $(LOPTS) $(PROG).f $(LIBS) -o $(PROG)

clean:
	rm -rf *.o
