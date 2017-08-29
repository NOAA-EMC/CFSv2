################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=	/bin/sh
CMD=	cfs_anomgb
FOPTS=  -O3 -g -convert big_endian -assume byterecl -assume noold_ldout_format -r8
LOPTS=  -mkl
INCS=	
OBJS=	anomgb.o
LIBS=	$(LIBSM) 
################################################################################
$(CMD):	$(OBJS)
	$(FC) $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)
$(OBJS):	$(INCS)
anomgb.o:	anomgb.f
	$(FC) -c $(FOPTS) $*.f
clean:	
	-rm -f $(OBJS)
