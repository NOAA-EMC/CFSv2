SHELL=		/bin/sh

SRCS=		modules.f mv_nvortex.f BOUND.f sig_p_convt1.f SEPAR.f \
		WNLIT.f FDUMP.f H12.f I1MACH.f J4SAVE.f XGETUA.f WNLSM.f \
		WNNLS.f XERABT.f XERCTL.f XERPRT.f XERROR.f XERRWV.f XERSAV.f\
		srotm.f srotmg.f amatrix.f rodist.f landcmsk.f w3tagb.f

OBJS=		modules.o mv_nvortex.o BOUND.o sig_p_convt1.o SEPAR.o \
		WNLIT.o FDUMP.o H12.o I1MACH.o J4SAVE.o XGETUA.o WNLSM.o \
		WNNLS.o XERABT.o XERCTL.o XERPRT.o XERROR.o XERRWV.o XERSAV.o \
		srotm.o srotmg.o amatrix.o rodist.o landcmsk.o w3tagb.o

FC =		mpiifort 

CMD =   	relocate_mv_nvortex

FFLAGS= 	-O3 -g -traceback -r8 -I $(SIGIO_INC4)
LDFLAGS=	-mkl -lmpi 

all:  		$(CMD)

$(CMD): 	$(OBJS)
		$(FC)  $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS_REL)

clean:
		-rm -f  *.o  *.mod

clobber:        clean
		-rm -f $(CMD) $(CMD).prof

install:
	mv $(CMD) ../../exec/$(CMD)

