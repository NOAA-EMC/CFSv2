SHELL=/bin/sh

LIBS=	$(LIBJW) 
INCS=	$(INCJW) 

SRCS=	 gblevn00.f gblevn01.f gblevn02.f gblevn03.f gblevn04.f gblevn06.f gblevn08.f gblevn10.f gblevn11.f gblevn12.f gblevn35.f grdcrd.f hterpt.f hterptz.f moifun.f mpicast.f oefg01.f prevents.f sortbufr.f 

FLGS=	-O3 -g -traceback 

CMD=	cfs_prevmpi 

all:	$(CMD)

$(CMD): $(SRCS)
	$(FC) -o $(@) $(FLGS) $(INCS) $(SRCS) $(LIBS) -mkl
	rm -f *.o *.mod

