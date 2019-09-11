SHELL=/bin/sh

SRCS=	 gblevn00.f gblevn01.f gblevn02.f gblevn03.f gblevn04.f gblevn06.f gblevn08.f gblevn10.f gblevn11.f gblevn12.f gblevn35.f grdcrd.f hterpt.f hterptz.f moifun.f mpicast.f oefg01.f prevents.f sortbufr.f gblevn10nems.f

FLGS=	$(FFLAGS)                    

FC=	$(FCMP)    

CMD=	cfs_prevmpi 

all:	$(CMD)

$(CMD): $(SRCS)
	$(FC) $(FLGS) -o $(@) $(SRCS) $(LIBS) $(INCS)
	mv $(CMD) ../../exec
	rm -f *.o *.mod

