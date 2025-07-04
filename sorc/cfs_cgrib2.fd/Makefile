SHELL=/bin/sh
#
SRCS=	params.f gds2gdt.f  pds2pdtcfs.f pds2pdtens.f cfscgrib2.f  cnvcfs12.f   cnv22.f \
        cnvcfs21.f gdt2gds.f    makepdscfs.f putgbexn.f   makepdsens.f setbit.f
OBJS=	params.o gds2gdt.o  pds2pdtcfs.o pds2pdtens.o cfscgrib2.o  cnvcfs12.o   cnv22.o \
        cnvcfs21.o gdt2gds.o    makepdscfs.o putgbexn.o   makepdsens.o setbit.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#

INC= $(G2_INC4) 

LIBS= $(G2_LIB4) $(W3NCO_LIB4) $(BACIO_LIB4) $(JASPER_LIB) $(PNG_LIB) $(Z_LIB) 

CMD =		cfs_cgrib2
PROFLIB =	-lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
FFLAGS =         -O3  -g -I $(INC)

#FFLAGS =	 -F
#FFLAGS =	 -Wf"-ez"

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)
	rm -f $(OBJS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS) *.mod

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
