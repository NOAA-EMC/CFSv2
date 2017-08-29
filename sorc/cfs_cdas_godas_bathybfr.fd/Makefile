SHELL=	/bin/sh
#
#
SRCS=	bathybfr.f

OBJS=	bathybfr.o

# Tunable parameters
#
# FC        Name of the Fortran compiling system to use
# LDFLAGS   Options of the loader
# FFLAGS    Options of the compiler
# DEBUG     Options of the compiler included for debugging
# LIBS      List of libraries
# CMD       Name of the executable
#
LDFLAGS =${LDFLAGSM}
LIBS = ${LIBSM} 

CMD =      cfs_cdas_godas_bathybfr
FFLAGS =${FFLAGSM}

# Lines from here down should not need to be changes.  They are
# the actual rules which make uses to build CMD.
#
all:        $(CMD)

$(CMD):      $(OBJS)
	$(FC)  $(LDFLAGS) -o $(@) $(OBJS)  $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:        clean
	-rm -f $(CMD)

void:   clobber
	-rm -f $(SRCS) makefile

