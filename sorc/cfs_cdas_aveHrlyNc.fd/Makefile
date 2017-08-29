SHELL=/bin/sh

FFLAGS=${FFLAGSM}
LDFLAGS=${LDFLAGSM}
PROG=cfs_cdas_aveHrlyNc

all: $(PROG)
$(PROG).o: $(PROG).f
	$(FC) $(FFLAGS) $(INC) -c $(PROG).f

OBJ = $(PROG).o

$(PROG): $(OBJ)
	$(FC) $(LDFLAGS) -o $(PROG) $(OBJ) $(NCDF)

clean:
	rm -rf ${OBJ}
