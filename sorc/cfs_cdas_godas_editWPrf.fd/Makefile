SHELL=/bin/sh
#
SRC=editWPrf.f
OBJ=$(SRC:.f=.o)

FFLAGS=${FFLAGSM}
TARGET=cfs_cdas_godas_editWPrf

.f.o:
	$(FC) $(FFLAGS) $(INC) -c $<

$(TARGET): $(OBJ)
	$(FC) -o $(TARGET) $(OBJ) $(NCDF) $(W3LIB) $(DEBUG)

clean:
	rm *.o
