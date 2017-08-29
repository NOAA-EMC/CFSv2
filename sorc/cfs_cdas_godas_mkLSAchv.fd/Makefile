SHELL=/bin/sh
#

SRC=mkLSAchv.f
OBJ=$(SRC:.f=.o)

FFLAGS=${FFLAGSM}
TARGET=cfs_cdas_godas_mkLSAchv

.f.o:
	$(FC) $(FFLAGS) -c $<

$(TARGET): $(OBJ)
	$(FC) -o $(TARGET) $(OBJ) $(WLIB)

clean:
	rm *.o
