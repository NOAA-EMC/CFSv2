SHELL=/bin/sh

SRC=avePrfDly.f
OBJ=$(SRC:.f=.o)

FFLAGS=${FFLAGSM}
TARGET=cfs_cdas_godas_avePrfDly

.f.o:
	$(FC) $(FFLAGS) -c $<

$(TARGET): $(OBJ)
	$(FC) -o $(TARGET) $(OBJ) $(WLIB)

clean:
	rm -rf ${OBJS}
