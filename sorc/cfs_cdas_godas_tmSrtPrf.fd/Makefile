SHELL=/bin/sh

SRC=tmSrtPrf.f
OBJ=$(SRC:.f=.o)

FFLAGS=${FFLAGSM}
TARGET=cfs_cdas_godas_tmSrtPrf

.f.o:
	$(FC) $(FFLAGS) -c $<

$(TARGET): $(OBJ)
	$(FC) -o $(TARGET) $(OBJ) $(WLIB)

clean:
	rm *.o
