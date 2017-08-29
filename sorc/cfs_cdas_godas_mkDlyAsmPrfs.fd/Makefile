SHELL=/bin/sh
#

FFLAGS=${FFLAGSM}
TARGET=cfs_cdas_godas_mkDlyAsmPrfs

all: $(TARGET)
tmUtil_mod.o: tmUtil_mod.f
	$(FC) $(FFLAGS) -c tmUtil_mod.f

mkDlyAsmPrfs.o: mkDlyAsmPrfs.f
	$(FC) $(FFLAGS) $(INC) -c mkDlyAsmPrfs.f

OBJ = tmUtil_mod.o mkDlyAsmPrfs.o

$(TARGET): $(OBJ)
	$(FC) -o $(TARGET) $(OBJ) $(WLIB) $(NCDF)

clean:
	rm *.o *.mod

