SHELL=/bin/sh
#
FFLAGS=${FFLAGSM}
TARGET=cfs_cdas_godas_mkDlyAsmPrf

all: $(TARGET)
tmUtil_mod.o: tmUtil_mod.f
	$(FC) $(FFLAGS) -c tmUtil_mod.f

mkDlyAsmPrf.o: mkDlyAsmPrf.f
	$(FC) $(FFLAGS) $(INC) -c mkDlyAsmPrf.f

OBJ = tmUtil_mod.o mkDlyAsmPrf.o

$(TARGET): $(OBJ)
	$(FC) -o $(TARGET) $(OBJ) $(WLIB) $(NCDF)

clean:
	rm *.o *.mod

