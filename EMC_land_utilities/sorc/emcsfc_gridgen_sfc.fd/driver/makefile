SHELL=	/bin/sh

#----------------------------------------------------------------------------------
# Makefile to build the emcsfc_gridgen_sfc driver program.
#
# Must be invoked from the make.sh driver script.  See the README.build
# file for more details.
#----------------------------------------------------------------------------------

EXE = emcsfc_gridgen_sfc

LIBS = ../../../lib/libgridgen_sfc.a \
       $(G2_LIBd) $(IP_LIBd) $(SP_LIBd) \
       $(BACIO_LIB4) $(W3NCO_LIBd) $(W3EMC_LIBd) \
       $(JASPER_LIB) $(PNG_LIB) $(Z_LIB)

OBJS = driver.o

$(EXE):	$(OBJS) 
	$(FCOMP) $(FFLAGS) -o $(EXE) $(LDFLAGS) $(OBJS) $(LIBS)

driver.o: driver.f90
	$(FCOMP) $(FFLAGS) -I../../../incmod/emcsfc_gridgen_sfc -I$(G2_INCd) -c driver.f90

all: check_prereqs $(EXE)

clean:
	rm -f *.o *.mod $(EXE) ../../../exec/$(EXE)

install:
	/usr/bin/install -c -d ../../../exec
	/usr/bin/install -c $(EXE) ../../../exec

check_prereqs:
	/nwprod/spa_util/check_libs.bash $(LIBS)
	/nwprod/spa_util/check_incs.bash ../../../incmod/emcsfc_gridgen_sfc/program_setup.mod 
	/nwprod/spa_util/check_incs.bash $(G2_INCd)/grib_mod.mod
	/nwprod/spa_util/check_incs.bash $(IP_INCd)/gdswzd_mod.mod
