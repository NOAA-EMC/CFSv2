#!/bin/sh

#------------------------------------------------------------------------
#  Makefile for emcsfc_sst2mdl program.
#
#  Before invoking this makefile, read the README.build file.
#------------------------------------------------------------------------

EXE=emcsfc_sst2mdl

OBJS = program_setup.o \
       sstdat.o \
       model_grid.o \
       sst2mdl.o \
       grib_utils.o \
       driver.o 

LIBS= $(G2_LIBd) $(IP_LIBd) $(SP_LIBd) $(LANDSFCUTIL_LIBd) \
      $(BACIO_LIB4) $(W3NCO_LIBd) $(W3EMC_LIBd) $(JASPER_LIB) $(PNG_LIB) $(Z_LIB)

$(EXE):	$(OBJS)
	$(FCOMP) $(FFLAGS) -o $(EXE) $(OBJS) $(LIBS)

driver.o: sstdat.o model_grid.o sst2mdl.o program_setup.o driver.f
	$(FCOMP) $(FFLAGS) -c driver.f
program_setup.o: program_setup.f
	$(FCOMP) $(FFLAGS) -c program_setup.f
grib_utils.o: grib_utils.f
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -c grib_utils.f
sstdat.o: program_setup.o sstdat.f
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -I$(LANDSFCUTIL_INCd) -c sstdat.f
model_grid.o: program_setup.o model_grid.f
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -I$(LANDSFCUTIL_INCd) -c model_grid.f
sst2mdl.o: sstdat.o model_grid.o program_setup.o sst2mdl.f
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -I$(LANDSFCUTIL_INCd) -c sst2mdl.f

all: check_prereqs $(EXE)

clean:
	rm -f *.o *.mod $(EXE) ../../exec/$(EXE)
install:
	/usr/bin/install -c -d ../../exec
	/usr/bin/install -c $(EXE) ../../exec
check_prereqs:
	/nwprod/spa_util/check_libs.bash $(LIBS)
	/nwprod/spa_util/check_incs.bash $(G2_INCd)/grib_mod.mod $(LANDSFCUTIL_INCd)/consts.mod
	/nwprod/spa_util/check_incs.bash $(IP_INCd)/gdswzd_mod.mod
