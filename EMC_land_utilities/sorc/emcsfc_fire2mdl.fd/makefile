SHELL=	/bin/sh

#------------------------------------------------------------------------
#  Makefile for emcsfc_fire2mdl program.
#
#  Must be invoked from the 'make.sh' driver script.  See the
#  README.build file for details.
#------------------------------------------------------------------------

LIBS= $(BACIO_LIB4) $(G2_LIBd) $(W3NCO_LIBd) $(IP_LIBd) $(SP_LIBd) \
      $(JASPER_LIB) $(PNG_LIB) $(Z_LIB)

CMD= emcsfc_fire2mdl

OBJS = driver.o firedat.o program_setup.o model_grid.o interp.o \
       lambconf.o grib_utils.o

$(CMD):	$(OBJS)
	$(FCOMP) $(FFLAGS) -o $(CMD) $(OBJS) $(LIBS) 

driver.o:  firedat.o program_setup.o model_grid.o driver.f90
	$(FCOMP) $(FFLAGS) -c driver.f90
firedat.o: program_setup.o firedat.f90
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -c firedat.f90
model_grid.o: program_setup.o model_grid.f90
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -c model_grid.f90
program_setup.o:  program_setup.f90
	$(FCOMP) $(FFLAGS) -c program_setup.f90
interp.o:  model_grid.o firedat.o program_setup.o interp.f90
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -I$(IP_INCd) -c interp.f90
lambconf.o:  lambconf.f90
	$(FCOMP) $(FFLAGS) -c lambconf.f90
grib_utils.o:  grib_utils.f90
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -c grib_utils.f90
all: check_prereqs $(CMD)
clean:
	rm -f *.o *.mod ${CMD} ../../exec/$(CMD)
install:
	/usr/bin/install -c -d ../../exec
	/usr/bin/install -c $(CMD) ../../exec
check_prereqs:
	/nwprod/spa_util/check_libs.bash $(LIBS)
	/nwprod/spa_util/check_incs.bash $(G2_INCd)/grib_mod.mod
	/nwprod/spa_util/check_incs.bash $(IP_INCd)/gdswzd_mod.mod
