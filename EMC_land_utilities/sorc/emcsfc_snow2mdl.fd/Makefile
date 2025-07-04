SHELL=	/bin/sh

#------------------------------------------------------------------------
#  Makefile for emcsfc_snow2mdl program.
#
#  Before invoking, read the README.build file for details.
#------------------------------------------------------------------------

CMD=	emcsfc_snow2mdl

OBJS=	driver.o \
        program_setup.o \
        snowdat.o \
        model_grid.o \
        grib_utils.o \
        snow2mdl.o 

LIBS= $(G2_LIBd) $(IP_LIBd) $(SP_LIBd) $(LANDSFCUTIL_LIBd) $(BACIO_LIB4) \
      $(W3NCO_LIBd) $(JASPER_LIB) $(PNG_LIB) $(Z_LIB)

$(CMD):	$(OBJS)
	$(FCOMP) $(FFLAGS) -o $(CMD) $(OBJS) $(LIBS)

driver.o: snowdat.o model_grid.o snow2mdl.o program_setup.o driver.F90
	$(FCOMP) $(FFLAGS) -c driver.F90
program_setup.o: program_setup.F90
	$(FCOMP) $(FFLAGS) -c program_setup.F90
snowdat.o: program_setup.o model_grid.o snowdat.F90
	$(FCOMP) $(FFLAGS) -I$(IP_INCd) -I$(G2_INCd) -c snowdat.F90
model_grid.o: program_setup.o model_grid.F90
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -c model_grid.F90
snow2mdl.o: snowdat.o program_setup.o model_grid.o snow2mdl.F90
	$(FCOMP) $(FFLAGS) -I$(IP_INCd) -I$(LANDSFCUTIL_INCd) -I$(G2_INCd) -c snow2mdl.F90
grib_utils.o: grib_utils.F90
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -c grib_utils.F90
clean:
	rm -f *.o *.mod $(CMD) 
install:
	/usr/bin/install -c -d ../../exec
	/usr/bin/install -c $(CMD) ../../exec
