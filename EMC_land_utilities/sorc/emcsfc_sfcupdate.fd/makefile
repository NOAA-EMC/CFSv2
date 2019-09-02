SHELL=	/bin/sh

#------------------------------------------------------------------------
#  Makefile for emcsfc_sfcupdate program.
#
#  Read the README.build file before invoking this makefile.
#------------------------------------------------------------------------

EXE= emcsfc_sfcupdate

OBJS = program_setup.o \
       read_data_regional.o \
       read_data.o \
       process_data.o \
       write_data_regional.o \
       sfccycle2.o \
       grib_utils.o \
       sfcupdate.o

LIBS=  $(LANDSFCUTIL_LIBd) $(G2_LIBd) $(NEMSIO_LIB) $(BACIO_LIB4) \
       $(W3NCO_LIBd) $(JASPER_LIB) $(PNG_LIB) $(Z_LIB)

$(EXE):	${OBJS} 
	$(FCOMP) $(FFLAGS) -o $(EXE) ${OBJS} $(LDFLAGS) $(LIBS)

sfcupdate.o: program_setup.o read_data_regional.o\
             write_data_regional.o sfccycle2.o read_data.o sfcupdate.f
	$(FCOMP) $(FFLAGS) -c sfcupdate.f
program_setup.o: program_setup.f
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -I$(LANDSFCUTIL_INCd) -c program_setup.f
grib_utils.o: grib_utils.f
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -c grib_utils.f
read_data.o: read_data.f
	$(FCOMP) $(FFLAGS) -I$(G2_INCd) -I$(LANDSFCUTIL_INCd) -c read_data.f
read_data_regional.o: program_setup.o read_data_regional.f
	$(FCOMP) $(FFLAGS) -I$(NEMSIO_INC) -c read_data_regional.f
process_data.o: read_data.o process_data.f
	$(FCOMP) $(FFLAGS) -I$(LANDSFCUTIL_INCd) -c process_data.f
write_data_regional.o: program_setup.o write_data_regional.f
	$(FCOMP) $(FFLAGS) -I$(NEMSIO_INC) -c write_data_regional.f
sfccycle2.o: read_data.o process_data.o sfccycle2.f
	$(FCOMP) $(FFLAGS) -I$(LANDSFCUTIL_INCd) -c sfccycle2.f
all: check_prereqs $(EXE)
clean:
	rm -f *.o *.mod $(EXE) ../../exec/$(EXE)
install:
	/usr/bin/install -c -d ../../exec
	/usr/bin/install -c $(EXE) ../../exec
check_prereqs:
	/nwprod/spa_util/check_libs.bash $(LIBS)
	/nwprod/spa_util/check_incs.bash $(G2_INCd)/grib_mod.mod $(LANDSFCUTIL_INCd)/consts.mod $(NEMSIO_INC)/nemsio_module.mod
