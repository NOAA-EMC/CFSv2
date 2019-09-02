SHELL=	/bin/sh

#------------------------------------------------------------------------
#  Makefile for emcsfc_accum_firedata program.
#
#  Carefully read the README.build file before invoking this makefile.
#------------------------------------------------------------------------

LIBS= $(BACIO_LIB4) $(G2_LIB4) $(W3NCO_LIB4) $(SP_LIB4) \
      $(JASPER_LIB) $(PNG_LIB) $(Z_LIB)

EXE = emcsfc_accum_firedata

OBJS = accum_firedata.o

${EXE}:	${OBJS} 
	${FCOMP} ${FFLAGS} ${LDFLAGS} -o ${EXE} ${OBJS} ${LIBS}

accum_firedata.o: accum_firedata.f90
	${FCOMP} ${FFLAGS} -I$(G2_INC4) -c accum_firedata.f90

all: check_prereqs $(EXE)

clean:
	rm -f *.o *.mod ${EXE} ../../exec/$(EXE)

install:
	/usr/bin/install -c -d ../../exec
	/usr/bin/install -c $(EXE) ../../exec

check_prereqs:
	/nwprod/spa_util/check_libs.bash $(LIBS)
	/nwprod/spa_util/check_incs.bash $(G2_INC4)/grib_mod.mod
