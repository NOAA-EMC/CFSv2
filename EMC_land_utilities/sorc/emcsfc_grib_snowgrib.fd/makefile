#-------------------------------------------------------------------------------
#  Makefile for emcsfc_grib_snowgrib program. 
#
#  Must be invoked from the 'make.sh' driver script.  See the
#  README.build file for details.
#-------------------------------------------------------------------------------

SHELL=	/bin/sh

CMD=  emcsfc_grib_snowgrib

OBJS= snowgrib.o	

LIBS=${G2_LIB4} \
     ${IP_LIB4} \
     ${SP_LIB4}  \
     ${W3NCO_LIB4} \
     ${BACIO_LIB4} \
     ${JASPER_LIB} \
     ${PNG_LIB} \
     ${Z_LIB}

$(CMD):	$(OBJS)
	$(FCOMP) $(FFLAGS) -o $(CMD) $(OBJS) $(LDFLAGS) $(LIBS)

$(OBJS):  makefile

.f.o:
	$(FCOMP) -c $(FFLAGS) -I$(G2_INC4) $*.f

clean:
	rm -f $(OBJS) $(CMD)
install:
	/usr/bin/install -c -d ../../exec
	/usr/bin/install -c $(CMD) ../../exec
check_prereqs:
	/nwprod/spa_util/check_libs.bash $(LIBS)
#       /nwprod/spa_util/check_incs.bash $(G2_INC4)
