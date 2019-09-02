SHELL=	/bin/sh

#----------------------------------------------------------------------------------
# Makefile to build the emcsfc_gridgen_sfc library.
#
# Read the README.build for details before invoking this makefile.
#----------------------------------------------------------------------------------

TARGETLIB = libgridgen_sfc.a

OBJS = calc_latlons.o \
       calc_tiles.o \
       find_bounds_ll.o \
       gaussian_reduced.o \
       green.o \
       grib_latlons.o \
       grid_info.o \
       gridgen_sfc.o \
       init_grib1.o \
       init_grib2.o \
       init_grid.o \
       interp_tiles.o \
       interp_to_mdl.o \
       interp_utils.o \
       leaf_area_index.o \
       ll2xy_util_bgrid.o \
       ll2xy_util_egrid.o \
       ll2xy_util_polar.o \
       lsmask_orog.o  \
       max_snow_albedo.o \
       mpimod.o \
       native_endianness.o \
       program_setup.o \
       roughness.o \
       slope_type.o \
       smooth.o \
       snow_free_albedo.o \
       soil_substrate.o \
       soil_vegtype_tile.o \
       sort.o 

${TARGETLIB}:	${OBJS} 
	ar ru ${TARGETLIB} ${OBJS}

calc_latlons.o: program_setup.o mpimod.o calc_latlons.f90
	${FCOMP} ${FFLAGS} -c calc_latlons.f90

calc_tiles.o: mpimod.o lsmask_orog.o calc_tiles.f90
	${FCOMP} ${FFLAGS} -c calc_tiles.f90

find_bounds_ll.o: program_setup.o mpimod.o find_bounds_ll.f90
	${FCOMP} ${FFLAGS} -c find_bounds_ll.f90

gaussian_reduced.o: program_setup.o gaussian_reduced.f90
	${FCOMP} ${FFLAGS} -c gaussian_reduced.f90

green.o: program_setup.o green.f90
	${FCOMP} ${FFLAGS} -c green.f90

grid_info.o: grid_info.f90
	${FCOMP} ${FFLAGS} -c grid_info.f90

gridgen_sfc.o: program_setup.o init_grib1.o init_grib2.o calc_latlons.o \
               lsmask_orog.o mpimod.o soil_vegtype_tile.o grid_info.o gridgen_sfc.f90
	${FCOMP} ${FFLAGS} -c gridgen_sfc.f90

init_grib1.o: program_setup.o calc_latlons.o init_grib1.f90
	${FCOMP} ${FFLAGS} -c init_grib1.f90

init_grib2.o: program_setup.o calc_latlons.o init_grib2.f90
	${FCOMP} ${FFLAGS} -I$(G2_INCd) -c init_grib2.f90

init_grid.o: program_setup.o calc_latlons.o ll2xy_util_bgrid.o ll2xy_util_egrid.o ll2xy_util_polar.o init_grid.f90
	${FCOMP} ${FFLAGS} -c init_grid.f90

grib_latlons.o: init_grib1.o calc_latlons.o mpimod.o program_setup.o init_grib2.o grib_latlons.f90
	${FCOMP} ${FFLAGS} -I$(G2_INCd) -c grib_latlons.f90

interp_tiles.o: init_grib1.o program_setup.o mpimod.o calc_latlons.o lsmask_orog.o \
                ll2xy_util_bgrid.o ll2xy_util_egrid.o interp_tiles.f90
	${FCOMP} ${FFLAGS} -I$(IP_INCd) -c interp_tiles.f90

interp_to_mdl.o: native_endianness.o mpimod.o program_setup.o lsmask_orog.o \
                 calc_latlons.o init_grib1.o init_grib2.o interp_to_mdl.f90
	${FCOMP} ${FFLAGS} -c -I$(G2_INCd) interp_to_mdl.f90

interp_utils.o: interp_utils.f90
	${FCOMP} ${FFLAGS} -c interp_utils.f90

leaf_area_index.o: program_setup.o interp_to_mdl.o leaf_area_index.f90
	${FCOMP} ${FFLAGS} -c leaf_area_index.f90

ll2xy_util_bgrid.o: ll2xy_util_bgrid.f90
	${FCOMP} ${FFLAGS} -c ll2xy_util_bgrid.f90

ll2xy_util_egrid.o: ll2xy_util_egrid.f90
	${FCOMP} ${FFLAGS} -c ll2xy_util_egrid.f90

ll2xy_util_polar.o: ll2xy_util_polar.f90
	${FCOMP} ${FFLAGS} -c ll2xy_util_polar.f90

lsmask_orog.o: native_endianness.o program_setup.o init_grib2.o init_grib1.o calc_latlons.o \
               mpimod.o ll2xy_util_bgrid.o ll2xy_util_egrid.o ll2xy_util_polar.o lsmask_orog.f90
	${FCOMP} ${FFLAGS} -I$(G2_INCd) -I$(IP_INCd) -c lsmask_orog.f90

max_snow_albedo.o: program_setup.o max_snow_albedo.f90
	${FCOMP} ${FFLAGS} -c max_snow_albedo.f90

mpimod.o: mpimod.f90
	${FCOMP} ${FFLAGS} -c mpimod.f90

native_endianness.o: native_endianness.f90
	${FCOMP} ${FFLAGS} -c native_endianness.f90

program_setup.o: mpimod.o grid_info.o program_setup.f90
	${FCOMP} ${FFLAGS} -c program_setup.f90

roughness.o: mpimod.o soil_vegtype_tile.o program_setup.o lsmask_orog.o init_grib1.o init_grib2.o roughness.f90
	${FCOMP} ${FFLAGS} -I$(G2_INCd) -c roughness.f90

slope_type.o: program_setup.o slope_type.f90
	${FCOMP} ${FFLAGS} -c slope_type.f90

smooth.o: smooth.f90
	${FCOMP} ${FFLAGS} -c smooth.f90

snow_free_albedo.o: interp_to_mdl.o program_setup.o snow_free_albedo.f90
	${FCOMP} ${FFLAGS} -c snow_free_albedo.f90

soil_substrate.o: native_endianness.o init_grib1.o program_setup.o calc_latlons.o \
                  lsmask_orog.o mpimod.o init_grib2.o soil_substrate.f90
	${FCOMP} ${FFLAGS} -I$(G2_INCd) -c soil_substrate.f90

soil_vegtype_tile.o: native_endianness.o init_grib1.o program_setup.o lsmask_orog.o \
                     interp_tiles.o mpimod.o init_grib2.o soil_vegtype_tile.f90
	${FCOMP} ${FFLAGS} -I$(G2_INCd) -c soil_vegtype_tile.f90

sort.o: sort.f90
	${FCOMP} ${FFLAGS} -c sort.f90

all:  check_prereqs ${TARGETLIB}

clean:
	rm -f *.o *.mod ${TARGETLIB} ../../../incmod/emcsfc_gridgen_sfc/*.mod ../../../lib/${TARGETLIB}

install:
	/usr/bin/install -c -d ../../../lib
	/usr/bin/install -c $(TARGETLIB) ../../../lib
	/usr/bin/install -c -d ../../../incmod/emcsfc_gridgen_sfc
	/usr/bin/install -c *.mod ../../../incmod/emcsfc_gridgen_sfc

check_prereqs:
	/nwprod/spa_util/check_incs.bash $(G2_INCd)/grib_mod.mod
	/nwprod/spa_util/check_incs.bash $(IP_INCd)/gdswzd_mod.mod
