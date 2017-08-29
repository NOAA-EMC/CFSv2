# Makefile created by mkmf $Id: mkmf,v 1.3 2005/05/12 21:41:04 gtn Exp $ 

include /gpfs/b3/ptmp/wx20wn/mom4d_half/bin/mkmf.template.ibm

CPPDEFS_XLF = -WF,"-Duse_netCDF,-Duse_libMPI,-D__aix"
CPPDEFS = -Duse_netCDF -Duse_libMPI -D__aix

.DEFAULT:
	-touch $@
all: ocean_grid_generator.exe
mpp_domains_reduce.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_reduce.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_reduce.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_sum.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_field.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_old.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_new.h mpp.o mpp_parameter.o mpp_datatype.o mpp_data.o mpp_domains_comm.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include -I/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_reduce.F90
mpp_data.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_data.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_sma.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_mpi.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_nocomm.inc mpp_parameter.o mpp_datatype.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include -I/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_data.F90
mpp_io.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h mpp_data.o mpp_datatype.o mpp_parameter.o mpp_io_util.o mpp_io_misc.o mpp_io_write.o mpp_io_read.o mpp_io_connect.o mpp.o mpp_domains.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io.F90
mpp_domains_util.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_util.F90 mpp.o mpp_parameter.o mpp_datatype.o mpp_data.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_util.F90
fms_io.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/fms_io.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_2d.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_3d.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_4d.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/write_data.inc mpp_io.o mpp_domains.o mpp.o platform.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include -I/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/fms_io.F90
mpp_io_util.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_util.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h mpp.o mpp_datatype.o mpp_data.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_util.F90
horiz_interp_spherical.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_spherical.f90 mpp.o fms.o constants.o horiz_interp_type.o
	$(FC) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_spherical.f90
vgrid.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/vgrid.f90 mpp.o mpp_io.o fms.o grids_type.o grids_util.o constants.o
	$(FC) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/vgrid.f90
mpp_util.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_util.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/system_clock.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_sma.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_mpi.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_nocomm.inc mpp_parameter.o mpp_datatype.o mpp_data.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include -I/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_util.F90
ocean_grid_generator.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/ocean_grid_generator.f90 fms.o mpp.o mpp_io.o grids_type.o hgrid.o vgrid.o topog.o constants.o
	$(FC) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/ocean_grid_generator.f90
mpp_io_misc.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_misc.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h mpp.o mpp_domains.o mpp_parameter.o mpp_data.o mpp_datatype.o mpp_io_util.o mpp_domains_define.o mpp_domains_reduce.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_misc.F90
topog.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/topog.f90 mpp.o mpp_domains.o mpp_io.o fms.o axis_utils.o grids_type.o grids_util.o horiz_interp.o constants.o
	$(FC) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/topog.f90
horiz_interp.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp.f90 fms.o mpp.o constants.o horiz_interp_type.o horiz_interp_conserve.o horiz_interp_bilinear.o horiz_interp_spherical.o
	$(FC) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp.f90
grids_util.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/grids_util.f90 fms.o mpp.o mpp_io.o constants.o
	$(FC) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/grids_util.f90
memuse.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/memutils/memuse.c
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/memutils/memuse.c
memutils.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/memutils/memutils.F90 mpp.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/memutils/memutils.F90
threadloc.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/threadloc.c
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/threadloc.c
horiz_interp_bilinear.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_bilinear.f90 mpp.o fms.o constants.o horiz_interp_type.o
	$(FC) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_bilinear.f90
hgrid.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/hgrid.f90 mpp.o mpp_io.o mpp_domains.o fms.o constants.o axis_utils.o grids_type.o grids_util.o
	$(FC) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/hgrid.f90
constants.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/constants/constants.f90
	$(FC) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/constants/constants.f90
mpp_datatype.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_datatype.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_datatype.h mpp_parameter.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include -I/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_datatype.F90
fms.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/fms.f90 mpp.o mpp_domains.o mpp_io.o fms_io.o memutils.o constants.o
	$(FC) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/fms.f90
mpp_domains_misc.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_misc.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_update_domains2D.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_old.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_old.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_new.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_new.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_old.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_new.h mpp.o mpp_parameter.o mpp_datatype.o mpp_data.o mpp_domains_util.o mpp_domains_comm.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include -I/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_misc.F90
mpp_comm.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_comm.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/system_clock.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_sma.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_sma.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_sma.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_sma.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_mpi.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_mpi.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_mpi.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_mpi.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_nocomm.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_nocomm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_nocomm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_nocomm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_chksum_int.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_chksum.h mpp_parameter.o mpp_data.o mpp_util.o mpp_datatype.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include -I/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_comm.F90
horiz_interp_conserve.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_conserve.f90 mpp.o fms.o constants.o horiz_interp_type.o
	$(FC) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_conserve.f90
mpp_io_connect.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_connect.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h mpp.o mpp_parameter.o mpp_datatype.o mpp_data.o mpp_io_misc.o mpp_io_write.o mpp_io_read.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_connect.F90
nsclock.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/nsclock.c
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/nsclock.c
mpp_io_write.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_write.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_write_2Ddecomp.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_write.h mpp.o mpp_domains.o mpp_parameter.o mpp_datatype.o mpp_data.o mpp_io_util.o mpp_io_misc.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include -I/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_write.F90
axis_utils.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/axis_utils/axis_utils.F90 mpp_io.o mpp.o fms.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/axis_utils/axis_utils.F90
mpp_parameter.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_parameter.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_parameter.F90
mpp_domains_comm.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_comm.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_domains_comm.h mpp.o mpp_parameter.o mpp_datatype.o mpp_data.o mpp_domains_util.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include -I/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_comm.F90
platform.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/platform/platform.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/platform/platform.F90
mpp.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h mpp_parameter.o mpp_data.o mpp_comm.o mpp_util.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp.F90
mpp_io_read.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_read.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_read_2Ddecomp.h mpp.o mpp_domains.o mpp_parameter.o mpp_datatype.o mpp_data.o mpp_io_util.o mpp_io_misc.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include -I/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_read.F90
horiz_interp_type.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_type.f90 mpp.o
	$(FC) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_type.f90
mpp_domains.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h mpp_parameter.o mpp_datatype.o mpp_data.o mpp_domains_util.o mpp_domains_reduce.o mpp_domains_misc.o mpp_domains_define.o mpp.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains.F90
mpp_domains_define.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_define.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h mpp.o mpp_parameter.o mpp_datatype.o mpp_data.o mpp_domains_util.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/gpfs/b3/ptmp/wx20wn/mom4d_half/include	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_define.F90
grids_type.o: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/grids_type.f90 mpp_domains.o
	$(FC) $(FFLAGS) -c	/gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/grids_type.f90
./mpp_transmit.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit.inc .
./horiz_interp.f90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp.f90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp.f90 .
./mpp_do_global_field_new.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_new.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_new.h .
./mpp_chksum_int.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_chksum_int.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_chksum_int.h .
./mpp_do_redistribute_gsm.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_gsm.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_gsm.h .
./mpp_read_2Ddecomp.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_read_2Ddecomp.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_read_2Ddecomp.h .
./mpp_do_updateV_gsm.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_gsm.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_gsm.h .
./mpp_io_misc.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_misc.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_misc.F90 .
./mpp_io_util.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_util.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_util.F90 .
./mpp_do_redistribute_new.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_new.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_new.h .
./mpp_reduce_sma.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_sma.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_sma.h .
./mpp_transmit_nocomm.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_nocomm.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_nocomm.h .
./mpp_do_update_gsm.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_gsm.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_gsm.h .
./mpp_do_update_new.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_new.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_new.h .
./topog.f90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/topog.f90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/topog.f90 .
./mpp_io_connect.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_connect.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_connect.F90 .
./mpp_transmit_sma.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_sma.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_sma.h .
./mpp_sum_gsm.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_gsm.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_gsm.h .
./mpp_do_update_caf.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_caf.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_caf.h .
./mpp_util.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_util.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_util.F90 .
./mpp_data_nocomm.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_nocomm.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_nocomm.inc .
./horiz_interp_conserve.f90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_conserve.f90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_conserve.f90 .
./mpp_reduce_gsm.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_gsm.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_gsm.h .
./nsclock.c: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/nsclock.c
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/nsclock.c .
./mpp_transmit_mpi.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_mpi.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_mpi.h .
./fms_platform.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h .
./mpp_sum_mpi.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_mpi.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_mpi.h .
./constants.f90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/constants/constants.f90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/constants/constants.f90 .
./mpp_do_update_old.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_old.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_old.h .
./mpp_io.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io.F90 .
./mpp_domains_comm.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_comm.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_comm.F90 .
./mpp_global_reduce.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_reduce.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_reduce.h .
./mpp_data_sma.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_sma.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_sma.inc .
./mpp_sum_caf.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_caf.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_caf.h .
./mpp_data_mpi.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_mpi.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_mpi.inc .
./horiz_interp_type.f90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_type.f90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_type.f90 .
./mpp_sum_nocomm.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_nocomm.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_nocomm.h .
./write_data.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/write_data.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/write_data.inc .
./mpp_reduce_mpi.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_mpi.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_mpi.h .
./read_data_4d.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_4d.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_4d.inc .
./hgrid.f90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/hgrid.f90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/hgrid.f90 .
./mpp_do_updateV_old.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_old.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_old.h .
./mpp_data.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_data.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_data.F90 .
./mpp_datatype.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_datatype.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_datatype.h .
./mpp_domains.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains.F90 .
./axis_utils.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/axis_utils/axis_utils.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/axis_utils/axis_utils.F90 .
./mpp_do_global_field_old.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_old.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_old.h .
./mpp_global_field.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_field.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_field.h .
./mpp_comm_sma.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_sma.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_sma.inc .
./mpp_write.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_write.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_write.h .
./horiz_interp_spherical.f90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_spherical.f90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_spherical.f90 .
./mpp_do_updateV_caf.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_caf.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_caf.h .
./mpp_comm.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_comm.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_comm.F90 .
./mpp_util_sma.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_sma.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_sma.inc .
./mpp_datatype.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_datatype.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_datatype.F90 .
./mpp_util_mpi.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_mpi.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_mpi.inc .
./mpp_do_global_field_caf.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_caf.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_caf.h .
./mpp_domains_util.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_util.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_util.F90 .
./mpp_domains_misc.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_misc.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_misc.F90 .
./read_data_2d.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_2d.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_2d.inc .
./mpp_global_sum.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_sum.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_sum.h .
./mpp_sum.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum.inc .
./mpp_do_redistribute_caf.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_caf.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_caf.h .
./system_clock.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/system_clock.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/system_clock.h .
./memuse.c: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/memutils/memuse.c
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/memutils/memuse.c .
./vgrid.f90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/vgrid.f90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/vgrid.f90 .
./ocean_grid_generator.f90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/ocean_grid_generator.f90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/ocean_grid_generator.f90 .
./mpp_io_write.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_write.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_write.F90 .
./horiz_interp_bilinear.f90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_bilinear.f90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_bilinear.f90 .
./mpp_domains_define.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_define.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_define.F90 .
./mpp_update_domains2D.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_update_domains2D.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_update_domains2D.h .
./mpp_domains_comm.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_domains_comm.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_domains_comm.h .
./mpp_comm_nocomm.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_nocomm.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_nocomm.inc .
./mpp_reduce_caf.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_caf.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_caf.h .
./mpp_io_read.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_read.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_read.F90 .
./grids_util.f90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/grids_util.f90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/grids_util.f90 .
./mpp_comm_mpi.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_mpi.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_mpi.inc .
./platform.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/platform/platform.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/platform/platform.F90 .
./mpp_do_global_field_gsm.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_gsm.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_gsm.h .
./mpp_write_2Ddecomp.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_write_2Ddecomp.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_write_2Ddecomp.h .
./memutils.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/memutils/memutils.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/memutils/memutils.F90 .
./fms_io.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/fms_io.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/fms_io.F90 .
./read_data_3d.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_3d.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_3d.inc .
./mpp_reduce_nocomm.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_nocomm.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_nocomm.h .
./fms.f90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/fms.f90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/fms.f90 .
./grids_type.f90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/grids_type.f90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/grids_type.f90 .
./mpp.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp.F90 .
./mpp_parameter.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_parameter.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_parameter.F90 .
./mpp_util_nocomm.inc: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_nocomm.inc
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_nocomm.inc .
./mpp_do_redistribute_old.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_old.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_old.h .
./mpp_domains_reduce.F90: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_reduce.F90
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_reduce.F90 .
./threadloc.c: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/threadloc.c
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/threadloc.c .
./mpp_do_updateV_new.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_new.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_new.h .
./mpp_chksum.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_chksum.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_chksum.h .
./mpp_sum_sma.h: /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_sma.h
	cp /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_sma.h .
SRC = /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_reduce.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_data.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_util.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/fms_io.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_util.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_spherical.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/vgrid.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_util.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/ocean_grid_generator.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_misc.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/topog.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/grids_util.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/memutils/memuse.c /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/memutils/memutils.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/threadloc.c /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_bilinear.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/hgrid.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/constants/constants.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_datatype.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/fms.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_misc.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_comm.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_conserve.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_connect.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/nsclock.c /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_write.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/axis_utils/axis_utils.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_parameter.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_comm.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/platform/platform.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_read.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_type.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_define.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/grids_type.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_sma.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_field.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_write.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_new.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_chksum_int.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_read_2Ddecomp.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_sma.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_mpi.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_new.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_sma.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_nocomm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_2d.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_sum.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_new.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/system_clock.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_sma.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_nocomm.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_mpi.h /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_update_domains2D.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_domains_comm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_mpi.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_nocomm.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_old.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_reduce.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_mpi.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_sma.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_mpi.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_write_2Ddecomp.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_nocomm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/write_data.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_mpi.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_4d.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_3d.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_nocomm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_old.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_nocomm.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_old.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_datatype.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_new.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_chksum.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_sma.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_old.h
OBJ = mpp_domains_reduce.o mpp_data.o mpp_io.o mpp_domains_util.o fms_io.o mpp_io_util.o horiz_interp_spherical.o vgrid.o mpp_util.o ocean_grid_generator.o mpp_io_misc.o topog.o horiz_interp.o grids_util.o memuse.o memutils.o threadloc.o horiz_interp_bilinear.o hgrid.o constants.o mpp_datatype.o fms.o mpp_domains_misc.o mpp_comm.o horiz_interp_conserve.o mpp_io_connect.o nsclock.o mpp_io_write.o axis_utils.o mpp_parameter.o mpp_domains_comm.o platform.o mpp.o mpp_io_read.o horiz_interp_type.o mpp_domains.o mpp_domains_define.o grids_type.o
OFF = /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_new.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_chksum_int.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_read_2Ddecomp.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_misc.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_util.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_new.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_sma.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_nocomm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_new.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/topog.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_connect.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_sma.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_util.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_nocomm.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_conserve.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/nsclock.c /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_transmit_mpi.h /gpfs/b3/ptmp/wx20wn/mom4d_half/include/fms_platform.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_mpi.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/constants/constants.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_update_old.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_comm.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_reduce.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_sma.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_data_mpi.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_type.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_nocomm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/write_data.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_mpi.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_4d.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/hgrid.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_old.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_data.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_datatype.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/axis_utils/axis_utils.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_old.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_field.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_sma.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_write.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_spherical.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_comm.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_sma.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_datatype.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_mpi.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_util.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_misc.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_2d.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_global_sum.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/system_clock.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/memutils/memuse.c /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/vgrid.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/ocean_grid_generator.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_write.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/horiz_interp/horiz_interp_bilinear.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_define.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_update_domains2D.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_domains_comm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_nocomm.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_caf.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_io_read.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/grids_util.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_comm_mpi.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/platform/platform.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_global_field_gsm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_write_2Ddecomp.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/memutils/memutils.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/fms_io.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/read_data_3d.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_reduce_nocomm.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/fms/fms.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/preprocessing/generate_grids/ocean/grids_type.f90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_parameter.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_util_nocomm.inc /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_redistribute_old.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/mpp_domains_reduce.F90 /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/threadloc.c /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_do_updateV_new.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_chksum.h /gpfs/b3/ptmp/wx20wn/mom4d_half/src/shared/mpp/include/mpp_sum_sma.h
clean: neat
	-rm -f .cppdefs $(OBJ) ocean_grid_generator.exe
neat:
	-rm -f $(TMPFILES)
localize: $(OFF)
	cp $(OFF) .
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
ocean_grid_generator.exe: $(OBJ)
	$(LD) $(OBJ) -o ocean_grid_generator.exe $(LDFLAGS)
