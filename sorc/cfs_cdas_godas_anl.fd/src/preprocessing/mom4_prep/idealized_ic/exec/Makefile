# Makefile created by mkmf $Id: mkmf,v 1.3 2005/05/12 21:41:04 gtn Exp $ 

include /nfsuser/g01/wx20wn/mom4d/bin/mkmf.template.ibm

CPPDEFS_XLF = -WF,"-Duse_netCDF,-Duse_libMPI,-D__aix"
CPPDEFS = -Duse_netCDF -Duse_libMPI -D__aix

.DEFAULT:
	-touch $@
all: idealized_ic.exe
mpp_domains_reduce.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_reduce.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_reduce.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_sum.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_field.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_old.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_new.h mpp.o mpp_parameter.o mpp_datatype.o mpp_data.o mpp_domains_comm.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include -I/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_reduce.F90
mpp_data.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_data.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_sma.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_mpi.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_nocomm.inc mpp_parameter.o mpp_datatype.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include -I/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_data.F90
constants.o: /nfsuser/g01/wx20wn/mom4d/src/shared/constants/constants.f90
	$(FC) $(FFLAGS) -c	/nfsuser/g01/wx20wn/mom4d/src/shared/constants/constants.f90
mpp_datatype.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_datatype.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_datatype.h mpp_parameter.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include -I/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_datatype.F90
idealized_ic_driver.o: /nfsuser/g01/wx20wn/mom4d/src/preprocessing/mom4_prep/idealized_ic/idealized_ic_driver.f90 mpp.o fms.o idealized_ic.o constants.o
	$(FC) $(FFLAGS) -c	/nfsuser/g01/wx20wn/mom4d/src/preprocessing/mom4_prep/idealized_ic/idealized_ic_driver.f90
mpp_io.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h mpp_data.o mpp_datatype.o mpp_parameter.o mpp_io_util.o mpp_io_misc.o mpp_io_write.o mpp_io_read.o mpp_io_connect.o mpp.o mpp_domains.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io.F90
mpp_domains_util.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_util.F90 mpp.o mpp_parameter.o mpp_datatype.o mpp_data.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_util.F90
fms.o: /nfsuser/g01/wx20wn/mom4d/src/shared/fms/fms.f90 mpp.o mpp_domains.o mpp_io.o fms_io.o memutils.o constants.o
	$(FC) $(FFLAGS) -c	/nfsuser/g01/wx20wn/mom4d/src/shared/fms/fms.f90
fms_io.o: /nfsuser/g01/wx20wn/mom4d/src/shared/fms/fms_io.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_2d.inc /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_3d.inc /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_4d.inc /nfsuser/g01/wx20wn/mom4d/src/shared/fms/write_data.inc mpp_io.o mpp_domains.o mpp.o platform.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include -I/nfsuser/g01/wx20wn/mom4d/src/shared/fms	/nfsuser/g01/wx20wn/mom4d/src/shared/fms/fms_io.F90
mpp_io_util.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_util.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h mpp.o mpp_datatype.o mpp_data.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_util.F90
mpp_domains_misc.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_misc.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_update_domains2D.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_old.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_old.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_new.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_new.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_old.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_new.h mpp.o mpp_parameter.o mpp_datatype.o mpp_data.o mpp_domains_util.o mpp_domains_comm.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include -I/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_misc.F90
mpp_util.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_util.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/system_clock.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_sma.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_mpi.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_nocomm.inc mpp_parameter.o mpp_datatype.o mpp_data.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include -I/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_util.F90
mpp_comm.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_comm.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/system_clock.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_sma.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_sma.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_sma.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_sma.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_mpi.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_mpi.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_mpi.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_mpi.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_nocomm.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_nocomm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_nocomm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_nocomm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_chksum_int.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_chksum.h mpp_parameter.o mpp_data.o mpp_util.o mpp_datatype.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include -I/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_comm.F90
idealized_ic.o: /nfsuser/g01/wx20wn/mom4d/src/preprocessing/mom4_prep/idealized_ic/idealized_ic.f90 fms.o mpp.o mpp_io.o mpp_domains.o constants.o
	$(FC) $(FFLAGS) -c	/nfsuser/g01/wx20wn/mom4d/src/preprocessing/mom4_prep/idealized_ic/idealized_ic.f90
mpp_io_connect.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_connect.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h mpp.o mpp_parameter.o mpp_datatype.o mpp_data.o mpp_io_misc.o mpp_io_write.o mpp_io_read.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_connect.F90
nsclock.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/nsclock.c
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) -c	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/nsclock.c
mpp_io_misc.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_misc.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h mpp.o mpp_domains.o mpp_parameter.o mpp_data.o mpp_datatype.o mpp_io_util.o mpp_domains_define.o mpp_domains_reduce.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_misc.F90
mpp_io_write.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_write.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_write_2Ddecomp.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_write.h mpp.o mpp_domains.o mpp_parameter.o mpp_datatype.o mpp_data.o mpp_io_util.o mpp_io_misc.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include -I/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_write.F90
mpp_parameter.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_parameter.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_parameter.F90
mpp_domains_comm.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_comm.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_domains_comm.h mpp.o mpp_parameter.o mpp_datatype.o mpp_data.o mpp_domains_util.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include -I/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_comm.F90
platform.o: /nfsuser/g01/wx20wn/mom4d/src/shared/platform/platform.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include	/nfsuser/g01/wx20wn/mom4d/src/shared/platform/platform.F90
mpp.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h mpp_parameter.o mpp_data.o mpp_comm.o mpp_util.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp.F90
mpp_io_read.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_read.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_read_2Ddecomp.h mpp.o mpp_domains.o mpp_parameter.o mpp_datatype.o mpp_data.o mpp_io_util.o mpp_io_misc.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include -I/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_read.F90
mpp_domains.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h mpp_parameter.o mpp_datatype.o mpp_data.o mpp_domains_util.o mpp_domains_reduce.o mpp_domains_misc.o mpp_domains_define.o mpp.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains.F90
memuse.o: /nfsuser/g01/wx20wn/mom4d/src/shared/memutils/memuse.c
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) -c	/nfsuser/g01/wx20wn/mom4d/src/shared/memutils/memuse.c
mpp_domains_define.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_define.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h mpp.o mpp_parameter.o mpp_datatype.o mpp_data.o mpp_domains_util.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c -I/nfsuser/g01/wx20wn/mom4d/include	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_define.F90
memutils.o: /nfsuser/g01/wx20wn/mom4d/src/shared/memutils/memutils.F90 mpp.o
	$(FC) $(CPPDEFS_XLF) $(FFLAGS) -c	/nfsuser/g01/wx20wn/mom4d/src/shared/memutils/memutils.F90
threadloc.o: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/threadloc.c
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) -c	/nfsuser/g01/wx20wn/mom4d/src/shared/mpp/threadloc.c
./mpp_io_misc.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_misc.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_misc.F90 .
./mpp_global_field.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_field.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_field.h .
./mpp_domains_misc.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_misc.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_misc.F90 .
./mpp_domains_util.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_util.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_util.F90 .
./mpp_do_updateV_gsm.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_gsm.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_gsm.h .
./mpp_global_reduce.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_reduce.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_reduce.h .
./mpp_chksum.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_chksum.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_chksum.h .
./mpp_reduce_nocomm.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_nocomm.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_nocomm.h .
./mpp_comm_nocomm.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_nocomm.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_nocomm.inc .
./mpp_io_read.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_read.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_read.F90 .
./system_clock.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/system_clock.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/system_clock.h .
./mpp_domains_comm.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_domains_comm.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_domains_comm.h .
./mpp_util.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_util.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_util.F90 .
./mpp_sum_sma.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_sma.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_sma.h .
./read_data_2d.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_2d.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_2d.inc .
./mpp_io_connect.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_connect.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_connect.F90 .
./mpp_reduce_caf.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_caf.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_caf.h .
./mpp_util_mpi.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_mpi.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_mpi.inc .
./mpp_domains_comm.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_comm.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_comm.F90 .
./constants.f90: /nfsuser/g01/wx20wn/mom4d/src/shared/constants/constants.f90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/constants/constants.f90 .
./mpp_do_global_field_new.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_new.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_new.h .
./mpp_do_update_caf.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_caf.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_caf.h .
./mpp_data_nocomm.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_nocomm.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_nocomm.inc .
./mpp_domains_reduce.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_reduce.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_reduce.F90 .
./mpp_domains_define.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_define.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_define.F90 .
./mpp_datatype.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_datatype.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_datatype.F90 .
./mpp_data.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_data.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_data.F90 .
./mpp_do_update_gsm.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_gsm.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_gsm.h .
./mpp_update_domains2D.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_update_domains2D.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_update_domains2D.h .
./read_data_3d.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_3d.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_3d.inc .
./mpp_transmit_nocomm.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_nocomm.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_nocomm.h .
./mpp_reduce_mpi.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_mpi.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_mpi.h .
./mpp_do_update_new.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_new.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_new.h .
./memutils.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/memutils/memutils.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/memutils/memutils.F90 .
./mpp_io_util.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_util.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_util.F90 .
./platform.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/platform/platform.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/platform/platform.F90 .
./mpp_do_global_field_old.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_old.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_old.h .
./mpp_domains.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains.F90 .
./mpp_reduce_gsm.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_gsm.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_gsm.h .
./mpp_do_global_field_caf.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_caf.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_caf.h .
./mpp_io.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io.F90 .
./mpp_util_nocomm.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_nocomm.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_nocomm.inc .
./memuse.c: /nfsuser/g01/wx20wn/mom4d/src/shared/memutils/memuse.c
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/memutils/memuse.c .
./fms.f90: /nfsuser/g01/wx20wn/mom4d/src/shared/fms/fms.f90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/fms/fms.f90 .
./idealized_ic.f90: /nfsuser/g01/wx20wn/mom4d/src/preprocessing/mom4_prep/idealized_ic/idealized_ic.f90
	cp /nfsuser/g01/wx20wn/mom4d/src/preprocessing/mom4_prep/idealized_ic/idealized_ic.f90 .
./mpp_do_redistribute_old.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_old.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_old.h .
./mpp_transmit_mpi.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_mpi.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_mpi.h .
./mpp_sum_gsm.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_gsm.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_gsm.h .
./mpp_write_2Ddecomp.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_write_2Ddecomp.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_write_2Ddecomp.h .
./mpp_sum_caf.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_caf.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_caf.h .
./write_data.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/fms/write_data.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/fms/write_data.inc .
./mpp_comm_sma.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_sma.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_sma.inc .
./mpp_do_updateV_old.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_old.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_old.h .
./mpp_comm_mpi.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_mpi.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_mpi.inc .
./mpp_reduce_sma.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_sma.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_sma.h .
./nsclock.c: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/nsclock.c
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/nsclock.c .
./mpp_sum.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum.inc .
./mpp_util_sma.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_sma.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_sma.inc .
./mpp_comm.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_comm.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_comm.F90 .
./mpp_write.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_write.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_write.h .
./fms_io.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/fms/fms_io.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/fms/fms_io.F90 .
./fms_platform.h: /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h
	cp /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h .
./mpp_io_write.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_write.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_write.F90 .
./mpp_datatype.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_datatype.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_datatype.h .
./mpp_do_redistribute_caf.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_caf.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_caf.h .
./mpp.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp.F90 .
./mpp_data_sma.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_sma.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_sma.inc .
./mpp_global_sum.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_sum.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_sum.h .
./mpp_do_updateV_new.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_new.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_new.h .
./read_data_4d.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_4d.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_4d.inc .
./mpp_transmit.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit.inc .
./mpp_do_redistribute_gsm.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_gsm.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_gsm.h .
./mpp_transmit_sma.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_sma.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_sma.h .
./mpp_parameter.F90: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_parameter.F90
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_parameter.F90 .
./mpp_sum_mpi.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_mpi.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_mpi.h .
./idealized_ic_driver.f90: /nfsuser/g01/wx20wn/mom4d/src/preprocessing/mom4_prep/idealized_ic/idealized_ic_driver.f90
	cp /nfsuser/g01/wx20wn/mom4d/src/preprocessing/mom4_prep/idealized_ic/idealized_ic_driver.f90 .
./mpp_sum_nocomm.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_nocomm.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_nocomm.h .
./mpp_do_updateV_caf.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_caf.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_caf.h .
./mpp_read_2Ddecomp.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_read_2Ddecomp.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_read_2Ddecomp.h .
./mpp_do_global_field_gsm.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_gsm.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_gsm.h .
./threadloc.c: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/threadloc.c
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/threadloc.c .
./mpp_do_update_old.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_old.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_old.h .
./mpp_chksum_int.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_chksum_int.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_chksum_int.h .
./mpp_data_mpi.inc: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_mpi.inc
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_mpi.inc .
./mpp_do_redistribute_new.h: /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_new.h
	cp /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_new.h .
SRC = /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_reduce.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_data.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/constants/constants.f90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_datatype.F90 /nfsuser/g01/wx20wn/mom4d/src/preprocessing/mom4_prep/idealized_ic/idealized_ic_driver.f90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_util.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/fms/fms.f90 /nfsuser/g01/wx20wn/mom4d/src/shared/fms/fms_io.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_util.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_misc.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_util.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_comm.F90 /nfsuser/g01/wx20wn/mom4d/src/preprocessing/mom4_prep/idealized_ic/idealized_ic.f90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_connect.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/nsclock.c /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_misc.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_write.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_parameter.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_comm.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/platform/platform.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_read.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/memutils/memuse.c /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_define.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/memutils/memutils.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/threadloc.c /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_field.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_old.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_reduce.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_chksum.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_mpi.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_write_2Ddecomp.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_nocomm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_nocomm.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_sma.inc /nfsuser/g01/wx20wn/mom4d/src/shared/fms/write_data.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/system_clock.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_domains_comm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_mpi.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_sma.h /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_2d.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_old.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_sma.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_mpi.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_sma.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_new.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_nocomm.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_write.h /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_datatype.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_update_domains2D.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_3d.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_nocomm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_sum.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_sma.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_new.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_mpi.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit.inc /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_4d.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_sma.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_mpi.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_new.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_nocomm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_old.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_read_2Ddecomp.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_old.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_chksum_int.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_nocomm.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_mpi.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_new.h
OBJ = mpp_domains_reduce.o mpp_data.o constants.o mpp_datatype.o idealized_ic_driver.o mpp_io.o mpp_domains_util.o fms.o fms_io.o mpp_io_util.o mpp_domains_misc.o mpp_util.o mpp_comm.o idealized_ic.o mpp_io_connect.o nsclock.o mpp_io_misc.o mpp_io_write.o mpp_parameter.o mpp_domains_comm.o platform.o mpp.o mpp_io_read.o mpp_domains.o memuse.o mpp_domains_define.o memutils.o threadloc.o
OFF = /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_misc.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_field.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_misc.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_util.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_reduce.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_chksum.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_nocomm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_nocomm.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_read.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/system_clock.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_domains_comm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_util.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_sma.h /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_2d.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_connect.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_mpi.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_comm.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/constants/constants.f90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_new.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_nocomm.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_reduce.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains_define.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_datatype.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_data.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_update_domains2D.h /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_3d.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_nocomm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_mpi.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_new.h /nfsuser/g01/wx20wn/mom4d/src/shared/memutils/memutils.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_util.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/platform/platform.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_old.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_domains.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_nocomm.inc /nfsuser/g01/wx20wn/mom4d/src/shared/memutils/memuse.c /nfsuser/g01/wx20wn/mom4d/src/shared/fms/fms.f90 /nfsuser/g01/wx20wn/mom4d/src/preprocessing/mom4_prep/idealized_ic/idealized_ic.f90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_old.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_mpi.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_write_2Ddecomp.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/fms/write_data.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_sma.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_old.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_comm_mpi.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_reduce_sma.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/nsclock.c /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_util_sma.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_comm.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_write.h /nfsuser/g01/wx20wn/mom4d/src/shared/fms/fms_io.F90 /nfsuser/g01/wx20wn/mom4d/include/fms_platform.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_io_write.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_datatype.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_sma.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_global_sum.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_new.h /nfsuser/g01/wx20wn/mom4d/src/shared/fms/read_data_4d.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_transmit_sma.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/mpp_parameter.F90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_mpi.h /nfsuser/g01/wx20wn/mom4d/src/preprocessing/mom4_prep/idealized_ic/idealized_ic_driver.f90 /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_sum_nocomm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_updateV_caf.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_read_2Ddecomp.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_global_field_gsm.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/threadloc.c /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_update_old.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_chksum_int.h /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_data_mpi.inc /nfsuser/g01/wx20wn/mom4d/src/shared/mpp/include/mpp_do_redistribute_new.h
clean: neat
	-rm -f .cppdefs $(OBJ) idealized_ic.exe
neat:
	-rm -f $(TMPFILES)
localize: $(OFF)
	cp $(OFF) .
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
idealized_ic.exe: $(OBJ)
	$(LD) $(OBJ) -o idealized_ic.exe $(LDFLAGS)
