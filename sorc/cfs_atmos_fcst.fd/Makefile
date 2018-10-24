XEC     = $(EXECM)
F77      = $(F77M)
F90      = $(F90M)
CC       = $(FCC)

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif
include $(ESMFMKFILE)
FINCS    = $(ESMF_MOD)  ##$(ESMF_F90COMPILEPATHS)

ARCH     = $(ARCHM)
PGSZ     = $(PGSZM)
FRR      = $(FRRM)
FXX      = $(FXXM)
OPTS     = $(OPTSM)
OPTS_SER = $(OPTS_SERM)
OPTS90   = $(OPTS90M)
OPTS90A  = $(OPTS90AM)
OPTSIO   = $(OPTSIOM)
FFLAG90  = $(OPTS90) $(FINCS) $(FINCM) $(FRR)
FFLAG90A = $(OPTS90A) $(FINCS) $(FRR)
FFLAGS   = $(OPTS) $(TRAPS) $(FXX)  ### -no-gen-interfaces  ## turn off arg type checking
FFLAGX   = $(OPTS) $(TRAPS) $(FXX)
FFLAGIO  = $(OPTSIO) $(TRAPS) $(FXX)
FFLAGY   = $(OPTS) $(FXX)
FFLAGM   = $(OPTS) $(TRAPS) $(DEBUG) $(FXX)
FFLAGM2  = $(OPTS) $(TRAPS) $(DEBUG) $(FXX)
FFLAGM3  = $(OPTS) $(TRAPS) $(DEBUG) $(FRR)
FFLAGSF  = $(OPTSBT) $(FRR)
FFLAGSI  = $(OPTSBT) $(FRR)
FFLAGB   = $(OPTSBT) $(FXX)
FFLAG_SER = $(OPTS_SER) $(DOUBLEFLAG) $(TRAPS) $(DEBUG) $(FXX)
FFLAGSC   =

LDR      = $(LDRM)
ESMFLIB  = -L$(ESMF_LIB)
LDFLAGS =  $(LDFLAGSM)
LIBS =  $(LIBSM) $(ESMF_MOD)

.SUFFIXES: .o .f .F .h

#
# *****************************************************************
#
OBJS0	= NameList_ESMFMod.o          \
	  Sfc_Var_ESMFMod.o           \
	  GFS_ErrMsgMod.o             \
	  GFS_ESMFStateAddGetMod.o    \
	  GFS_InternalState_ESMFMod.o \
	  GFS_ESMFStateMod.o          \
	  GFS_GetCf_ESMFMod.o         \
	  GFS_Initialize_ESMFMod.o    \
	  GFS_Run_ESMFMod.o           \
	  GFS_Finalize_ESMFMod.o      \
	  GFS_GridComp_ESMFMod.o      \
	  GFS_Standalone_ESMF_ENS.o   \
	  Grid_ESMFCreate.o           \
	  StartTimeGet_ESMF.o         \
	  Ensemble_sub.o



OBJ_MOD	= machine.o             \
          num_parthds.o         \
	  module.o              \
	  iounitdef.o           \
	  physcons_v.o          \
	  funcphys_v.o          \
	  progtm_module.o       \
	  rascnvv2_v.o          \
          mersenne_twister.o    \
	  resol_def.o           \
	  gg_def.o              \
	  vert_def.o            \
	  sig_io.o              \
	  date_def.o            \
	  layout1.o             \
	  namelist_def.o        \
	  namelist_soilveg.o    \
	  coordinate_def.o      \
	  tracer_const_h-new.o  \
	  reduce_lons_grid_module.o \
	  matrix_sig_def.o      \
	  mpi_def.o             \
	  semi_lag_def.o        \
	  deldifs_def.o         \
	  bfilt_def.o           \
	  sfcio_module.o        \
	  sigio_module.o        \
	  sigio_r_module.o      \
	  gfsio_module.o        \
	  gfsio_def.o           \
 	  d3d_def.o             \
	  module_bfmicrophysics.o


OBJS	= \
	get_cd_hyb.o get_am_bm_hyb.o\
	get_cd_hyb_gc.o get_am_bm_hyb_gc.o\
	wrt3d_hyb.o\
        wnew.o     \
	setlats.o\
	getysk.o\
	getysk_gc.o\
	redist_lats_a.o\
	redist_lats_r_b_fix.o\
	ifshuff.o\
	shuffle_grid.o\
	digifilt.o\
	gcycle_v.o\
	wrtout.o\
	grid_collect.o\
	info.o\
	four2grid_thread.o\
	hpmdummy.o\
	coundummy.o\
	damp_speed.o\
	deldifs.o\
	fix_fields.o\
	spect_fields.o\
	delnpe.o\
	delnpo.o \
	dezouv.o\
	impadj_sig.locl.o\
	impadj_hyb.locl.o\
	impadj_hyb.locl_gc.o\
	dozeuv.o\
	epslon.o\
	filtr1eo.o \
	filtr2eo.o\
	get_ls_node.o\
	get_lats_node_shuff_a_fix.o\
	get_lats_node_shuff_r_fix.o\
	getcon.o \
	gfidi_sig.o\
	gfidi_hyb.o\
	gfidi_hyb_gc.o\
	gfidi_hyb_gc_h-new.o\
	glats.o\
	gloopa_opt.o\
	lon_lat_para.o\
	compns.o\
	gozrineo.o\
	pln2eo.o \
	rms_spect.o \
	lats_shuff.o\
	uvoedz.o                     \
	mpi_quit.o                   \
	sicdif_sig.o                 \
	sicdif_hyb.o                 \
	sicdif_hyb_gc.o              \
	dotstep_sicdif.o             \
	gather_times.o               \
	four2fln.o                   \
	four2flnio.o                 \
	sortrx.o                     \
	sumfln.o                     \
	sumder2.o                    \
	triseori.o                   \
	uveodz.o                     \
	ysminv.o                     \
        fftpack.o		     \
	byteswap.o                   \
	ver_sig.o       


OBJS_PORT	= \
	fftpack.o \
	four2grid_thread.fftpack.o \
	noblas.o\
	funcphys_subsx.o\
#
OBJS_RAD	=              \
	radlw_param.o          \
	radlw_datatb.o         \
	radlw_main.o           \
	radsw_param.o          \
	radsw_datatb.o         \
	radsw_main.o           \
	radiation_astronomy.o  \
	radiation_aerosols.o   \
	radiation_gases.o      \
	radiation_clouds.o     \
	radiation_surface.o    \
	grrad.o                \
	gloopr.o

#

OBJS_PHY= \
	ozinterp.o          \
	ozphys_v.o          \
	gloopb_call.gbphys.o\
	gbphys_v.o          \
	gbphys_call.o       \
	gbphys_adv_hyb_gc.o \
	gbphys_adv_hyb_gc_h-new.o \
	dcyc2_v.o           \
	dcyc2_v.pre.rad.o   \
	set_soilveg.o       \
	sfc_drv.o           \
	sfc_land.o          \
	progt2_v.o          \
	sfc_sice.o          \
	sfc_ocean.o         \
	sfc_diff.o          \
	sfc_diag.o          \
	sflx.o              \
	moninp_v.o          \
	moninp1_v.o         \
	moninq_v.o          \
	moninq1_v.o         \
	gwdps_v.o           \
	gwdc.o              \
	sascnv_v.o          \
	sascnvn_v.o         \
	cnvcld_v.o          \
	shalcv_v.o          \
	shalcv_v_opr.o      \
	shalcnv_v.o         \
	lrgsclr_v.o         \
	gscond_v.o          \
	precpd_v.o          \
	mstadb_v.o          \
	mstcnv_v.o          \
	get_prs_v.o         \
	gsmddrive_v.o       \
	omegtes.o           \
	omegtes_gc.o        \
	omegas.o            \
	hyb2sig.o           \
	hyb2press.o         \
	hyb2press_gc.o      \
	sig2press.o 

###funcphys_subsx.o   only srt gpxs was called in fix_fields -table not used

OBJS_IO= \
	read_fix.o         \
	gribit.o           \
	wrt3d.o            \
	wrtsfc.o           \
	para_fixio_w.o     \
	sfcsub.o           \
	treadeo.io.o       \
	treadeo.gfsio.o    \
	grid_to_spec.o     \
	spect_to_grid.o    \
	spect_tv_enthalpy_ps.o\
	setsig.o           \
	twriteeo.o         \
	bafrio.o           \
	spect_send.o       \
	spect_write.o


OBJS_CC= \
	mpi_more.o  \
	cmp.comm.o  \
	atm.comm.o  \
	tiles.o

SRC	= $(OBJS0:.o=.f) $(OBJ_MOD:.o=.f) $(OBJS:.o=.f) $(OBJS_RAD:.o=.f) $(OBJS_PHY:.o=.f) $(OBJS_IO:.o=.f) $(OBJS_CC:.o=.f)
#
INCS = f_hpm.h mpi_inc.h function2

#
# *****************************************************************
#
all: model-mpi

model-mpi: $(OBJ_MOD) $(OBJS_CC) $(OBJS0) $(OBJS) $(OBJS_PHY) $(OBJS_RAD) $(OBJS_IO) 
	$(LDR) $(LDFLAGS) -o $(EXEC) $(OBJ_MOD) $(OBJS_CC) $(OBJS0) $(OBJS) $(OBJS_PHY) $(OBJS_RAD) $(OBJS_IO) $(LIBS)

clean:
	rm -f $(OBJ_MOD) $(OBJS0) $(OBJS) $(OBJS_RAD) $(OBJS_PHY) $(OBJS_IO) *.mod

tar:
	tar -cvf tar.gfs.r4r8 $(SRC) $(INCS) $(COMS) $(OBJS_PORT:.o=.f) lonsper* res* xx* Makefile* ini.* scr.* m*real_?

.F.o:
	$(F77) $(FFLAGS) -c -d $< 
	#$(F77) $(FFLAGS) -c -d -WF,-DCLR:${RASV} $< 
.f.o:
	$(F77) $(FFLAGS) -c $< 


omegas.o:	omegas.f
		$(F77) $(FFLAGM) -c omegas.f

#
# *****************************************************************
#
cnvcld_v.o:	cnvcld_v.f
		$(F77) $(FFLAGM) -c cnvcld_v.f

dcyc2_v.o:	dcyc2_v.f
		$(F77) $(FFLAGM) -c dcyc2_v.f

dcyc2_v.pre.rad.o:	dcyc2_v.pre.rad.f
		$(F77) $(FFLAGM) -c dcyc2_v.pre.rad.f

digifilt.o:	digifilt.f
		$(F77) $(FFLAGX) -c digifilt.f

funcphys_subsx.o:	funcphys_subsx.f
		$(F77) $(FFLAGM) -c funcphys_subsx.f

gbphys_adv_hyb_gc.o:	gbphys_adv_hyb_gc.f
		$(F77) $(FFLAGM) -c gbphys_adv_hyb_gc.f

gbphys_adv_hyb_gc_h-new.o:	gbphys_adv_hyb_gc_h-new.f
		$(F77) $(FFLAGM) -c gbphys_adv_hyb_gc_h-new.f

gbphys_call.o:	gbphys_call.f
		$(F77) $(FFLAGM) -c gbphys_call.f

gbphys_v.o:	gbphys_v.f
		$(F77) $(FFLAGM) -c gbphys_v.f

get_prs_v.o:	get_prs_v.f
		$(F77) $(FFLAGM) -c get_prs_v.f

gscond_v.o:	gscond_v.f
		$(F77) $(FFLAGM) -c gscond_v.f

gsmddrive_v.o:	gsmddrive_v.f
		$(F77) $(FFLAGM) -c gsmddrive_v.f

module_bfmicrophysics.o:	module_bfmicrophysics.f
		$(F77) $(FFLAGM) -c module_bfmicrophysics.f

gwdps_v.o:	gwdps_v.f
		$(F77) $(FFLAGM) -c gwdps_v.f

gwdc.o:		gwdc.f
		$(F77) $(FFLAGM) -c gwdc.f

hyb2press_gc.o:	hyb2press_gc.f
		$(F77) $(FFLAGM) -c hyb2press_gc.f

hyb2press.o:	hyb2press.f
		$(F77) $(FFLAGM) -c hyb2press.f

hyb2sig.o:	hyb2sig.f
		$(F77) $(FFLAGM) -c hyb2sig.f

lrgsclr_v.o:	lrgsclr_v.f
		$(F77) $(FFLAGM) -c lrgsclr_v.f

moninp_v.o:	moninp_v.f
		$(F77) $(FFLAGM) -c moninp_v.f

moninp1_v.o:	moninp1_v.f
		$(F77) $(FFLAGM) -c moninp1_v.f

moninq_v.o:	moninq_v.f
		$(F77) $(FFLAGM) -c moninq_v.f

moninq1_v.o:	moninq1_v.f
		$(F77) $(FFLAGM) -c moninq1_v.f

mstadb_v.o:	mstadb_v.f
		$(F77) $(FFLAGM) -c mstadb_v.f

mstcnv_v.o:	mstcnv_v.f
		$(F77) $(FFLAGM) -c mstcnv_v.f

omegtes_gc.o:	omegtes_gc.f
		$(F77) $(FFLAGM) -c omegtes_gc.f

omegtes.o:	omegtes.f
		$(F77) $(FFLAGM) -c omegtes.f

ozinterp.o:	ozinterp.f
		$(F77) $(FFLAGM) -c ozinterp.f

ozphys_v.o:	ozphys_v.f
		$(F77) $(FFLAGM) -c ozphys_v.f

precpd_v.o:	precpd_v.f
		$(F77) $(FFLAGM) -c precpd_v.f

rascnvv2_v.o:	rascnvv2_v.f
		$(F77) $(FFLAGM) -c rascnvv2_v.f

sfc_sice.o:	sfc_sice.f
		$(F77) $(FFLAGM) -c sfc_sice.f

set_soilveg.o:	set_soilveg.f
		$(F77) $(FFLAGM) -c set_soilveg.f

namelist_soilveg.o:	namelist_soilveg.f
		$(F77) $(FFLAGM) -c namelist_soilveg.f

sfc_land.o:	sfc_land.f
		$(F77) $(FFLAGM) -c sfc_land.f
                                                                            
progt2_v.o:	progt2_v.f
		$(F77) $(FFLAGM) -c progt2_v.f

sfc_drv.o:	sfc_drv.f
		$(F77) $(FFLAGM) -c sfc_drv.f

sflx.o: 	sflx.f
		$(F77) $(FFLAGM) -c sflx.f

sfc_ocean.o:	sfc_ocean.f
		$(F77) $(FFLAGM) -c sfc_ocean.f

sfc_diff.o:	sfc_diff.f
		$(F77) $(FFLAGM) -c sfc_diff.f

sfc_diag.o:	sfc_diag.f
		$(F77) $(FFLAGM) -c sfc_diag.f

sascnv_v.o:	sascnv_v.f
		$(F77) $(FFLAGM) -c sascnv_v.f

sascnvn_v.o:	sascnvn_v.f
		$(F77) $(FFLAGM) -c sascnvn_v.f

shalcv_v.o:	shalcv_v.f
		$(F77) $(FFLAGM) -c shalcv_v.f

shalcv_v_opr.o:	shalcv_v_opr.f
		$(F77) $(FFLAGM) -c shalcv_v_opr.f

shalcnv_v.o:	shalcnv_v.f
		$(F77) $(FFLAGM) -c shalcnv_v.f

sig2press.o:	sig2press.f
		$(F77) $(FFLAGM) -c sig2press.f

# *****************************************************************

radlw_param.o:	radlw_param.f
		$(F77) $(FFLAGM) -c radlw_param.f


radlw_datatb.o:	radlw_datatb.f
		$(F77) $(FFLAGM) -c radlw_datatb.f


radlw_main.o:	radlw_main.f
		$(F77) $(FFLAGM2) -c radlw_main.f

radsw_param.o:	radsw_param.f
		$(F77) $(FFLAGM) -c radsw_param.f

radsw_datatb.o:	radsw_datatb.f
		$(F77) $(FFLAGM) -c radsw_datatb.f

radsw_main.o:	radsw_main.f
		$(F77) $(FFLAGM2) -c radsw_main.f

radiation_astronomy.o:	radiation_astronomy.f
		$(F77) $(FFLAGM) -c radiation_astronomy.f

radiation_aerosols.o:	radiation_aerosols.f
		$(F77) $(FFLAGM) -c radiation_aerosols.f

radiation_gases.o:	radiation_gases.f
		$(F77) $(FFLAGM) -c radiation_gases.f

radiation_clouds.o:	radiation_clouds.f
		$(F77) $(FFLAGM) -c radiation_clouds.f

radiation_surface.o:	radiation_surface.f
		$(F77) $(FFLAGM) -c radiation_surface.f

grrad.o:	grrad.f
		$(F77) $(FFLAGM) -c grrad.f

progtm_module.o:	progtm_module.f
		$(F77) $(FFLAGM) -c progtm_module.f

machine.o:	machine.f
		$(F77) $(FFLAGM) -c machine.f

#
# *****************************************************************
#
physcons_v.o:	physcons_v.f
		$(F77) $(FFLAG90) -c physcons_v.f

iounitdef.o:	iounitdef.f
		$(F77) $(FFLAG90) -c iounitdef.f

funcphys_v.o:	funcphys_v.f
		$(F77) $(FFLAG90) -c funcphys_v.f

sfcio_module.o:	sfcio_module.f
		$(F77) $(FFLAGSF) -c sfcio_module.f

sigio_module.o:	sigio_module.f
		$(F77) $(FFLAGSI) -c sigio_module.f

gfsio_def.o:	gfsio_def.f
		$(F77) $(FFLAGSI) -c gfsio_def.f

gfsio_module.o:	gfsio_module.f
		$(F77) $(FFLAGSI) -c gfsio_module.f

sigio_r_module.o:	sigio_r_module.f
		$(F77) $(FFLAGSI) -c sigio_r_module.f

bafrio.o:	bafrio.f
		$(F77) $(FFLAGB) -c bafrio.f

read_fix.o:	read_fix.f
		$(F77) $(FFLAGX) -c read_fix.f

softcount.o:	softcount.f
		$(F77) $(FFLAGX) -c softcount.f

gloopr.o:	gloopr.f
		$(F77) $(FFLAGM2) -c gloopr.f

gloopb_call.gbphys.o:	gloopb_call.gbphys.f
		$(F77) $(FFLAGM2) -c gloopb_call.gbphys.f

sfcsub.o:	sfcsub.f
		$(F77) $(FFLAGM) -c sfcsub.f

gcycle_v.o:	gcycle_v.f
		$(F77) $(FFLAGM) -c gcycle_v.f

getaer.o:	getaer.f
		$(F77) $(FFLAGIO) -c getaer.f

wrt3d.o:	wrt3d.f
		$(F77) $(FFLAGIO) -c wrt3d.f

gribit.o:	gribit.f
		$(F77) $(FFLAGIO) -c gribit.f

wrtsfc.o:	wrtsfc.f
		$(F77) $(FFLAGIO) -c wrtsfc.f

para_fixio_w.o:		para_fixio_w.f
		$(F77) $(FFLAGIO) -c para_fixio_w.f

treadeo.io.o:	treadeo.io.f
		$(F77) $(FFLAGIO) -c treadeo.io.f

treadeo.gfsio.o:	treadeo.gfsio.f
		$(F77) $(FFLAGIO) -c treadeo.gfsio.f

twriteeo.o:	twriteeo.f
		$(F77) $(FFLAGIO) -c twriteeo.f

spect_send.o:	spect_send.f
		$(F77) $(FFLAGIO) -c spect_send.f

spect_write.o:	spect_write.f
		$(F77) $(FFLAGIO) -c spect_write.f

spect_to_grid.o:	spect_to_grid.f
		$(F77) $(FFLAGIO) -c spect_to_grid.f

spect_tv_enthalpy_ps.o:	spect_tv_enthalpy_ps.f
		$(F77) $(FFLAGIO) -c spect_tv_enthalpy_ps.f

grid_to_spec.o:	grid_to_spec.f
		$(F77) $(FFLAGIO) -c grid_to_spec.f

wrtout.o:	wrtout.f
		$(F77) $(FFLAGIO) -c wrtout.f

NameList_ESMFMod.o:	 NameList_ESMFMod.f
		$(F77) $(FFLAG90) -c NameList_ESMFMod.f

Sfc_Var_ESMFMod.o:	Sfc_Var_ESMFMod.f
		$(F77) $(FFLAG90) -c Sfc_Var_ESMFMod.f

GFS_ESMFStateAddGetMod.o:	 GFS_ESMFStateAddGetMod.f
		$(F77) $(FFLAG90) -c GFS_ESMFStateAddGetMod.f

GFS_InternalState_ESMFMod.o:	 GFS_InternalState_ESMFMod.f
		$(F77) $(FFLAG90) -c GFS_InternalState_ESMFMod.f

GFS_ESMFStateMod.o:	 GFS_ESMFStateMod.f
		$(F77) $(FFLAG90) -c GFS_ESMFStateMod.f

GFS_ErrMsgMod.o:	 GFS_ErrMsgMod.f
		$(F77) $(FFLAG90) -c GFS_ErrMsgMod.f

GFS_GetCf_ESMFMod.o:	 GFS_GetCf_ESMFMod.f
		$(F77) $(FFLAG90) -c GFS_GetCf_ESMFMod.f

GFS_ESMFMod.o:	 GFS_ESMFMod.f
		$(F77) $(FFLAG90) -c GFS_ESMFMod.f

#GFS_Grid_fnl_ESMFMod.o:	 GFS_Grid_fnl_ESMFMod.f
	#$(F77) $(FFLAG90A) -c GFS_Grid_fnl_ESMFMod.f

GFS_GridComp_ESMFMod.o:	 GFS_GridComp_ESMFMod.f
		$(F77) $(FFLAG90A) -c GFS_GridComp_ESMFMod.f

GFS_Initialize_ESMFMod.o:	 GFS_Initialize_ESMFMod.f
		$(F77) $(FFLAG90) -c GFS_Initialize_ESMFMod.f

GFS_Run_ESMFMod.o:	GFS_Run_ESMFMod.f
		$(F77) $(FFLAG90) -c GFS_Run_ESMFMod.f

GFS_Finalize_ESMFMod.o:	GFS_Finalize_ESMFMod.f
		$(F77) $(FFLAG90) -c GFS_Finalize_ESMFMod.f

GFS_InputFile2ImportState.o:	 GFS_InputFile2ImportState.f
		$(F77) $(FFLAG90) -c GFS_InputFile2ImportState.f

GFS_Standalone_ESMF_ENS.o:	 GFS_Standalone_ESMF_ENS.f
		$(F77) $(FFLAG90) -c GFS_Standalone_ESMF_ENS.f

Grid_ESMFCreate.o:	 Grid_ESMFCreate.f
		$(F77) $(FFLAG90) -c Grid_ESMFCreate.f

StartTimeGet_ESMF.o:	 StartTimeGet_ESMF.f
		$(F77) $(FFLAG90) -c StartTimeGet_ESMF.f

Ensemble_sub.o:	 Ensemble_sub.f
		$(F77) $(FFLAG90) -c Ensemble_sub.f

mpi_more.o:	mpi_more.f
		$(F77) $(FFLAGS) -c mpi_more.f

cmp.comm.o:	cmp.comm.f
		$(F77) $(FFLAGS) -c cmp.comm.f

atm.comm.o:	atm.comm.f
		$(F77) $(FFLAGS) -c atm.comm.f

tiles.o:	tiles.f
		$(F77) $(FFLAGS) -c tiles.f

