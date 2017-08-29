  MODULE GFS_ESMFStateMod

!  June 2005 Weiyu Yang             Initial code.
!  May  2008 Weiyu Yang             Updated to use the ESMF 3.1.0r library.

!
!!USES:
!
 USE ESMF_Mod                 ! The ESMF library.

! The derived type of the internal state.
!----------------------------------------
 USE GFS_InternalState_ESMFMod

! Routines which can be used to add a fortran array to 
! an ESMF state and to get a fortran array from an 
! ESMF state
!-----------------------------------------------------
 USE GFS_ESMFStateAddGetMod
 USE GFS_ErrMsgMod

 USE mpi_def

 IMPLICIT none

 CONTAINS

 SUBROUTINE GFS_ESMFImportState2InternalState(gcGFS, impGFS, Int_State, rc)

! This subroutine can be used to update the initial condition 
! fields of the internal state from the ESMF inport state.
!------------------------------------------------------------

! Every possible import state has its own turn-on/turn-off switch flag
! which can be used to fit different interface requirement from different
! outside grid component systems.
!------------------------------------------------------------------------

 USE physcons, ONLY: con_rerth, con_g

 TYPE(ESMF_GridComp),              INTENT(inout) :: gcGFS     ! ESMF grid component which contains
                                                              ! the ESMF import state.
 TYPE(ESMF_State)                                :: impGFS    ! the ESMF import state.
 TYPE(GFS_InternalState), POINTER, INTENT(inout) :: Int_State ! the internal state which contains the initial
                                                              ! condition fields to run the GFS model.
 INTEGER, OPTIONAL,                INTENT(out)   :: rc        ! error signal variable.

 TYPE(ESMF_VM)                                        :: vm   ! ESMF virtual machine,
 INTEGER                                              :: rc1, rcfinal        ! error signal variable.
 INTEGER                                              :: mm1, i, i1, i2, j, n, l, ls_dim2, ii1(2)
 INTEGER                                              :: indlsev, jbasev
 INTEGER                                              :: indlsod, jbasod
 INTEGER,                DIMENSION(lonr, latr)        :: kmsk
 REAL(KIND = kind_evod)                               :: GA2
 REAL(KIND = kind_evod), DIMENSION(lnt2)              :: TRISCA
 REAL(KIND = kind_io4),  DIMENSION(lnt2)              :: TRISCA_4
 REAL(KIND = kind_io4),  DIMENSION(lonr, latr)        :: buf, buf11, buf12, buf13
 REAL(KIND = kind_io8),  DIMENSION(lonr, lats_node_r) :: buffo

 REAL(KIND = kind_io8),  DIMENSION(lonr, lats_node_r, 3) :: buff2

 REAL(KIND = kind_io4),  DIMENSION(:, :), POINTER     :: buf1
 INTEGER,                DIMENSION(Int_State%nodes, 2):: ijn_g, displs_g
 CHARACTER(5),           DIMENSION(:),    POINTER     :: SMC_name, STC_name, SLC_name

 INCLUDE 'function2'

! Initialize the error signal variables.
!---------------------------------------
 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

 mm1     = Int_State%me + 1

 CALL ESMF_LogWrite("Begining the Run, Update Internal State with the ESMF Import State", &
                    ESMF_LOG_INFO, rc = rc1)

! Getting the global VM for the gathering data purpose.
!------------------------------------------------------
 CALL ESMF_GridCompGet(gcGFS, vm = vm, rc = rc1)
 CALL ERR_MSG1(rc1,'Error Happened When Getting the GLobal VM',rcfinal)

! Create the gathering parameter arrays which will be used
! to gather the distributed ESMF state local array to the global array.
!----------------------------------------------------------------------

! ijn_g(1) and displs_g(1) are for the spectral space sigma file arrays,
! ijn_g(2) and displs_g(2) are for the Gaussian grid surface data arrays.
!------------------------------------------------------------------------
 displs_g(1, :) = 0
 DO i = 1, Int_State%nodes
     IF(me == i-1) THEN
         ii1(1) = Int_State%lnt2_s
         ii1(2) = Int_State%lonr_s
     END IF
     CALL ESMF_VMBroadcast(vm, ii1, 2, i-1, blockingflag = ESMF_BLOCKING, rc = rc1)
     ijn_g(i, 1) = ii1(1)
     ijn_g(i, 2) = ii1(2)

 CALL ERR_MSG1(rc1,'Error Happened When VMBroadcast ijn_g',rcfinal)
     IF(i /= 1) THEN
         displs_g(i, 1) = displs_g(i-1, 1) + ijn_g(i-1, 1)
         displs_g(i, 2) = displs_g(i-1, 2) + ijn_g(i-1, 2)
     END IF
 END DO

! Get the start time and date from the ESMF import state and update the 
! fhour and idate.
!----------------------------------------------------------------------

! idate1_im:  (1) --- bfhour (integer), (2) - (5) --- idate.
!-----------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%idate1_import == 1) THEN

! Get the ESMF import state date and time information and put
! into the internal state. "GetF90ArrayFromState" is an user
! created software to get a fortran array from an ESMF state.
! The first argument is the ESMF state from which the fortran
! array will be gotten.  The second argument is the user defined
! name to identify the array in the ESMF state.  The third one
! is the obtained fortran array and the last one is the error
! signal variable.
!---------------------------------------------------------------
     CALL GetF90ArrayFromState(impGFS, 'DATE', Int_State%idate1_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,'Error Happened When Getting ESMF State - DATE_im',rcfinal)

! Put the date and time information to fhour and idate
! which will be used to run the model.
!-----------------------------------------------------
     fhour = Int_State%idate1_im(1, 1)
     DO i = 1, 4
         idate(i) = Int_State%idate1_im(1, i+1)
     END DO
 END IF

! Get the surface orography array from the ESMF import state.
!------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%z_import == 1) THEN

! Get the local surface orography array from the ESMF import state.
!------------------------------------------------------------------
     CALL GetF90ArrayFromState(impGFS, 'HS', Int_State%z_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - HS_im",rcfinal)

! Gather the local surface orography array into the global array.
!----------------------------------------------------------------
     CALL mpi_allgatherv(Int_State%z_im, ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                      displs_g, MPI_R_IO, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - z_im",rcfinal)
 
! Change the real size 4 to real size 8 for GFS run.
!---------------------------------------------------
     TRISCA = TRISCA_4

! Distribute the global surface orography array into the GFS
! internal structure array TRIE_LS and TRIO_LS which are in
! the internal state.
!-----------------------------------------------------------
     CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_GZ), &
                           Int_State%TRIO_LS(1, 1, Int_State%P_GZ), &
                           1, Int_State%LS_NODE)

! Some necessary pre-set up computations.
!----------------------------------------
     GA2 = con_g/(con_rerth * con_rerth)

     DO j = 1, LS_MAX_NODE
         L      = Int_State%LS_NODE(j)
         jbasev = Int_State%LS_NODE(j + ls_dim)
         i1     = indlsev(L, L)
         IF(MOD(L, 2) == MOD(jcap+1, 2)) THEN
             i2 = indlsev(jcap+1, L)
         ELSE
             i2 = indlsev(jcap, L)
         END IF
         DO i = i1 , i2
             Int_State%TRIE_LS(i, 1,  Int_State%P_GZ)                        &
                 = Int_State%TRIE_LS(i, 1,  Int_State%P_GZ)*Int_State%SNNP1EV(i)*GA2
             Int_State%TRIE_LS(i, 2,  Int_State%P_GZ)                        &
                 = Int_State%TRIE_LS(i, 2,  Int_State%P_GZ)*Int_State%SNNP1EV(i)*GA2
         END DO
     END DO

     ls_dim2 = ls_dim * 2
     DO j = 1, LS_MAX_NODE
         L      = Int_State%LS_NODE(j)
         jbasod = Int_State%LS_NODE(j + ls_dim2)
         i1     = indlsod(L+1, L)
         IF(MOD(L, 2) == MOD(jcap+1, 2)) THEN
             i2 = indlsod(jcap, L) 
         ELSE
             i2 = indlsod(jcap+1, L)
         END IF
         DO i = i1 , i2
             Int_State%TRIO_LS(i, 1,  Int_State%P_GZ)                        &
                 = Int_State%TRIO_LS(i, 1,  Int_State%P_GZ)*Int_State%SNNP1OD(i)*GA2
             Int_State%TRIO_LS(i, 2,  Int_State%P_GZ)                        &
                 = Int_State%TRIO_LS(i, 2,  Int_State%P_GZ)*Int_State%SNNP1OD(i)*GA2
         END DO
     END DO
 END IF

! Get the surface pressure array from the ESMF import state.
! For the detailed comments for every computational steps
! please refer to the surface orography array code.
!-----------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%ps_import == 1) THEN
     CALL GetF90ArrayFromState(impGFS, 'PS', Int_State%ps_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - PS_im",rcfinal)

     CALL mpi_allgatherv(Int_State%ps_im, ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                      displs_g, MPI_R_IO, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - ps_im",rcfinal)

     TRISCA = TRISCA_4

     CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_QM), &
                           Int_State%TRIO_LS(1, 1, Int_State%P_QM), &
                           1, Int_State%LS_NODE)
 END IF

! Get the temperature array from the ESMF import state.
!------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%temp_import == 1) THEN

! Get the local temperature array from the ESMF import state.
!------------------------------------------------------------
     CALL GetF90ArrayFromState(impGFS, 'T', Int_State%temp_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - T_im",rcfinal)

! Do loop over the vertical levels.
!----------------------------------
     DO i = 1, Int_State%levs

! Gather each level local temperature array into the global array.
!-----------------------------------------------------------------
         CALL mpi_allgatherv(Int_State%temp_im(:, i), ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                          displs_g, MPI_R_IO, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - temp_im",rcfinal)

! Change the real size 4 to real size 8 for GFS run.
!---------------------------------------------------
         TRISCA = TRISCA_4

! Distribute the global temperature array into the GFS
! internal structure array TRIE_LS and TRIO_LS which are in
! the internal state.
!----------------------------------------------------------
         CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_TEM+i-1), &
                               Int_State%TRIO_LS(1, 1, Int_State%P_TEM+i-1), &
                               1, Int_State%LS_NODE)
     END DO
 END IF

! Get the divergence array from the ESMF import state.
! For detailed line by line comments please refer to 
! the temperature code.
!-----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%div_import == 1) THEN
     CALL GetF90ArrayFromState(impGFS, 'D', Int_State%div_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - D_im",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_allgatherv(Int_State%div_im(:, i), ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                          displs_g, MPI_R_IO, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - div_im",rcfinal)

         TRISCA = TRISCA_4

         CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_DIM+i-1), &
                               Int_State%TRIO_LS(1, 1, Int_State%P_DIM+i-1), &
                               1, Int_State%LS_NODE)
      END DO
 END IF

! Get the vorticity array from the ESMF import state.
!-----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vor_import == 1) THEN
     CALL GetF90ArrayFromState(impGFS, 'Z', Int_State%vor_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - Z_im",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_allgatherv(Int_State%vor_im(:, i), ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                          displs_g, MPI_R_IO, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - vor_im",rcfinal)

         TRISCA = TRISCA_4

         CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_ZEM+i-1), &
                               Int_State%TRIO_LS(1, 1, Int_State%P_ZEM+i-1), &
                               1, Int_State%LS_NODE)
      END DO
 END IF

! Get the moisture array from the ESMF import state.
!---------------------------------------------------
 IF(Int_State%ESMF_Sta_List%q_import == 1) THEN
     CALL GetF90ArrayFromState(impGFS, 'SHUM', Int_State%q_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - SHUM_im",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_allgatherv(Int_State%q_im(:, i), ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                          displs_g, MPI_R_IO, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - q_im",rcfinal)

         TRISCA = TRISCA_4

         CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_RM+i-1), &
                               Int_State%TRIO_LS(1, 1, Int_State%P_RM+i-1), &
                               1, Int_State%LS_NODE)
      END DO
 END IF

! Get the ozone array from the ESMF import state.
!------------------------------------------------
 IF(Int_State%ESMF_Sta_List%oz_import == 1) THEN
     CALL GetF90ArrayFromState(impGFS, 'SOZ', Int_State%oz_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - SOZ_im",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_allgatherv(Int_State%oz_im(:, i), ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                          displs_g, MPI_R_IO, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - oz_im",rcfinal)

         TRISCA = TRISCA_4

         CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_RM+Int_State%levs+i-1), &
                               Int_State%TRIO_LS(1, 1, Int_State%P_RM+Int_State%levs+i-1), &
                               1, Int_State%LS_NODE)
      END DO
 END IF

! Get the cloud liquid water array from the ESMF import state.
!-------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%scld_import == 1) THEN
     CALL GetF90ArrayFromState(impGFS, 'SCLD', Int_State%scld_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - SCLD_im",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_allgatherv(Int_State%scld_im(:, i), ijn_g(mm1, 1),  MPI_R_IO, TRISCA_4, ijn_g, &
                          displs_g, MPI_R_IO, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - scld_im",rcfinal)

         TRISCA = TRISCA_4

         CALL TRISEORI(TRISCA, Int_State%TRIE_LS(1, 1, Int_State%P_RM+Int_State%levs*2+i-1), &
                               Int_State%TRIO_LS(1, 1, Int_State%P_RM+Int_State%levs*2+i-1), &
                               1, Int_State%LS_NODE)
      END DO
 END IF

! For the Gaussian grid surface data.
!------------------------------------

! Get the sea level ice mask data array from the ESMF import state.
!------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%sea_level_ice_mask_import == 1) THEN

     kmsk = 0
! Get the local sea level ice mask data array from the ESMF import state.
!------------------------------------------------------------------------
     CALL GetF90ArrayFromState(impGFS, 'SLMSK', Int_State%sea_level_ice_mask_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - SLMSK_im",rcfinal)

! Do loop over the latitudes.
!----------------------------
     DO i = 1, latr

! Gather each latitude local sea level ice mask data array into the whole latitude array.
!----------------------------------------------------------------------------------------
         CALL mpi_gatherv(Int_State%sea_level_ice_mask_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - sea_level_ice_mask_im",rcfinal)
     END DO

! Distribute the global sea level ice mask array into the GFS
! internal structure array SLMSK which are in the internal state.
!----------------------------------------------------------------
     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SLMSK, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the orography array from the ESMF import state.
! For detailed line by line comments please refer to
! the sea level ice mask code.
!----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%orography_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'OROG', Int_State%orography_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - OROG_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%orography_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - orography_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%ORO, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the skin temperature array from the ESMF import state.
! For detailed line by line comments please refer to
! the sea level ice mask code.
!-----------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%t_skin_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'TSEA', Int_State%t_skin_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - TSEA_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%t_skin_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - t_skin_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%TSEA, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the snow depth array from the ESMF import state.
!-----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%snow_depth_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'SHELEG', Int_State%snow_depth_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - SHELEG_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%snow_depth_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - snow_depth_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SHELEG, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the deep soil temperature array from the ESMF import state.
!----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%deep_soil_t_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'TG3', Int_State%deep_soil_t_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - TG3_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%deep_soil_t_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - deep_soil_t_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%TG3, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the surface roughness data array from the ESMF import state.
!-----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%roughness_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'ZORL', Int_State%roughness_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - ZORL_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%roughness_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - roughness_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%ZORL, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the albedo visible scattered array from the ESMF import state.
!-------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%albedo_visible_scattered_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'ALVSF', Int_State%albedo_visible_scattered_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - ALVSF_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%albedo_visible_scattered_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - albedo_visible_scattered_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%ALVSF, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the albedo visible beam array from the ESMF import state.
!--------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%albedo_visible_beam_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'ALVWF', Int_State%albedo_visible_beam_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - ALVWF_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%albedo_visible_beam_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - albedo_visible_beam_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%ALVWF, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the albedo IR scattered array from the ESMF import state.
!--------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%albedo_nearIR_scattered_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'ALNSF', Int_State%albedo_nearIR_scattered_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - ALNSF_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%albedo_nearIR_scattered_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - albedo_nearIR_scattered_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%ALNSF, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the albedo IR beam array from the ESMF import state.
!---------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%albedo_nearIR_beam_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'ALNWF', Int_State%albedo_nearIR_beam_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - ALNWF_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%albedo_nearIR_beam_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - albedo_nearIR_beam_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%ALNWF, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the vegetation cover data array from the ESMF import state.
!----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vegetation_cover_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'VFRAC', Int_State%vegetation_cover_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - VFRAC_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%vegetation_cover_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - vegetation_cover_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%VFRAC, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the canopy water data array from the ESMF import state.
!------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%canopy_water_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'CANOPY', Int_State%canopy_water_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - CANOPY_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%canopy_water_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - canopy_water_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%CANOPY, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the 10 meter wind fraction data array from the ESMF import state.
!----------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%m10_wind_fraction_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'F10M', Int_State%m10_wind_fraction_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - F10M_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%m10_wind_fraction_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - m10_wind_fraction_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%F10M, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the vegetation type data array from the ESMF import state.
!---------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vegetation_type_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'VTYPE', Int_State%vegetation_type_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - VTYPE_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%vegetation_type_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - vegetation_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%VTYPE, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the soil type data array from the ESMF import state.
!---------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%soil_type_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'STYPE', Int_State%soil_type_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - STYPE_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%soil_type_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - soil_type_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%STYPE, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the zeneith angle facsf data array from the ESMF import state.
!-------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%zeneith_angle_facsf_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'FACSF', Int_State%zeneith_angle_facsf_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - FACSF_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%zeneith_angle_facsf_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - zeneith_angle_facsf_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%FACSF, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the zeneith angle facwf data array from the ESMF import state.
!-------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%zeneith_angle_facwf_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'FACWF', Int_State%zeneith_angle_facwf_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - FACWF_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%zeneith_angle_facwf_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - zeneith_angle_facwf_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%FACWF, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the uustar data array from the ESMF import state.
!------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%uustar_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'UUSTAR', Int_State%uustar_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - UUSTAR_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%uustar_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - uustar_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%UUSTAR, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the ffmm data array from the ESMF import state.
!----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%ffmm_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'FFMM', Int_State%ffmm_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - FFMM_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%ffmm_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - ffmm_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%FFMM, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the ffhh data array from the ESMF import state.
!----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%ffhh_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'FFHH', Int_State%ffhh_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - FFHH_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%ffhh_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - ffhh_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%FFHH, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the sea ice thickness data array from the ESMF import state.
!-----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%sea_ice_thickness_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'SIH', Int_State%sea_ice_thickness_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - SIH_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%sea_ice_thickness_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - sea_ice_thickness_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%HICE, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the sea ice concentration data array from the ESMF import state.
!---------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%sea_ice_concentration_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'SIC', Int_State%sea_ice_concentration_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - SIC_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%sea_ice_concentration_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - sea_ice_concentration_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%FICE, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the TpRCp data array from the ESMF import state.
!-----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%tprcp_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'TPRCP', Int_State%tprcp_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - TPRCP_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%tprcp_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - tprcp_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%TPRCP, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the srflag data array from the ESMF import state.
!------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%srflag_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'SRFLAG', Int_State%srflag_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - SRFLAG_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%srflag_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - srflag_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SRFLAG, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the actual snow depth data array from the ESMF import state.
!-----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%actual_snow_depth_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'SNWDPH', Int_State%actual_snow_depth_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - SNWDPH_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%actual_snow_depth_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - actual_snow_depth_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SNWDPH, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the minimum vegetation cover data array from the ESMF import state.
!------------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vegetation_cover_min_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'VMN', Int_State%vegetation_cover_min_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - VMN_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%vegetation_cover_min_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - vegetation_cover_min_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SHDMIN, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the maximum vegetation cover data array from the ESMF import state.
!------------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vegetation_cover_max_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'VMX', Int_State%vegetation_cover_max_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - VMX_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%vegetation_cover_max_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - vegetation_cover_max_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SHDMAX, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the slope type data array from the ESMF import state.
!----------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%slope_type_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'SLP', Int_State%slope_type_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - SLP_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%slope_type_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - slope_type_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SLOPE, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the maximum snow albedo data array from the ESMF import state.
!-------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%snow_albedo_max_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'ABS', Int_State%snow_albedo_max_im, 0, rc = rc1)

 CALL ERR_MSG1(rc1,"Gete ESMF State - ABS_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%snow_albedo_max_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_allgatherv - snow_albedo_max_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%SNOALB, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the soil temperature data array from the ESMF import state.
! It contains multiple level data.
!----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%soil_t_import == 1) THEN
     kmsk = 0

     ALLOCATE(STC_name(Int_State%lsoil), stat = rc1)
     CALL ERR_MSG1(rc1, " - Allocate the working array - STC_name", rcfinal)

     DO i = 1, Int_State%lsoil
         WRITE(STC_name(i), 2004) i
     END DO

     CALL GetF90ArrayFromState(impGFS, STC_name(1), Int_State%soil_t_im1, 0, rc = rc1)
     CALL GetF90ArrayFromState(impGFS, STC_name(2), Int_State%soil_t_im2, 0, rc = rc1)
     CALL GetF90ArrayFromState(impGFS, STC_name(3), Int_State%soil_t_im3, 0, rc = rc1)
     DEALLOCATE(STC_name)

     CALL ERR_MSG1(rc1,"Get ESMF State - STC_im",rcfinal)

     DO j = 1, latr
         CALL mpi_gatherv(Int_State%soil_t_im1(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf11(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)
         CALL mpi_gatherv(Int_State%soil_t_im2(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf12(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)
         CALL mpi_gatherv(Int_State%soil_t_im3(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf13(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - soil_t_im",rcfinal)
     END DO

     CALL split2d(buf11, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2, Int_State%global_lats_r, Int_State%lonsperlar)

     CALL split2d(buf12, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2(1, 1, 2), Int_State%global_lats_r, Int_State%lonsperlar)

     CALL split2d(buf13, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2(1, 1, 3), Int_State%global_lats_r, Int_State%lonsperlar)

     DO i = 1, 3
         DO i2 = 1, lats_node_r
             DO i1 = 1, lonr
                 Int_State%sfc_fld%STC(i, i1, i2) = buff2(i1, i2, i)
             END DO
         END DO
     END DO

2004 FORMAT('STC_', i1)
 END IF

 IF(Int_State%ESMF_Sta_List%soil_mois_import == 1) THEN
     kmsk = 0

     ALLOCATE(SMC_name(Int_State%lsoil),    stat = rc1)

     CALL ERR_MSG1(rc1," - Allocate the working array - SMC_name",rcfinal)

     DO i = 1, Int_State%lsoil
         WRITE(SMC_name(i), 2005) i
     END DO

     CALL GetF90ArrayFromState(impGFS, SMC_name(1), Int_State%soil_mois_im1, 0, rc = rc1)
     CALL GetF90ArrayFromState(impGFS, SMC_name(2), Int_State%soil_mois_im2, 0, rc = rc1)
     CALL GetF90ArrayFromState(impGFS, SMC_name(3), Int_State%soil_mois_im3, 0, rc = rc1)
     DEALLOCATE(SMC_name)

     CALL ERR_MSG1(rc1,"Get ESMF State - SMC_im",rcfinal)

     DO j = 1, latr
         CALL mpi_gatherv(Int_State%soil_mois_im1(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf11(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)
         CALL mpi_gatherv(Int_State%soil_mois_im2(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf12(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)
         CALL mpi_gatherv(Int_State%soil_mois_im3(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf13(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - soil_mois_im",rcfinal)
     END DO

     CALL split2d(buf11,  buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2, Int_State%global_lats_r, Int_State%lonsperlar)

     CALL split2d(buf12, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2(1, 1, 2), Int_State%global_lats_r, Int_State%lonsperlar)

     CALL split2d(buf13, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2(1, 1, 3), Int_State%global_lats_r, Int_State%lonsperlar)

     DO i = 1, 3
         DO i2 = 1, lats_node_r
             DO i1 = 1, lonr
                 Int_State%sfc_fld%SMC(i, i1, i2) = buff2(i1, i2, i)
             END DO
         END DO
     END DO

2005 FORMAT('SMC_', i1)
 END IF

 IF(Int_State%ESMF_Sta_List%liquid_soil_moisture_import == 1) THEN
     kmsk = 0

     ALLOCATE(SLC_name(Int_State%lsoil),    stat = rc1)

     CALL ERR_MSG1(rc1," - Allocate the working array - SLC_name",rcfinal)

     DO i = 1, Int_State%lsoil
         WRITE(SLC_name(i), 2003) i
     END DO

     CALL GetF90ArrayFromState(impGFS, SLC_name(1), Int_State%liquid_soil_moisture_im1, 0, rc = rc1)
     CALL GetF90ArrayFromState(impGFS, SLC_name(2), Int_State%liquid_soil_moisture_im2, 0, rc = rc1)
     CALL GetF90ArrayFromState(impGFS, SLC_name(3), Int_State%liquid_soil_moisture_im3, 0, rc = rc1)
     DEALLOCATE(SLC_name)

     CALL ERR_MSG1(rc1,"Get ESMF State - SLC_im",rcfinal)

     DO j = 1, latr
         CALL mpi_gatherv(Int_State%liquid_soil_moisture_im1(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf11(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)
         CALL mpi_gatherv(Int_State%liquid_soil_moisture_im2(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf12(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)
         CALL mpi_gatherv(Int_State%liquid_soil_moisture_im3(1, j), ijn_g(mm1, 2),  MPI_R_IO, buf13(1, j), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - liquid_soil_moisture_im",rcfinal)
     END DO

     CALL split2d(buf11,  buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2, Int_State%global_lats_r, Int_State%lonsperlar)

     CALL split2d(buf12, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2(1, 1, 2), Int_State%global_lats_r, Int_State%lonsperlar)

     CALL split2d(buf13, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, buff2(1, 1, 3), Int_State%global_lats_r, Int_State%lonsperlar)

     DO i = 1, 3
         DO i2 = 1, lats_node_r
             DO i1 = 1, lonr
                 Int_State%sfc_fld%SLC(i, i1, i2) = buff2(i1, i2, i)
             END DO
         END DO
     END DO

2003 FORMAT('SLC_', i1)
 END IF

! Get the convective cloud cover data array from the ESMF import state.
!----------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%conv_cloud_cover_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'CV', Int_State%conv_cloud_cover_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Gete ESMF State - CV_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%conv_cloud_cover_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - conv_cloud_cover_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%CV, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the convective cloud base data array from the ESMF import state.
!---------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%conv_cloud_base_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'CVB', Int_State%conv_cloud_base_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Gete ESMF State - CVB_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%conv_cloud_base_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - conv_cloud_base_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%CVB, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Get the convective cloud top data array from the ESMF import state.
!--------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%conv_cloud_top_import == 1) THEN
     kmsk = 0
     CALL GetF90ArrayFromState(impGFS, 'CVT', Int_State%conv_cloud_top_im, 0, rc = rc1)

     CALL ERR_MSG1(rc1,"Gete ESMF State - CVT_im",rcfinal)
     DO i = 1, latr
         CALL mpi_gatherv(Int_State%conv_cloud_top_im(1, i), ijn_g(mm1, 2),  MPI_R_IO, buf(1, i), &
                          ijn_g(1, 2), displs_g(1, 2), MPI_R_IO, Int_State%nodes-1, mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_allgatherv - conv_cloud_top_im",rcfinal)
     END DO

     CALL split2d(buf, buffo, Int_State%global_lats_r)
     CALL interpred(1, kmsk, buffo, Int_State%sfc_fld%CVT, Int_State%global_lats_r, Int_State%lonsperlar)
 END IF

! Print out the final error signal message and put it to rc.
!-----------------------------------------------------------
 IF(rcfinal == ESMF_SUCCESS) THEN
     PRINT*, "PASS: GFS_ESMFImportState2InternalStateMod.F90"
 ELSE
     PRINT*, "FAIL: GFS_ESMFImportState2InternalStateMod.F90"
 END IF

 IF(PRESENT(rc)) THEN
     rc = rcfinal
 END IF

 END SUBROUTINE GFS_ESMFImportState2InternalState





 SUBROUTINE GFS_InternalState2ESMFExportState(gcGFS, expGFS, Int_State, rc)

! This subroutine reads the last written sigma file and surface file and put them 
! into the GFS ESMF export state.  This subroutine will be changed that all export
! ESMF states will get data directly from the GFS internal structure data arrays
! instead of from the sigma file and surface file.

!
!!USES:
!
 USE sfcio_module

!
! !INPUT/OUTPUT VARIABLES AND PARAMETERS:
!----------------------------------------

 TYPE(ESMF_GridComp),              INTENT(inout) :: gcGFS     ! ESMF grid component which contains
                                                              ! the ESMF export state.
 TYPE(ESMF_State),                 INTENT(inout) :: expGFS    ! the ESMF export state.
 TYPE(GFS_InternalState), POINTER, INTENT(inout) :: Int_State ! the internal state which contains the
                                                              ! GFS final output arrays.
 INTEGER, OPTIONAL,                INTENT(out)   :: rc        ! error signal variable.

! Local array size parameter of the ESMF export state arrays.
!------------------------------------------------------------
 INTEGER                                         :: lnt2_s, lonr_s, latr_s

! Working variables and arrays.
!------------------------------
 TYPE(ESMF_Grid)                   :: grid1        ! the ESMF GRID TYPE ARRAY, for the
                                                   ! single level spectral fields.
 TYPE(ESMF_Grid)                   :: grid2        ! the ESMF GRID TYPE ARRAY, for the
                                                   ! multiple levels spectral fields.
 TYPE(ESMF_Grid)                   :: grid3        ! the ESMF GRID TYPE ARRAY, for the
                                                   ! Gaussian grid surface fields.
 TYPE(ESMF_Grid)                   :: grid4        ! the ESMF GRID TYPE ARRAY, for the
                                                   ! start date and time to run the GFS.

 TYPE(ESMF_VM)         :: vm     ! ESMF virtual machine.
 TYPE(sfcio_head)      :: head
 TYPE(sfcio_data)      :: data
 INTEGER               :: rc1     ! error signal work variable.
 INTEGER               :: rcfinal ! the dinal error signal variable.
 INTEGER               :: nosig, nosfc, me, i, j, ii1(2)
 INTEGER               :: i1(2), idate2(4), idate1(5)

 REAL(KIND = kind_io4) :: bfhour
 REAL(KIND = kind_io4) :: buf(Int_State%lnt2)
 REAL(KIND = kind_io4) :: trisca_1(Int_State%lnt2, Int_State%levs)
 REAL(KIND = kind_io4) :: trisca_2(Int_State%lnt2, Int_State%levs)

 CHARACTER(16)         :: CFHOUR

 CHARACTER(5),          DIMENSION(:),    POINTER :: SMC_name, STC_name, SLC_name
 REAL(KIND = kind_io4), DIMENSION(:, :), POINTER :: sfc_t1, sfc_t2, sfc_t3
 REAL(KIND = kind_io4), DIMENSION(:, :), POINTER :: sfc_m1, sfc_m2, sfc_m3
 REAL(KIND = kind_io4), DIMENSION(:, :), POINTER :: sfc_lm1, sfc_lm2, sfc_lm3

 LOGICAL, SAVE :: first
 DATA first /.true./

 SAVE sfc_t1, sfc_t2, sfc_t3, sfc_m1, sfc_m2, sfc_m3, sfc_lm1, sfc_lm2, sfc_lm3

 INTEGER, DIMENSION(Int_State%nodes, 2)          :: ijn_s, displs_s

! Initialize the error signal variables.
!---------------------------------------
 rc1     = ESMF_SUCCESS
 rcfinal = ESMF_SUCCESS

! Get the GFS final forecast time information from the internal state CFOUR1.
!----------------------------------------------------------------------------
  print *,' bef cfhour cfhour1=',int_state%cfhour1
 CFHOUR  = Int_State%CFHOUR1

! Get the task ID number from the internal state "me".
!-----------------------------------------------------
 me      = Int_State%me

! Print out the final forecast time information.
!-----------------------------------------------
 PRINT*, 'in Int_State to expGFS, CFHOUR=', CFHOUR

 CALL ESMF_LogWrite("Begining to Create the ESMF Export State.", &
                    ESMF_LOG_INFO, rc = rc1)

! Getting the global VM for the gathering data purpose
! and the grid3, which is for the Gaussian grid arrays.
!------------------------------------------------------
 CALL ESMF_GridCompGet(gcGFS, vm = vm, grid = grid3, rc = rc1)

 CALL ERR_MSG1(rc1,"Get the GLobal VM and Grid3",rcfinal)

! User code.  Besides grid3, need to create grid1, grid2 and grid4 and
! distribute them into the default ESMF DE layout.
!---------------------------------------------------------------------
 CALL Grid_ESMFCreate2(vm, grid1, grid2, grid4, Int_State, rc1)

 CALL ERR_MSG1(rc1,"Grid_ESMFCreate for Grid1, Grid2 and Grid4",rcfinal)

! Get the local array size, which are for the parallel ESMF interface states,
! from the internal state.  lnt2_s is for the spectral arrays.  lonr_s and
! latr_s are for the longitude and latitude of the Gaussian grid arrays, respectively.
!------------------------------------------------------------------------------------- 
 lnt2_s = Int_State%lnt2_s
 lonr_s = Int_State%lonr_s
 latr_s = Int_State%latr_s

! i1 is the grid4 array size, which is for the start date and time information.
!------------------------------------------------------------------------------
 i1     = Int_State%grid4_i1

! Set up the scattering parameter arrays which will be used to
! scatter the global arrays into the parallel ESMF state local arrays.
! ijn_s(:, 1) and displs_s(:, 1) is usedfor the spectral fields and ijn_s(:, 2)
! and displs_s(:, 2) is used for the Gaussian grid fields.
!-------------------------------------------------------------------------
 displs_s(1, :) = 0
 DO i = 1, Int_State%nodes
     IF(me == i-1) THEN
         ii1(1) = lnt2_s
         ii1(2) = lonr_s
     END IF
     CALL ESMF_VMBroadcast(vm, ii1, 2, i-1, blockingflag = ESMF_BLOCKING, rc = rc1)
     ijn_s(i, 1) = ii1(1)
     ijn_s(i, 2) = ii1(2)

     CALL ERR_MSG1(rc1,"VMBroadcast ijn_s.",rcfinal)

     IF(i /= 1) THEN
         displs_s(i, 1) = displs_s(i-1, 1) + ijn_s(i-1, 1)
         displs_s(i, 2) = displs_s(i-1, 2) + ijn_s(i-1, 2)
     END IF
 END DO

! Open the final outputed sigma file and read the header, and put
! the date and time information into idate1.
!----------------------------------------------------------------
 nosig = 61
 IF(me == 0) THEN 
     OPEN(nosig, FILE = 'SIG.F'//CFHOUR, FORM = 'unformatted')
     REWIND nosig
     READ(nosig)
     READ(nosig) bfhour, idate2
     idate1(1)       = NINT(bfhour)
     DO i = 1, 4
         idate1(i+1) = idate2(i)
     END DO
 END IF
 
! All the ESMF export states have their own turn-on/turn-off switch flag
! to flexible fit the different requirement from different outside ESMF
! grid components.
!-----------------------------------------------------------------------

! Put the GFS run date and time information into the ESMF export state
! for other ESMF grid component use.
! idate1_im and idate1_ex:  (1) --- bfhour (integer), (2) - (5) --- idate.
!-------------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%idate1_export == 1) THEN

! Allocate the date and time fortran array which is in the internal state
! and will be used to create the corresponding ESMF export state.
!-------------------------------------------------------------------------
     ALLOCATE(Int_State%idate1_ex(i1(1), i1(2)), stat = rc1)

     CALL ERR_MSG1(rc1," - Allocate the Internal State - idate1_ex",rcfinal)

! Use the ESMF broadcast communication routine to broadcast the date and time
! information which was gotten from the header of the sigma file.
!----------------------------------------------------------------------------
     CALL ESMF_VMBroadcast(vm, idate1, 5, 0, blockingflag = ESMF_BLOCKING, rc = rc1)

     CALL ERR_MSG1(rc1,"VMBroadcast idate1_ex.",rcfinal)

! Put the date and time information into the date and time array in the
! internal state, and print it out.
!----------------------------------------------------------------------
     Int_State%idate1_ex(1, :) = idate1
     PRINT*, 'in Int to expGFS, idate1_ex = ', Int_State%idate1_ex

! Use the "AddF90ArrayToState" which is a user created software to add
! the fortran array into the ESMF export state. The first argument is 
! ESMF state which will be added a fortran array.  The second is the
! ESMF grid which will be used for the added fortran array in the ESMF
! state.  The third is the user defined name for this array.  The fourth
! id the added fortran array and the last one is the error signal variable.
!--------------------------------------------------------------------------
     CALL AddF90ArrayToState(expGFS, grid4, 'DATE', &
         Int_State%idate1_ex, rc = rc1)

     CALL ERR_MSG1(rc1,"Create ESMF State - DATE_ex",rcfinal)

 END IF

! read the first spectral field which is the surface orography field.
!--------------------------------------------------------------------
 IF(me == 0) THEN 
     READ(nosig) buf
 END IF

 IF(Int_State%ESMF_Sta_List%z_export == 1) THEN

! Allocate the local fortran surface orography array which is used to add 
! the local fortran surface orography array into the parallel ESMF interface state.
!----------------------------------------------------------------------------------
     ALLOCATE(Int_State%z_ex(lnt2_s, 1), stat = rc1)

     CALL ERR_MSG1(rc1," - Allocate the Internal State - z_ex",rcfinal)

! Scatter the global surface orography array which is gotten from the sigma
! file into the local fortran array which will be added into the ESMF state.
!---------------------------------------------------------------------------
     CALL mpi_scatterv(buf, ijn_s, displs_s,             &
          MPI_R_IO, Int_State%z_ex, lnt2_s, MPI_R_IO, 0, &
          mpi_comm_all, rc1)

     CALL ERR_MSG1(rc1,"MPI_Scatterv - z_ex",rcfinal)

! Use the "AddF90ArrayToState" which is a user created software to add
! the local surface orography fortran array into the ESMF export state.
!----------------------------------------------------------------------
     CALL AddF90ArrayToState(expGFS, grid1, 'HS', &
          Int_State%z_ex, rc = rc1)

     CALL ERR_MSG1(rc1,"Create ESMF State - HS_ex",rcfinal)

 END IF

! continue to read the second field from the sigma file which is
! the surface pressure filed.
!---------------------------------------------------------------
 IF(me == 0) THEN 
     READ(nosig) buf
 END IF

! Add the surface pressure field into the ESMF export state.  For
! the detailed description comments please refer the surface
! orography field.
!----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%ps_export == 1) THEN
     ALLOCATE(Int_State%ps_ex(lnt2_s, 1), stat = rc1)

     CALL ERR_MSG1(rc1," - Allocate the Internal State - ps_ex",rcfinal)

     CALL mpi_scatterv(buf, ijn_s, displs_s,              &
          MPI_R_IO, Int_State%ps_ex, lnt2_s, MPI_R_IO, 0, &
          mpi_comm_all, rc1)

     CALL ERR_MSG1(rc1,"MPI_Scatterv - ps_ex",rcfinal)

     CALL AddF90ArrayToState(expGFS, grid1, 'PS', &
         Int_State%ps_ex, rc = rc1)

     CALL ERR_MSG1(rc1,"Create ESMF State - PS_ex",rcfinal)

 END IF

! read the temperature fileds from the sigma file
! and put it into trisca_1.
!------------------------------------------------
 DO i = 1, Int_State%levs
     IF(me == 0) THEN 
         READ(nosig) buf
         trisca_1(:, i) = buf
     END IF
 END DO

! Add the temperature fileds into the ESMF export state.
!-------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%temp_export == 1) THEN

! Allocate the local multiple level temperature array which will be
! added into the parallel ESMF interface export state.
!------------------------------------------------------------------
     ALLOCATE(Int_State%temp_ex(lnt2_s, Int_State%levs), stat = rc1)

     CALL ERR_MSG1(rc1," - Allocate the Internal State - temp_ex",rcfinal)

! Do loop over the vertical levels to scatter the global temperature fields
! into the local temperature fortran array.
!--------------------------------------------------------------------------
     DO i = 1, Int_State%levs
         CALL mpi_scatterv(trisca_1(1, i), ijn_s, displs_s,              &
              MPI_R_IO, Int_State%temp_ex(1, i), lnt2_s, MPI_R_IO, 0,    &
              mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_Scatterv - temp_ex",rcfinal)
     END DO

! Added the multiple level local fortran temperature array into
! the ESMF export state.
!--------------------------------------------------------------
     CALL AddF90ArrayToState(expGFS, grid2, 'T', &
         Int_State%temp_ex, rc = rc1)

     CALL ERR_MSG1(rc1,"Create ESMF State - T_ex",rcfinal)

 END IF

! read the divergence field and the vorticity field and put them
! into the multiple level arrays trisca_1 and trisca_2, respectively.
!--------------------------------------------------------------------
 DO i = 1, Int_State%levs
     IF(me == 0) THEN 
         READ(nosig) buf
         trisca_1(:, i) = buf
         READ(nosig) buf
         trisca_2(:, i) = buf
     END IF
 END DO

! To add the divergence field into the ESMF export state.  For the detailed
! description comments please refer to the temperature field.
!--------------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%div_export == 1) THEN
     ALLOCATE(Int_State%div_ex(lnt2_s, Int_State%levs), stat = rc1)

     CALL ERR_MSG1(rc1," - Allocate the Internal State - div_ex",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_scatterv(trisca_1(1, i), ijn_s, displs_s,              &
              MPI_R_IO, Int_State%div_ex(1, i), lnt2_s, MPI_R_IO, 0,     &
              mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_Scatterv - div_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid2, 'D', &
         Int_State%div_ex, rc = rc1)

     CALL ERR_MSG1(rc1,"Create ESMF State - D_ex",rcfinal)

 END IF

! Add the vorticity field into the ESMF export state.
!----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vor_export == 1) THEN
     ALLOCATE(Int_State%vor_ex(lnt2_s, Int_State%levs), stat = rc1)

     CALL ERR_MSG1(rc1," - Allocate the Internal State - vor_ex",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_scatterv(trisca_2(1, i), ijn_s, displs_s,              &
              MPI_R_IO, Int_State%vor_ex(1, i), lnt2_s, MPI_R_IO, 0,     &
              mpi_comm_all, rc1)

         CALL ERR_MSG1(rc1,"MPI_Scatterv - vor_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid2, 'Z', &
         Int_State%vor_ex, rc = rc1)

     CALL ERR_MSG1(rc1,"Create ESMF State - Z_ex",rcfinal)

 END IF

! read the moisture field from the sigma file.
!---------------------------------------------
 DO i = 1, Int_State%levs
     IF(me == 0) THEN 
         READ(nosig) buf
         trisca_1(:, i) = buf
     END IF
 END DO

! Add the moisture field into the ESMF export state.
!---------------------------------------------------
 IF(Int_State%ESMF_Sta_List%q_export == 1) THEN
     ALLOCATE(Int_State%q_ex(lnt2_s, Int_State%levs), stat = rc1)

     CALL ERR_MSG1(rc1," - Allocate the Internal State - q_ex",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_scatterv(trisca_1(1, i), ijn_s, displs_s,              &
              MPI_R_IO, Int_State%q_ex(1, i), lnt2_s, MPI_R_IO, 0,       &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - q_ex",rcfinal)
     END DO
     CALL AddF90ArrayToState(expGFS, grid2, 'SHUM', &
         Int_State%q_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - SHUM_ex",rcfinal)

 END IF

! read the ozone field from the sigma file.
!------------------------------------------
 DO i = 1, Int_State%levs
     IF(me == 0) THEN 
         READ(nosig) buf
         trisca_1(:, i) = buf
     END IF
 END DO

! Add the ozone field into the ESMF export state.
!------------------------------------------------
 IF(Int_State%ESMF_Sta_List%oz_export == 1) THEN
     ALLOCATE(Int_State%oz_ex(lnt2_s, Int_State%levs), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - oz_ex",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_scatterv(trisca_1(1, i), ijn_s, displs_s,              &
              MPI_R_IO, Int_State%oz_ex(1, i), lnt2_s, MPI_R_IO, 0,      &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - oz_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid2, 'SOZ', &
         Int_State%oz_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - SOZ_ex",rcfinal)

 END IF

! read the cloud liquid water field from the sigma file.
!-------------------------------------------------------
 IF(me == 0) THEN 
     DO i = 1, Int_State%levs
         READ(nosig) buf
         trisca_1(:, i) = buf
     END DO
     CLOSE (nosig)
 END IF

! Add the cloud liquid water field into the ESMF export state.
!-------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%scld_export == 1) THEN
     ALLOCATE(Int_State%scld_ex(lnt2_s, Int_State%levs), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - scld_ex",rcfinal)

     DO i = 1, Int_State%levs
         CALL mpi_scatterv(trisca_1(1, i), ijn_s, displs_s,              &
              MPI_R_IO, Int_State%scld_ex(1, i), lnt2_s, MPI_R_IO, 0,    &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - scld_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid2, 'SCLD', &
         Int_State%scld_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - SCLD_ex",rcfinal)

 END IF

! Open the final outputed surface file and read in all header
! and all surface fields and put them into "head" and "data".
!------------------------------------------------------------
 nosfc = 61
 IF(me == 0) THEN 
     OPEN(nosfc, FILE = 'SFC.F'//CFHOUR, FORM = 'unformatted')

     CALL sfcio_srhead(nosfc, head, rc1)

 CALL ERR_MSG1(rc1," CALL sfcio_srhead",rcfinal)

     CALL sfcio_aldata(head, data, rc1)

 CALL ERR_MSG1(rc1," CALL sfcio_aldata",rcfinal)

     CALL sfcio_srdata(nosfc, head, data, rc1)

 CALL ERR_MSG1(rc1," CALL sfcio_srdata",rcfinal)

     CLOSE(nosfc)
 END IF

! To add the sea level ice mask data into the ESMF export state.
!---------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%sea_level_ice_mask_export == 1) THEN

! Allocate the local sea level ice mask data array with the local size parameters
! lonr_s and latr_s.  This array will be added into the ESMF parallel interface
! export state.
!--------------------------------------------------------------------------------
     ALLOCATE(Int_State%sea_level_ice_mask_ex(lonr_s, latr_s), stat = rc1)
 CALL ERR_MSG1(rc1," - Allocate the Internal State - sea_level_ice_mask_ex",rcfinal)

! Do loop over the latitude to scatter the global array longitude data into the local
! fortran array.
!------------------------------------------------------------------------------------
     DO i = 1, latr_s
         CALL mpi_scatterv(data%slmsk(1, i), ijn_s(1, 2), displs_s(1, 2),           &
              MPI_R_IO, Int_State%sea_level_ice_mask_ex(1, i), lonr_s, MPI_R_IO, 0, &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - sea_level_ice_mask_ex",rcfinal)
     END DO

! Use user create software "AddF90ArrayToState" to add the sea level ice 
! mask local fortran array into the ESMF export state.
!-----------------------------------------------------------------------
     CALL AddF90ArrayToState(expGFS, grid3, 'SLMSK', &
         Int_State%sea_level_ice_mask_ex, rc = rc1)
 CALL ERR_MSG1(rc1,"Create ESMF State - SLMSK_ex",rcfinal)
 END IF

! Add the surface orography data into the ESMF export state.  For
! detailed destription comments please refer the sea level ice mask field.
!-------------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%orography_export == 1) THEN
     ALLOCATE(Int_State%orography_ex(lonr_s, latr_s), stat = rc1)
 CALL ERR_MSG1(rc1," - Allocate the Internal State - orography_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%orog(1, i), ijn_s(1, 2), displs_s(1, 2),              &
              MPI_R_IO, Int_State%orography_ex(1, i), lonr_s, MPI_R_IO, 0,            &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - orography_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'OROG', Int_State%orography_ex, rc = rc1)
 CALL ERR_MSG1(rc1,"Create ESMF State - OROG_ex",rcfinal)
 END IF

! Add the skin temperature data into the ESMF export state.
!----------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%t_skin_export == 1) THEN
     ALLOCATE(Int_State%t_skin_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - t_skin_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%tsea(1, i), ijn_s(1, 2), displs_s(1, 2),       &
              MPI_R_IO, Int_State%t_skin_ex(1, i), lonr_s, MPI_R_IO, 0,        &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - t_skin_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'TSEA', &
         Int_State%t_skin_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - TSEA_ex",rcfinal)
 END IF

! Add the snow depth data into the ESMF export state.
!----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%snow_depth_export == 1) THEN
     ALLOCATE(Int_State%snow_depth_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - snow_depth_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%sheleg(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%snow_depth_ex(1, i), lonr_s, MPI_R_IO, 0,           &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - snow_depth_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'SHELEG', &
         Int_State%snow_depth_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - SHELEG_ex",rcfinal)
 END IF

! Add the deep soil temperature data into the ESMF export state.
!---------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%deep_soil_t_export == 1) THEN
     ALLOCATE(Int_State%deep_soil_t_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - deep_soil_t_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%tg3(1, i), ijn_s(1, 2), displs_s(1, 2),               &
              MPI_R_IO, Int_State%deep_soil_t_ex(1, i), lonr_s, MPI_R_IO, 0,          &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - deep_soil_t_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'TG3', &
         Int_State%deep_soil_t_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - TG3_ex",rcfinal)
 END IF

! Add the surface roughness data into the ESMF export state.
!-----------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%roughness_export == 1) THEN
     ALLOCATE(Int_State%roughness_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - roughness_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%zorl(1, i), ijn_s(1, 2), displs_s(1, 2),              &
              MPI_R_IO, Int_State%roughness_ex(1, i), lonr_s, MPI_R_IO, 0,            &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - roughness_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'ZORL', &
         Int_State%roughness_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - ZORL_ex",rcfinal)
 END IF

! Add the albedo visible scattered data into the ESMF export state.
!------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%albedo_visible_scattered_export == 1) THEN
     ALLOCATE(Int_State%albedo_visible_scattered_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - albedo_visible_scattered_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%alvsf(1, i), ijn_s(1, 2), displs_s(1, 2),                 &
              MPI_R_IO, Int_State%albedo_visible_scattered_ex(1, i), lonr_s, MPI_R_IO, 0, &
              mpi_comm_all, rc1)
    
 CALL ERR_MSG1(rc1,"MPI_Scatterv - albedo_visible_scattered_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'ALVSF', &
         Int_State%albedo_visible_scattered_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - ALVSF_ex",rcfinal)
 END IF

! Add the albedo visible beam data into the ESMF export state.
!-------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%albedo_visible_beam_export == 1) THEN
     ALLOCATE(Int_State%albedo_visible_beam_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - albedo_visible_beam_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%alvwf(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%albedo_visible_beam_ex(1, i), lonr_s, MPI_R_IO, 0,  &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - albedo_visible_beam_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'ALVWF', &
         Int_State%albedo_visible_beam_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - ALVWF_ex",rcfinal)
 END IF

! Add the albedo IR scattered data into the ESMF export state.
!-------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%albedo_nearIR_scattered_export == 1) THEN
     ALLOCATE(Int_State%albedo_nearIR_scattered_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - albedo_nearIR_scattered_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%alnsf(1, i), ijn_s(1, 2), displs_s(1, 2),                &
              MPI_R_IO, Int_State%albedo_nearIR_scattered_ex(1, i), lonr_s, MPI_R_IO, 0, &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - albedo_nearIR_scattered_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'ALNSF', &
         Int_State%albedo_nearIR_scattered_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - ALNSF_ex",rcfinal)
 END IF

! Add the albedo IR beam data into the ESMF export state.
!--------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%albedo_nearIR_beam_export == 1) THEN
     ALLOCATE(Int_State%albedo_nearIR_beam_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - albedo_nearIR_beam_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%alnwf(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%albedo_nearIR_beam_ex(1, i), lonr_s, MPI_R_IO, 0,   &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - albedo_nearIR_beam_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'ALNWF', &
         Int_State%albedo_nearIR_beam_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - ALNWF_ex",rcfinal)
 END IF

! Add the vegetation cover data into the ESMF export state.
!----------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vegetation_cover_export == 1) THEN
     ALLOCATE(Int_State%vegetation_cover_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - vegetation_cover_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%vfrac(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%vegetation_cover_ex(1, i), lonr_s, MPI_R_IO, 0,     &
              mpi_comm_all, rc1)
    
 CALL ERR_MSG1(rc1,"MPI_Scatterv - vegetation_cover_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'VFRAC', Int_State%vegetation_cover_ex, rc = rc1)
 CALL ERR_MSG1(rc1,"Create ESMF State - VFRAC_ex",rcfinal)
 END IF

! Add the canopy water data into the ESMF export state.
!------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%canopy_water_export == 1) THEN
     ALLOCATE(Int_State%canopy_water_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - canopy_water_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%canopy(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%canopy_water_ex(1, i), lonr_s, MPI_R_IO, 0,         &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - canopy_water_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'CANOPY', &
         Int_State%canopy_water_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - CANOPY_ex",rcfinal)
 END IF

! Add the 10 meter wind fraction data into the ESMF export state.
!----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%m10_wind_fraction_export == 1) THEN
     ALLOCATE(Int_State%m10_wind_fraction_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - m10_wind_fraction_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%f10m(1, i), ijn_s(1, 2), displs_s(1, 2),              &
              MPI_R_IO, Int_State%m10_wind_fraction_ex(1, i), lonr_s, MPI_R_IO, 0,    &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - m10_wind_fraction_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'F10M', &
         Int_State%m10_wind_fraction_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - F10M_ex",rcfinal)
 END IF

! Add the vegetation type data into the ESMF export state.
!---------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vegetation_type_export == 1) THEN
     ALLOCATE(Int_State%vegetation_type_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - vegetation_type_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%vtype(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%vegetation_type_ex(1, i), lonr_s, MPI_R_IO, 0,      &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - vegetation_type_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'VTYPE', &
         Int_State%vegetation_type_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - VTYPE_ex",rcfinal)
 END IF

! Add the soil type data into the ESMF export state.
!---------------------------------------------------
 IF(Int_State%ESMF_Sta_List%soil_type_export == 1) THEN
     ALLOCATE(Int_State%soil_type_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - soil_type_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%stype(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%soil_type_ex(1, i), lonr_s, MPI_R_IO, 0,            &
              mpi_comm_all, rc1)
    
 CALL ERR_MSG1(rc1,"MPI_Scatterv - soil_type_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'STYPE', &
         Int_State%soil_type_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - STYPE_ex",rcfinal)
 END IF

! Add the zeneith angle facsf data into the ESMF export state.
!-------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%zeneith_angle_facsf_export == 1) THEN
     ALLOCATE(Int_State%zeneith_angle_facsf_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - zeneith_angle_facsf_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%facsf(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%zeneith_angle_facsf_ex(1, i), lonr_s, MPI_R_IO, 0,  &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - zeneith_angle_facsf_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'FACSF', &
         Int_State%zeneith_angle_facsf_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - FACSF_ex",rcfinal)
 END IF

! Add the zeneith angle facwf data into the ESMF export state.
!-------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%zeneith_angle_facwf_export == 1) THEN
     ALLOCATE(Int_State%zeneith_angle_facwf_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - zeneith_angle_facwf_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%facwf(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%zeneith_angle_facwf_ex(1, i), lonr_s, MPI_R_IO, 0,  &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - zeneith_angle_facwf_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'FACWF', &
         Int_State%zeneith_angle_facwf_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - FACWF_ex",rcfinal)
 END IF

! Add the Uustar data into the ESMF export state.
!------------------------------------------------
 IF(Int_State%ESMF_Sta_List%uustar_export == 1) THEN
     ALLOCATE(Int_State%uustar_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - uustar_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%uustar(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%uustar_ex(1, i), lonr_s, MPI_R_IO, 0,               &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - uustar_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'UUSTAR', Int_State%uustar_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - UUSTAR_ex",rcfinal)
 END IF

! Add the ffmm data into the ESMF export state.
!----------------------------------------------
 IF(Int_State%ESMF_Sta_List%ffmm_export == 1) THEN
     ALLOCATE(Int_State%ffmm_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - ffmm_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%ffmm(1, i), ijn_s(1, 2), displs_s(1, 2),              &
              MPI_R_IO, Int_State%ffmm_ex(1, i), lonr_s, MPI_R_IO, 0,                 &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - ffmm_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'FFMM', Int_State%ffmm_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - FFMM_ex",rcfinal)
 END IF

! Add the ffhh data into the ESMF export state.
!----------------------------------------------
 IF(Int_State%ESMF_Sta_List%ffhh_export == 1) THEN
     ALLOCATE(Int_State%ffhh_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - ffhh_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%ffhh(1, i), ijn_s(1, 2), displs_s(1, 2),              &
              MPI_R_IO, Int_State%ffhh_ex(1, i), lonr_s, MPI_R_IO, 0,                 &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - ffhh_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'FFHH', Int_State%ffhh_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - FFHH_ex",rcfinal)
 END IF

! Add the sea ice thickness data into the ESMF export state.
!-----------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%sea_ice_thickness_export == 1) THEN
     ALLOCATE(Int_State%sea_ice_thickness_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - sea_ice_thickness_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%hice(1, i), ijn_s(1, 2), displs_s(1, 2),              &
              MPI_R_IO, Int_State%sea_ice_thickness_ex(1, i), lonr_s, MPI_R_IO, 0,    &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - sea_ice_thickness_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'SIH',                          &
                             Int_State%sea_ice_thickness_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - SIH_ex",rcfinal)
 END IF

! Add the sea ice concentration data into the ESMF export state.
!---------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%sea_ice_concentration_export == 1) THEN
     ALLOCATE(Int_State%sea_ice_concentration_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - sea_ice_concentration_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%fice(1, i), ijn_s(1, 2), displs_s(1, 2),              &
              MPI_R_IO, Int_State%sea_ice_concentration_ex(1, i), lonr_s, MPI_R_IO, 0,&
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - sea_ice_concentration_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'SIC',                          &
                             Int_State%sea_ice_concentration_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - SIC_ex",rcfinal)
 END IF

! Add the TpRCp data into the ESMF export state.
!-----------------------------------------------
 IF(Int_State%ESMF_Sta_List%tprcp_export == 1) THEN
     ALLOCATE(Int_State%tprcp_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - tprcp_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%tprcp(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%tprcp_ex(1, i), lonr_s, MPI_R_IO, 0,                &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - tprcp_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'TPRCP', Int_State%tprcp_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - TPRCP_ex",rcfinal)
 END IF

! Add the srflag data into the ESMF export state.
!------------------------------------------------
 IF(Int_State%ESMF_Sta_List%srflag_export == 1) THEN
     ALLOCATE(Int_State%srflag_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - srflag_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%srflag(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%srflag_ex(1, i), lonr_s, MPI_R_IO, 0,               &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - srflag_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'SRFLAG', Int_State%srflag_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - SRFLAG_ex",rcfinal)
 END IF

! Add the actual snow depth data into the ESMF export state.
!-----------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%actual_snow_depth_export == 1) THEN
     ALLOCATE(Int_State%actual_snow_depth_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - actual_snow_depth_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%snwdph(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%actual_snow_depth_ex(1, i), lonr_s, MPI_R_IO, 0,    &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - actual_snow_depth_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'SNWDPH', Int_State%actual_snow_depth_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - SNWDPH_ex",rcfinal)
 END IF

! Add the minimum vegetation cover data into the ESMF export state.
!------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vegetation_cover_min_export == 1) THEN
     ALLOCATE(Int_State%vegetation_cover_min_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - vegetation_cover_min_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%shdmin(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%vegetation_cover_min_ex(1, i), lonr_s, MPI_R_IO, 0, &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - vegetation_cover_min_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'VMN',                          &
                             Int_State%vegetation_cover_min_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - VMN_ex",rcfinal)
 END IF

! Add the maximum vegetation cover data into the ESMF export state.
!------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%vegetation_cover_max_export == 1) THEN
     ALLOCATE(Int_State%vegetation_cover_max_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - vegetation_cover_max_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%shdmax(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%vegetation_cover_max_ex(1, i), lonr_s, MPI_R_IO, 0, &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - vegetation_cover_max_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'VMX',                          &
                             Int_State%vegetation_cover_max_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - VMX_ex",rcfinal)
 END IF

! Add the slope type data into the ESMF export state.
!----------------------------------------------------
 IF(Int_State%ESMF_Sta_List%slope_type_export == 1) THEN
     ALLOCATE(Int_State%slope_type_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - slope_type_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%slope(1, i), ijn_s(1, 2), displs_s(1, 2),             &
              MPI_R_IO, Int_State%slope_type_ex(1, i), lonr_s, MPI_R_IO, 0,           &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - slope_type_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'SLP', Int_State%slope_type_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - SLP_ex",rcfinal)
 END IF

! Add the maximum snow albedo data into the ESMF export state.
!-------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%snow_albedo_max_export == 1) THEN
     ALLOCATE(Int_State%snow_albedo_max_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - snow_albedo_max_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%snoalb(1, i), ijn_s(1, 2), displs_s(1, 2),            &
              MPI_R_IO, Int_State%snow_albedo_max_ex(1, i), lonr_s, MPI_R_IO, 0,      &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - snow_albedo_max_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'ABS', Int_State%snow_albedo_max_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - ABS_ex",rcfinal)
 END IF

 IF(first) THEN
     ALLOCATE(sfc_t1(lonr_s, latr_s))
     ALLOCATE(sfc_t2(lonr_s, latr_s))
     ALLOCATE(sfc_t3(lonr_s, latr_s))
     ALLOCATE(sfc_m1(lonr_s, latr_s))
     ALLOCATE(sfc_m2(lonr_s, latr_s))
     ALLOCATE(sfc_m3(lonr_s, latr_s))
     ALLOCATE(sfc_lm1(lonr_s, latr_s))
     ALLOCATE(sfc_lm2(lonr_s, latr_s))
     ALLOCATE(sfc_lm3(lonr_s, latr_s))
     first = .false.
 END IF

! Add the multiple level soil temperature data into the ESMF export state.
!-------------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%soil_t_export == 1) THEN

! Allocate the soil temperature ESMF state name array, which will be assigned 
! the user defined names to identify the corresponding ESMF export state arrays.
!------------------------------------------------------------------------------- 
     ALLOCATE(STC_name(Int_State%lsoil),     stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the working array - STC_name",rcfinal)

! Assign the names to STC_name.
!------------------------------
     DO i = 1, Int_State%lsoil
         WRITE(STC_name(i), 1001) i
     END DO
1001 FORMAT ('STC_', i1)

! Allocate the multiple level soil temperature local array.
!----------------------------------------------------------
     ALLOCATE(Int_State%soil_t_ex(lonr_s, latr_s, Int_State%lsoil), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - soil_t_ex",rcfinal)

! Do loop over the vertical levels of the soil temperature field.
!----------------------------------------------------------------
     DO i = 1, Int_State%lsoil

! Scatter every single level global latitude data into the local fortran array.
!------------------------------------------------------------------------------
         DO j = 1, latr_s
             CALL mpi_scatterv(data%stc(1, j, i), ijn_s(1, 2), displs_s(1, 2),      &
                  MPI_R_IO, Int_State%soil_t_ex(1, j, i), lonr_s, MPI_R_IO, 0,      &
                  mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - soil_t_ex",rcfinal)
         END DO
     END DO
     
! Use the work arrays to get the data from the multiple array and be added
! into the ESMF export state.
!-------------------------------------------------------------------------
         sfc_t1 = Int_State%soil_t_ex(:, :, 1)
         sfc_t2 = Int_State%soil_t_ex(:, :, 2)
         sfc_t3 = Int_State%soil_t_ex(:, :, 3)
         CALL AddF90ArrayToState(expGFS, grid3, STC_name(1), sfc_t1, rc = rc1)
         CALL AddF90ArrayToState(expGFS, grid3, STC_name(2), sfc_t2, rc = rc1)
         CALL AddF90ArrayToState(expGFS, grid3, STC_name(3), sfc_t3, rc = rc1)
         
 CALL ERR_MSG1(rc1,"Create ESMF State - soil_t_ex",rcfinal)

! DeAllocate the STC name array.
!-------------------------------
     DEALLOCATE(STC_name)
 END IF

! Add the multiple level soil moisture field into the ESMF export state.
! For detailed description comments please refer the soil temperature code.
!--------------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%soil_mois_export == 1) THEN

     ALLOCATE(SMC_name(Int_State%lsoil),     stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the working array - SMC_name",rcfinal)

     DO i = 1, Int_State%lsoil
         WRITE(SMC_name(i), 1002) i
     END DO
1002 FORMAT('SMC_', i1)

     ALLOCATE(Int_State%soil_mois_ex(lonr_s, latr_s, Int_State%lsoil), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - soil_mois_ex",rcfinal)

     DO i = 1, Int_State%lsoil
         DO j = 1, latr_s
             CALL mpi_scatterv(data%smc(1, j, i), ijn_s(1, 2), displs_s(1, 2),      &
                  MPI_R_IO, Int_State%soil_mois_ex(1, j, i), lonr_s, MPI_R_IO, 0,            &
                  mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - soil_mois_ex",rcfinal)
         END DO
     END DO
     
! Use the work arrays to get the data from the multiple array and be added
! into the ESMF export state.
!-------------------------------------------------------------------------
         sfc_m1 = Int_State%soil_mois_ex(:, :, 1)
         sfc_m2 = Int_State%soil_mois_ex(:, :, 2)
         sfc_m3 = Int_State%soil_mois_ex(:, :, 3)
         CALL AddF90ArrayToState(expGFS, grid3, SMC_name(1), sfc_m1, rc = rc1)
         CALL AddF90ArrayToState(expGFS, grid3, SMC_name(2), sfc_m2, rc = rc1)
         CALL AddF90ArrayToState(expGFS, grid3, SMC_name(3), sfc_m3, rc = rc1)
 CALL ERR_MSG1(rc1,"Create ESMF State - soil_mois_ex",rcfinal)

     DEALLOCATE(SMC_name)
 END IF

! Add the multiple level soil liquid moisture field into the ESMF export state.
!------------------------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%liquid_soil_moisture_export == 1) THEN

     ALLOCATE(SLC_name(Int_State%lsoil),     stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the working array - SLC_name",rcfinal)

     DO i = 1, Int_State%lsoil
         WRITE(SLC_name(i), 1003) i
     END DO
1003 FORMAT('SLC_', i1)

     ALLOCATE(Int_State%liquid_soil_moisture_ex(lonr_s, latr_s, Int_State%lsoil), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - liquid_soil_moisture_ex",rcfinal)

     DO i = 1, Int_State%lsoil
         DO j = 1, latr_s
             CALL mpi_scatterv(data%slc(1, j, i), ijn_s(1, 2), displs_s(1, 2),      &
                  MPI_R_IO, Int_State%liquid_soil_moisture_ex(1, j, i),             &
                  lonr_s, MPI_R_IO, 0, mpi_comm_all, rc1)
    
 CALL ERR_MSG1(rc1,"MPI_Scatterv - liquid_soil_moisture_ex",rcfinal)
         END DO
     END DO
     
! Use the work arrays to get the data from the multiple array and be added
! into the ESMF export state.
!-------------------------------------------------------------------------
         sfc_lm1 = Int_State%liquid_soil_moisture_ex(:, :, 1)
         sfc_lm2 = Int_State%liquid_soil_moisture_ex(:, :, 2)
         sfc_lm3 = Int_State%liquid_soil_moisture_ex(:, :, 3)
         CALL AddF90ArrayToState(expGFS, grid3, SLC_name(1), sfc_lm1, rc = rc1)
         CALL AddF90ArrayToState(expGFS, grid3, SLC_name(2), sfc_lm2, rc = rc1)
         CALL AddF90ArrayToState(expGFS, grid3, SLC_name(3), sfc_lm3, rc = rc1)
 CALL ERR_MSG1(rc1,"Create ESMF State - liquid_soil_moisture_ex",rcfinal)

     DEALLOCATE(SLC_name)
 END IF

! Add the convective cloud cover data into the ESMF export state.
!----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%conv_cloud_cover_export == 1) THEN
     ALLOCATE(Int_State%conv_cloud_cover_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - conv_cloud_cover_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%cv(1, i), ijn_s(1, 2), displs_s(1, 2),                &
              MPI_R_IO, Int_State%conv_cloud_cover_ex(1, i), lonr_s, MPI_R_IO, 0,     &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - conv_cloud_cover_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'CV', &
         Int_State%conv_cloud_cover_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - CV_ex",rcfinal)
 END IF

! Add the convective cloud base data into the ESMF export state.
!---------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%conv_cloud_base_export == 1) THEN
     ALLOCATE(Int_State%conv_cloud_base_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - conv_cloud_base_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%cvb(1, i), ijn_s(1, 2), displs_s(1, 2),               &
              MPI_R_IO, Int_State%conv_cloud_base_ex(1, i), lonr_s, MPI_R_IO, 0,      &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - conv_cloud_base_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'CVB', &
         Int_State%conv_cloud_base_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - CVB_ex",rcfinal)
 END IF

! Add the convective cloud top er data into the ESMF export state.
!-----------------------------------------------------------------
 IF(Int_State%ESMF_Sta_List%conv_cloud_top_export == 1) THEN
     ALLOCATE(Int_State%conv_cloud_top_ex(lonr_s, latr_s), stat = rc1)

 CALL ERR_MSG1(rc1," - Allocate the Internal State - conv_cloud_top_ex",rcfinal)

     DO i = 1, latr_s
         CALL mpi_scatterv(data%cvt(1, i), ijn_s(1, 2), displs_s(1, 2),               &
              MPI_R_IO, Int_State%conv_cloud_top_ex(1, i), lonr_s, MPI_R_IO, 0,       &
              mpi_comm_all, rc1)

 CALL ERR_MSG1(rc1,"MPI_Scatterv - conv_cloud_top_ex",rcfinal)
     END DO

     CALL AddF90ArrayToState(expGFS, grid3, 'CVT', &
         Int_State%conv_cloud_top_ex, rc = rc1)

 CALL ERR_MSG1(rc1,"Create ESMF State - CVT_ex",rcfinal)
 END IF

! Print out the final error signal information and put it to the rc.
!-------------------------------------------------------------------
 IF(rcfinal == ESMF_SUCCESS) THEN
     PRINT*, "PASS: GFS_InternalState2ESMFExportState"
 ELSE
     PRINT*, "FAIL: GFS_InternalState2ESMFExportState"
 END IF

 IF(PRESENT(rc)) THEN
     rc = rcfinal
 END IF

 END SUBROUTINE GFS_InternalState2ESMFExportState

!SUBROUTINE ERR_MSG1(rc1,msg,rcfinal)
!
!integer, intent(inout)          :: rc1
!integer, intent(out)            :: rcfinal
!character (len=*), intent(in)   :: msg
!IF(ESMF_LogMsgFoundError(rc1, msg)) THEN
!    rcfinal = ESMF_FAILURE
!    PRINT*, 'Error Happened When Running the ESMF_ConfigGetAttribute-', &
!    'rc = ', rc1
!    rc1     = ESMF_SUCCESS
!END IF
!END SUBROUTINE ERR_MSG1
!
! End of the RSMF state module.
!------------------------------
 END MODULE GFS_ESMFStateMod
