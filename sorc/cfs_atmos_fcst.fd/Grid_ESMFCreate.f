! This code is used to create the ESMF grids for the GFS ESMF model.
! Weiyu Yang, 09/2005.
!-------------------------------------------------------------------

! Modified for using the ESMF 3.1.0 new grid interface. 02/15/2008.
! Weiyu Yang.
!------------------------------------------------------------------


 SUBROUTINE Grid_ESMFCreate1(vm, grid1, grid3, grid4, DistGrid1, &
     DistGrid3, DistGrid4, Int_State, rc)

!
!!USES:
!
 USE ESMF_Mod                  ! The ESMF library.
 USE GFS_InternalState_ESMFMod ! The contents of the ESMF internal state.

 IMPLICIT none

 TYPE(ESMF_VM),           INTENT(inout) :: vm           ! the ESMF virtual machine.
 TYPE(ESMF_Grid),         INTENT(inout) :: grid1        ! the ESMF GRID TYPE ARRAY.
                                                        ! For the single level spectral arrays.
 TYPE(ESMF_Grid),         INTENT(inout) :: grid3        ! the ESMF GRID TYPE ARRAY.
                                                        ! For the Gaussian grid surface arrays.
 TYPE(ESMF_Grid),         INTENT(inout) :: grid4        ! the ESMF GRID TYPE ARRAY. For the 
                                                        ! GFS start date and time information.
 INTEGER, INTENT(out)                   :: rc           ! Error signal variable.
 TYPE(GFS_InternalState), INTENT(inout) :: Int_State    ! The ESMF internal state.

 TYPE(ESMF_DistGrid),     INTENT(inout) :: DistGrid1    ! the ESMF DistGrid.
 TYPE(ESMF_DistGrid),     INTENT(inout) :: DistGrid3    ! the ESMF DistGrid.
 TYPE(ESMF_DistGrid),     INTENT(inout) :: DistGrid4    ! the ESMF DistGrid.
 INTEGER                                :: rc1          ! Error signal work variable.

 INTEGER,                 DIMENSION(2)  :: counts       ! Parameter array to set up the 
                                                        ! size of the 2-D ESMF grid.
 INTEGER,                 DIMENSION(2)  :: min, max     ! Parameter arrays to set up the
                                                        ! start number and the end number of
                                                        ! the ESMF grid in each dimension.

! Initialize the error signal variables.
!---------------------------------------
 rc1 = ESMF_SUCCESS
 rc  = ESMF_SUCCESS

! Create grid.
! Use uniform grid to represent both the Gaussian grid 
! and the spectral space grids, since no dx, dy is needed.
!---------------------------------------------------------

! Create the single level spectral ESMF grid.  The first dimension is the
! spectral coefficient, that is a 1-D array.  Thus the second dimension
! size is one.  Grid starts from 1 and end at the total number of the
! spectral coefficients.
!------------------------------------------------------------------------
 counts(1)        = (Int_State%jcap+1)*(Int_State%jcap+2)
 counts(2)        = 1
 min(1)           = 1
 min(2)           = 1
 max(1)           = counts(1)
 max(2)           = counts(2)

! Create the ESMF DistGrid1 using the 1-D default decomposition.
!---------------------------------------------------------------
     CALL ESMF_LogWrite("Create DistGrid1", ESMF_LOG_INFO, rc = rc1)

 DistGrid1 = ESMF_DistGridCreate(minIndex = min, maxIndex = max, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create DistGrid1")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating DistGrid1, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Create the ESMF grid1 based on the created ESMF DistGrid1 information.
!-----------------------------------------------------------------------
     CALL ESMF_LogWrite("Create Grid1", ESMF_LOG_INFO, rc = rc1)

 grid1 = ESMF_GridCreate(name = "GFS grid1", distgrid = DistGrid1, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create Grid1")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating Grid1, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Set up parameter arrays for the ESMF grid of the Gaussian grid space.
! The first dimension is the longitude direction which will be 
! parallelized for the ESMF interface state.  The second dimension is 
! the latitude direction.
!----------------------------------------------------------------------
 counts(1) = Int_State%lonr
 counts(2) = Int_State%latr
 max(1)    = counts(1)
 max(2)    = counts(2)

! Create the ESMF DistGrid3 using the 1-D default decomposition.
!---------------------------------------------------------------
     CALL ESMF_LogWrite("Create DistGrid3", ESMF_LOG_INFO, rc = rc1)

 DistGrid3 = ESMF_DistGridCreate(minIndex = min, maxIndex = max, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create DistGrid3")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating DistGrid3, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Create the ESMF grid3 based on the created ESMF DistGrid3 information.
! Grid3 is the grid for the Gaussian grid space.
!-----------------------------------------------------------------------
     CALL ESMF_LogWrite("Create Grid3", ESMF_LOG_INFO, rc = rc1)

 grid3 = ESMF_GridCreate(name = "GFS grid3", distgrid = DistGrid3, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create Grid3")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating Grid3, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Set up parameter arrays for the ESMF grid used for the date and time
! information to run the GFS.  All processors contains the same five date
! and time valus.
!------------------------------------------------------------------------
 counts(1) = Int_State%NODES
 counts(2) = 5
 max(1)    = counts(1)
 max(2)    = counts(2)

! Create the ESMF DistGrid4 using the 1-D default decomposition.
!---------------------------------------------------------------
     CALL ESMF_LogWrite("Create DistGrid4", ESMF_LOG_INFO, rc = rc1)

 DistGrid4 = ESMF_DistGridCreate(minIndex = min, maxIndex = max, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create DistGrid4")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating DistGrid4, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Create the ESMF grid4 based on the created ESMF DistGrid3 information.
!-----------------------------------------------------------------------
     CALL ESMF_LogWrite("Create Grid4", ESMF_LOG_INFO, rc = rc1)

 grid4 = ESMF_GridCreate(name = "GFS grid4", distgrid = DistGrid4, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create Grid4")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating Grid4, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Finally print out the error signal information and put it to "rc".
!-------------------------------------------------------------------
 IF(rc == ESMF_SUCCESS) THEN
     PRINT*, "PASS: Grid_ESMFCreate1."
 ELSE
     PRINT*, "FAIL: Grid_ESMFCreate1."
 END IF

 END SUBROUTINE Grid_ESMFCreate1






! Subroutine Grid_ESMFCreate2 is the same as the Grid_ESMFCreate1, except 
! it creates the grid2, the multiple spectral array ESMF grid, instead of
! creating the grid3, the Gaussian grid array ESMF grid.
! The detailed description comments please refer to the subroutine
! "Grid_ESMFCreate1".
!------------------------------------------------------------------------
 SUBROUTINE Grid_ESMFCreate2(vm, grid1, grid2, grid4, Int_State, rc)
!
!!USES:
!
 USE ESMF_Mod                 ! The ESMF library.
 USE GFS_InternalState_ESMFMod

 TYPE(ESMF_VM),           INTENT(inout) :: vm           ! the ESMF virtual machine.
 TYPE(ESMF_Grid),         INTENT(inout) :: grid1        ! the ESMF GRID TYPE ARRAY.
 TYPE(ESMF_Grid),         INTENT(inout) :: grid2        ! the ESMF GRID TYPE ARRAY.
 TYPE(ESMF_Grid),         INTENT(inout) :: grid4        ! the ESMF GRID TYPE ARRAY.
 INTEGER,                 INTENT(out)   :: rc
 TYPE(GFS_InternalState), INTENT(inout) :: Int_State


 TYPE(ESMF_DistGrid)               :: DistGrid1         ! the ESMF DistGrid.
 TYPE(ESMF_DistGrid)               :: DistGrid2         ! the ESMF DistGrid.
 TYPE(ESMF_DistGrid)               :: DistGrid4         ! the ESMF DistGrid.

 INTEGER                           :: rc1

 INTEGER,            DIMENSION(2)  :: counts
 INTEGER,            DIMENSION(2)  :: min, max

 rc1 = ESMF_SUCCESS
 rc  = ESMF_SUCCESS


! Create grid.
! Use uniform grid to represent the Gaussian grid
! since no dx, dy is needed.
!--------------------------------------------------
 counts(1)        = (Int_State%jcap+1)*(Int_State%jcap+2)
 counts(2)        = 1
 min(1)           = 1
 min(2)           = 1
 max(1)           = counts(1)
 max(2)           = counts(2)

! Create the ESMF DistGrid1 using the 1-D default decomposition.
!---------------------------------------------------------------
     CALL ESMF_LogWrite("Create DistGrid1", ESMF_LOG_INFO, rc = rc1)

 DistGrid1 = ESMF_DistGridCreate(minIndex = min, maxIndex = max, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create DistGrid1")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating DistGrid1, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Create the ESMF grid1 based on the created ESMF DistGrid1 information.
!-----------------------------------------------------------------------
     CALL ESMF_LogWrite("Create Grid1", ESMF_LOG_INFO, rc = rc1)

 grid1 = ESMF_GridCreate(name = "GFS grid1", distgrid = DistGrid1, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create Grid1")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating Grid1, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 counts(2)        = Int_State%levs
 max(2)           = counts(2)

! Create the ESMF DistGrid2 using the 1-D default decomposition.
!---------------------------------------------------------------
     CALL ESMF_LogWrite("Create DistGrid2", ESMF_LOG_INFO, rc = rc1)

 DistGrid2 = ESMF_DistGridCreate(minIndex = min, maxIndex = max, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create DistGrid2")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating DistGrid2, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Create the ESMF grid2 based on the created ESMF DistGrid2 information.
!-----------------------------------------------------------------------
     CALL ESMF_LogWrite("Create Grid2", ESMF_LOG_INFO, rc = rc1)

 grid2 = ESMF_GridCreate(name = "GFS grid2", distgrid = DistGrid2, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create Grid2")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating Grid2, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 counts(1) = Int_State%NODES
 counts(2) = 5
 max(1)    = counts(1)
 max(2)    = counts(2)

! Create the ESMF DistGrid4 using the 1-D default decomposition.
!---------------------------------------------------------------
     CALL ESMF_LogWrite("Create DistGrid4", ESMF_LOG_INFO, rc = rc1)

 DistGrid4 = ESMF_DistGridCreate(minIndex = min, maxIndex = max, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create DistGrid4")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating DistGrid4, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

! Create the ESMF grid4 based on the created ESMF DistGrid2 information.
!-----------------------------------------------------------------------
     CALL ESMF_LogWrite("Create Grid4", ESMF_LOG_INFO, rc = rc1)

 grid4 = ESMF_GridCreate(name = "GFS grid4", distgrid = DistGrid4, rc = rc1)

     IF(ESMF_LogMsgFoundError(rc1, "Create Grid4")) THEN
         rc = ESMF_FAILURE
         PRINT*, 'Error Happened When Creating Grid4, rc = ', rc1
         rc1 = ESMF_SUCCESS
     END IF

 IF(rc == ESMF_SUCCESS) THEN
     PRINT*, "PASS: Grid_ESMFCreate2."
 ELSE
     PRINT*, "FAIL: Grid_ESMFCreate2."
 END IF

 END SUBROUTINE Grid_ESMFCreate2
