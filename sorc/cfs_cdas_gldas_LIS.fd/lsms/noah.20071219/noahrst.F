!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: noahrst.F90
!
! !DESCRIPTION:
!  This program reads restart files for Noah.  This
!   includes all relevant water/energy storages, tile information,
!   and time information.  It also rectifies changes in the tile space.  
!
! !REVISION HISTORY:
!  1  Oct 1999: Jared Entin; Initial code
!  15 Oct 1999: Paul Houser; Significant F90 Revision
!  05 Sep 2001: Brian Cosgrove; Modified code to use Dag Lohmann's NOAA
!               initial conditions if necessary.  This is controlled with
!               local variable NOAAIC.  Normally set to 0 in this subroutine
!               but set to 1 if want to use Dag's NOAA IC's.  Changed output
!               directory structure, and commented out if-then check so that
!               directory is always made.
!  28 Apr 2002: Kristi Arsenault; Added NOAH LSM into LDAS
!  28 May 2002: Kristi Arsenault; For STARTCODE=4, corrected SNEQV values  
!                and put SMC, SH2O, STC limit for GDAS and GEOS forcing.
!
! RESTART FILE FORMAT(fortran sequential binary):
!  YR,MO,DA,HR,MN,SS,VCLASS,NCH !Restart time,Veg class,no.tiles, no.soil lay 
!  TILE(NCH)%COL        !Grid Col of Tile   
!  TILE(NCH)%ROW        !Grid Row of Tile
!  TILE(NCH)%FGRD       !Fraction of Grid covered by tile
!  TILE(NCH)%VEGT       !Vegetation Type of Tile
!  NOAH(NCH)%STATES     !Model States in Tile Space
! 
! !INTERFACE:
#include <misc.h>
subroutine noahrst
! !USES:
  use lisdrv_module, only : lis, grid, tile
  use noah_varder, only : noahdrv
!  use time_module
  USE noah_varder      ! NOAH tile variables
  use time_manager
  use tile_spmdMod
!EOP
  IMPLICIT NONE      
  
  INTEGER :: RW              ! 1=read restart, 2=write restart
  INTEGER :: C,R,T,I,J,L,N,F ! Loop counters
  INTEGER :: FOUND           ! Counting variable
  
  INTEGER :: YR,MO,DA,HR,MN,SS  !Time variables
  INTEGER :: VCLASS,NC,NR,NCH
  
  REAL :: RHOICE=917.0              ! Density of ice
  REAL :: WT1,WT2                   ! Weights for soil wetness initialization

  CHARACTER*80 FILEN,MKFYRMO
  CHARACTER*1  FNAME(80),FBASE(80),FSUBS(80),FMKDIR(80)
  CHARACTER*1  FTIME(10),FYRMODIR(80)
  INTEGER K
  INTEGER NOAAIC   ! 0=Use IC's from card file, 1=Use NOAA IC's from 
  
  integer :: curSec
  real, allocatable :: tmptile(:)
  PARAMETER (NOAAIC=0)

!=== End Variable Definition =============================================
!BOC
!-------------------------------------------------------------------------
! Read Active Archive File
!-------------------------------------------------------------------------
  if(masterproc) then 
     IF(LIS%O%STARTCODE.EQ.1)THEN
        allocate(tmptile(lis%d%nch))
        OPEN(40,FILE=noahdrv%NOAH_RFILE,FORM='unformatted')
        
        call timemgr_read_restart(40)
!3.1
!        call timemgr_restart()
!        call get_curr_date(lis%t%yr,lis%t%mo,lis%t%da,curSec)
!        call sec2time(curSec,lis%t%hr,lis%t%mn,lis%t%ss)
!        call updatetime(lis%t) !Updates LIS variables.
        WRITE(*,*)'NOAH Restart File Used: ',noahdrv%NOAH_RFILE
        READ(40) VCLASS,NC,NR,NCH  !Time, veg class, no. tiles
 write(*,*) VCLASS,NC,NR,NCH
!------------------------------------------------------------------------
!   Check for Vegetation Class Conflict 
!------------------------------------------------------------------------
        IF(VCLASS.NE.LIS%P%VCLASS)THEN
           WRITE(*,*)noahdrv%NOAH_RFILE,LIS%P%VCLASS,VCLASS,' Vegetation class conflict'
           call endrun
        ENDIF
!------------------------------------------------------------------------
!   Check for Grid Space Conflict 
!------------------------------------------------------------------------
        IF(NC.NE.LIS%D%LNC.OR.NR.NE.LIS%D%LNR)THEN
           WRITE(*,*)noahdrv%NOAH_RFILE,'Grid space mismatch - NOAH HALTED'
           call endrun
        ENDIF
!------------------------------------------------------------------------
! Transfer Restart tile space to LIS tile space
!------------------------------------------------------------------------
        IF(NCH.NE.LIS%D%NCH)THEN           
           WRITE(*,*)'Restart Tile Space Mismatch, Halting..'
           call endrun
        endif
        READ(40) noah%T1         !NOAH Skin Temperature (K) 
        READ(40) noah%CMC        !NOAH Canopy Water Content (m)
        READ(40) noah%SNOWH      !NOAH Actual Snow Depth (m) 
        READ(40) noah%SNEQV      !NOAH Water Equivalent Snow Depth (m) 
        DO L=1,4
           READ(40) TMPTILE !NOAH Soil Layer Temp (4 layers)
           noah%STC(L)=TMPTILE
        ENDDO
        DO L=1,4
           READ(40) TMPTILE !NOAH Total soil moist. (4 layers)
           noah%SMC(L)=TMPTILE
        ENDDO
        DO L=1,4
           READ(40) TMPTILE !NOAH Liquid-only soil moist. (4 layers)
           noah%SH2O(L)=TMPTILE
        ENDDO
        READ(40) noah%CH         !NOAH Sfc Exchange Coef. for Heat/Moisture
        READ(40) noah%CM         !NOAH Sfc Exchange Coef. for Momentum
        close(40)        
        deallocate(tmptile)
     endif
  endif

#if ( defined SPMD )
  if ( ( lis%o%startcode == 1 ) .and. ( npes >  1 ) ) then
     call noah_scatter()
  endif
#endif

! print*, noah(180666)%T1
! print*, noah(180666)%CMC
! print*, noah(180666)%SNOWH
! print*, noah(180666)%SNEQV
! print*, noah(180666)%STC
! print*, noah(180666)%SMC
! print*, noah(180666)%SH2O

  print*,'DBG: done noahrst',' (',iam,')'
!EOC
end subroutine noahrst
