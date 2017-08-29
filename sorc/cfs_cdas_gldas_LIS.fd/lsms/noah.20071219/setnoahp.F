!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
#include <misc.h>
#include "absoft.h"
!BOP
!
! !ROUTINE: setnoahp.F90
!
! !DESCRIPTION:
!  This subroutine retrieves NOAH parameters - Significant F90 revisions
!   below this subroutine will be required in the future.  
!
! !REVISION HISTORY:
!  28 Apr 2002: Kristi Arsenault;  Added NOAH LSM, Initial Code
!  13 Oct 2003: Sujay Kumar; Domain independent modifications
!
! !INTERFACE:
subroutine setnoahp
! !USES:
  use lisdrv_module, only : grid,tile,lis
  use noah_varder      ! NOAH tile variables
#if ( defined OPENDAP )
  use opendap_module
#endif
!EOP      
  implicit none

!+++ Local Parameters for new soil definition ++++++++++++++++++++++++++++
!+++  Layer depths correspond to soil property map files - do not change!
!      Thicknesses of soil layers for any simulations
!      These are compatible with the FIRST set of Matt Rodell's
!      soil files, i.e. sand_nlis3.5.bfsa

  real,allocatable :: stype(:,:)
  integer :: cindex, rindex


  REAL, PARAMETER :: D1 = 0.02      ! thickness of soil layer 1, m
  REAL, PARAMETER :: D2 = 1.48      ! thickness of soil layer 2, m
  REAL, PARAMETER :: D3 = 2.00      ! thickness of soil layer 3, m
  
  !+++  Maximum allowable porosity.
  REAL, PARAMETER :: PORMAX=0.70

!=== Local Variables =====================================================

  INTEGER :: N,I,J,K,JJ,c,r                  !Loop counters
  real, allocatable :: SOILTYP(:,:)
  INTEGER, allocatable :: PLACESLTYP(:)
  REAL :: VALUE(LIS%P%NT,noahdrv%NOAH_NVEGP)
  REAL :: BASICSET(noahdrv%NOAH_ZST,noahdrv%NOAH_NSOILP)
  REAL, allocatable :: TMPALB(:,:)
  REAL, allocatable :: PLACETBOT(:,:)
  REAL, allocatable :: SAND1(:,:)
  REAL, allocatable :: CLAY1(:,:)
!BOC  
#if ( ! defined OPENDAP )
  integer :: tnroffset = 0
#endif
!-----------------------------------------------------------------------
! Convert UMD Classes to SIB Classes for Each Tile
!-----------------------------------------------------------------------
!J  print*,'MSG: setnoahp -- Calling MAPVEGC to convert UMD to SIB', & 
!       ' (', iam,')' 
  print*,'DBG: setnoahp -- nch',lis%d%nch,' (',iam,')'
!  
  do n=1,lis%d%nch
!     call mapvegc(tile(n)%vegt)       !jesse 20040507 NCEP vtype is SIB
     noah(n)%vegt = tile(n)%vegt
     noah(n)%sibveg = noah(n)%vegt
     if( noah(n)%sibveg .LT. 1 .OR. noah(n)%sibveg .GT. 13 ) &
         noah(n)%sibveg = 11
  enddo   
!  print*,'DBG: setnoahp -- left MAPVEGC',' (',iam,')'
!-----------------------------------------------------------------------
! Get Vegetation Parameters for NOAH Model in Tile Space
! Read in the NOAH Static Vegetation Parameter Files
!-----------------------------------------------------------------------
  open(unit=11,file=noahdrv%noah_vfile,status='old')
  
  do j=1,noahdrv%noah_nvegp
     read(11,*)(value(i,j),i=1,lis%p%nt)
  enddo
  close(11)
!-----------------------------------------------------------------------
! Assign STATIC vegetation parameters to each tile based on the
! type of vegetation present in that tile.
! These parameters will be stored in one long array--structured
! as follows: Tile 1, all the parameters (1 through numparam)
! then Tile 2, all the parameters. 
! Then Tile 3, all the parameters etc.
!-----------------------------------------------------------------------

  do i=1,lis%d%nch
     do j=1,noahdrv%noah_nvegp
        noah(i)%vegp(j)=value(tile(i)%vegt,j)
     enddo 
  enddo 
!-----------------------------------------------------------------------
! Read in bottom temperature fields and adjust for elevation differnce
! with either Eta (NLDAS) or GDAS (GLDAS) correction datasets. 
!-----------------------------------------------------------------------
#if ( defined OPENDAP )
  allocate(placetbot(parm_nc,1+nroffset:parm_nr+nroffset))
#else
  allocate(placetbot(lis%d%gnc,lis%d%gnr))
#endif

#if ( defined OPENDAP )
  print*, 'MSG: setnoahp -- Retrieving TBOT file ', & 
       trim(noahdrv%noah_tbot), ' (',iam,')'
  call system("opendap_scripts/gettbot.pl "//ciam//" "// & 
       trim(noahdrv%noah_tbot)//" "//              & 
       cparm_slat//" "//cparm_nlat//" "//          & 
       cparm_wlon//" "//cparm_elon)
#endif
  print *, 'Opening GLDAS TBOT File ', noahdrv%NOAH_TBOT
  OPEN(UNIT=12, FILE=noahdrv%NOAH_TBOT, & 
       FORM="UNFORMATTED")
  
  READ(12) PLACETBOT
  placetbot=0.
  READ(12) PLACETBOT
  placetbot=0.
  READ(12) PLACETBOT
  CLOSE(12)
 
  print*, 'MSG: setnoahp -- Read TBOT file',' (',iam,')'
  do i = 1, lis%d%nch
     if(placetbot(tile(i)%col,tile(i)%row-tnroffset).ne.-9999.00) then
        noah(i)%tempbot = placetbot(tile(i)%col, tile(i)%row-tnroffset)
     endif
  enddo
  call absoft_release_cache()
  
!-----------------------------------------------------------------------
! The MAX SNOW ALBEDO field is opened and read in here:
!-----------------------------------------------------------------------
#if ( defined OPENDAP )
  allocate(tmpalb(parm_nc,1+nroffset:parm_nr+nroffset))
#else
  allocate(tmpalb(lis%d%gnc,lis%d%gnr))
#endif

#if ( defined OPENDAP )
  print*, 'MSG: setnoahp -- Retrieving MAXSNALB file ', & 
       trim(noahdrv%noah_mxsnal), ' (',iam,')' 
  call system("opendap_scripts/getmaxsnalb.pl "//ciam//" "// & 
       trim(noahdrv%noah_mxsnal)//" "//                & 
       cparm_slat//" "//cparm_nlat//" "//              & 
       cparm_wlon//" "//cparm_elon)
#endif
  
  print *, 'MSG: setnoahp -- Opening GLDAS MAXSNALB File ', & 
       trim(noahdrv%noah_mxsnal), ' (',iam,')'
  OPEN(UNIT=12,FILE=noahdrv%NOAH_MXSNAL, & 
       FORM="UNFORMATTED")
  
  READ(12) TMPALB
  tmpalb=0.
  READ(12) TMPALB
  tmpalb=0.
  READ(12) TMPALB
  CLOSE(12)
  
  do i=1,lis%d%nch
     if(tmpalb(tile(i)%col, tile(i)%row-tnroffset).ne.-9999.00) then
        noah(i)%mxsnalb = tmpalb(tile(i)%col, tile(i)%row-tnroffset)
     endif
  enddo
  
  call absoft_release_cache()
  
  print*, 'MSG: setnoahp -- Read mxsnalb',' (',iam,')'
  print*, 'MSG: setnoahp -- reading z0 files',' (',iam,')'
!-----------------------------------------------------------------------
! The SURFACE ROUGHNESS Z0 field is opened and read in here:
! Jesse 20060412
!-----------------------------------------------------------------------
!#if ( defined OPENDAP )
!  allocate(tmpalb(parm_nc,1+nroffset:parm_nr+nroffset))
!#else
!  allocate(tmpalb(lis%d%gnc,lis%d%gnr))
!#endif

!#if ( defined OPENDAP )
 ! print*, 'MSG: setnoahp -- Retrieving Z0 file ', &
 !      trim(noahdrv%noah_mxsnal), ' (',iam,')'
 ! call system("opendap_scripts/getmaxsnalb.pl "//ciam//" "// &
 !      trim(noahdrv%noah_mxsnal)//" "//                &
 !      cparm_slat//" "//cparm_nlat//" "//              &
 !      cparm_wlon//" "//cparm_elon)
!#endif

  print *, 'MSG: setnoahp -- Opening GLDAS Z0 File', &
           ' FIX/z0_gfs.bfsa', ' (',iam,')'
  OPEN(UNIT=12,FILE='FIX/z0_gfs.bfsa', &
       FORM="UNFORMATTED")

  !     trim(noahdrv%noah_mxsnal), ' (',iam,')'
  ! OPEN(UNIT=12,FILE=noahdrv%NOAH_MXSNAL, &
  !     FORM="UNFORMATTED")

  READ(12) TMPALB
  tmpalb=0.
  READ(12) TMPALB
  tmpalb=0.
  READ(12) TMPALB
  CLOSE(12)

  do i=1,lis%d%nch
     if(tmpalb(tile(i)%col, tile(i)%row-tnroffset).ne.-9999.00) then
        noah(i)%z0 = tmpalb(tile(i)%col, tile(i)%row-tnroffset)
     endif
  enddo

  call absoft_release_cache()

  print*, 'MSG: setnoahp -- Read z0',' (',iam,')'
  print*,'MSG: setnoahp -- reading soil and clay files',' (',iam,')'
!-----------------------------------------------------------------------
! Open soil files (sand, clay, and porosity).
!-----------------------------------------------------------------------
#if ( defined OPENDAP )
  allocate(sand1(parm_nc,parm_nr))
  allocate(clay1(parm_nc,parm_nr))
  print*, 'MSG: setnoahp -- Retrieving SAND file ', & 
       trim(lis%p%safile), ' (',iam,')'
  call system("opendap_scripts/getsand.pl "//ciam//" "// & 
       trim(lis%p%safile)//" "//                      & 
       cparm_slat//" "//cparm_nlat//" "//          & 
       cparm_wlon//" "//cparm_elon)
  print*, 'MSG: setnoahp -- Retrieving CLAY file ', & 
       trim(lis%p%clfile), ' (',iam,')'
  call system("opendap_scripts/getclay.pl "//ciam//" "// & 
       trim(lis%p%clfile)//" "//                      & 
       cparm_slat//" "//cparm_nlat//" "//          & 
       cparm_wlon//" "//cparm_elon)
#else
!  allocate(sand1(lis%d%gnc,lis%d%gnr))
!  allocate(clay1(lis%d%gnc,lis%d%gnr))
!  allocate(soiltyp(lis%d%gnc,lis%d%gnr))
#endif
!  OPEN(15,FILE=LIS%P%SAFILE,FORM='UNFORMATTED',STATUS='OLD')
  allocate(sand1(lis%d%gnc,lis%d%gnr))
  allocate(clay1(lis%d%gnc,lis%d%gnr))
  allocate(soiltyp(lis%d%gnc,lis%d%gnr))
  print*, 'MSG: setnoahp -- Retrieving SOILTYPE file ', &
       trim(lis%p%clfile), ' (',iam,')'
  print*,'Allocated ',lis%d%gnc,lis%d%gnr
   OPEN(16,FILE=LIS%P%CLFILE,FORM='UNFORMATTED',STATUS='OLD')
   READ(16)sand1
!   sand1=0.
   read(16)clay1
!   clay1=0.
   READ(16)soiltyp
   CLOSE(16)

  deallocate(sand1)
  deallocate(clay1)
  call absoft_release_cache()
!-----------------------------------------------------------------------
! Read in the NOAH Soil Parameter File
!-----------------------------------------------------------------------
  OPEN(UNIT=18,FILE=noahdrv%NOAH_SFILE,STATUS='OLD', & 
       ACCESS='SEQUENTIAL')
  
  DO I=1,noahdrv%NOAH_NSOILP
     READ(18,*)(BASICSET(JJ,I),JJ=1,noahdrv%NOAH_ZST)
  ENDDO
  CLOSE(18)
  print*, 'MSG: setnoahp -- read sfile ', trim(noahdrv%NOAH_SFILE), & 
              ' (', iam, ')'
!-----------------------------------------------------------------------
! Convert grid space to tile space for soil type values.
!-----------------------------------------------------------------------
  allocate(placesltyp(lis%d%nch))
  do i=1,lis%d%nch
     placesltyp(i) = int(soiltyp(tile(i)%col,tile(i)%row-tnroffset))
     if ( placesltyp(i) .LT. 1 .OR. placesltyp(i) .GT. 9 ) &
          placesltyp(i) = 2
     noah(i)%zobsoil = placesltyp(i)
  end do

  if(lis%o%wparam.eq.1) then 
     allocate(stype(lis%d%lnc,lis%d%lnr))
     stype = -9999.0
     do i=1,lis%d%nch      
!3.1
        rindex = tile(i)%row - (lis%d%gridDesc(4)-lis%d%gridDesc(44)) &
             /lis%d%gridDesc(9)
        cindex = tile(i)%col - (lis%d%gridDesc(5)-lis%d%gridDesc(45)) &
             /lis%d%gridDesc(10)
!        rindex = tile(i)%row - (lis%d%kgds(4)-lis%d%kgds(44)) &
!             /lis%d%kgds(9)
!        cindex = tile(i)%col - (lis%d%kgds(5)-lis%d%kgds(45)) &
!             /lis%d%kgds(10)
        !           stype(cindex,rindex) = noah(i)%zobsoil(1)*1.0
        stype(cindex,rindex) =soiltyp(cindex,rindex)
!        print*,'SOIL2 ',i,j,soiltyp(i,j)
     enddo
     
     open(32,file="soiltype.bin",form='unformatted')
     write(32) stype
     close(32)  
     deallocate(stype)
  endif
  
  deallocate(soiltyp)
  call absoft_release_cache()
!-----------------------------------------------------------------------
! Assign SOIL Parameters to each tile based on the
! type of Zobler soil class present in that tile.
!-----------------------------------------------------------------------
  DO I=1,lis%d%nch            !Tile loop
     K=PLACESLTYP(I)           !Soil type
     DO J=1,noahdrv%NOAH_NSOILP   !Soil parameter loop
        NOAH(I)%SOILP(J)=BASICSET(K,J)
     ENDDO !J
  ENDDO !I
  deallocate(tmpalb)
  deallocate(placetbot)
  deallocate(placesltyp)
  call absoft_release_cache()
  return
!EOC
end subroutine setnoahp
