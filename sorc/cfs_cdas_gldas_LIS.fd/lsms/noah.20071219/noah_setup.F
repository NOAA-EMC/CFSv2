!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: noah_setup.F90 
!
! !DESCRIPTION:
!  
! Complete the setup routines for noah
!
! !REVISION HISTORY:
!
! 4 Nov. 1999: Paul Houser; Initial Code
! 28 Apr. 2002: K. Arsenault; Modified to NOAH LSM 2.5 code to LDAS
! 
! !INTERFACE:
subroutine noah_setup()
! !USES:
  use lisdrv_module, only: lis,tile 
  use noah_varder
  use spmdMod, only : masterproc, npes
!EOP
  IMPLICIT NONE
  integer :: t, n
!=== End Variable List ===================================================
!BOC
 print*,"noah_setup(",iam,")"
#if ( ! defined OPENDAP )
  if ( masterproc ) then
#endif
 print*,"noah_setup(",iam,")"
     call setnoahp()
     call noah_gfrac()
     call noah_alb()
     call noah_coldstart()
     do t=1,lis%d%nch
        noah(t)%swnet = 0
        noah(t)%lwnet = 0
        noah(t)%qle = 0
        noah(t)%qh = 0
        noah(t)%qg = 0
        noah(t)%snowf = 0
        noah(t)%rainf = 0
        noah(t)%evap = 0
        noah(t)%qs = 0
        noah(t)%qsb = 0
        noah(t)%qsm = 0
        noah(t)%swe = 0
        noah(t)%soilmoist1 = 0
        noah(t)%soilmoist2 = 0
        noah(t)%soilmoist3 = 0
        noah(t)%soilmoist4 = 0
        noah(t)%soilwet = 0
        noah(t)%ecanop = 0
        noah(t)%canopint = 0
        noah(t)%tveg = 0
        noah(t)%esoil = 0
        noah(t)%rootmoist =0
        noah(t)%soilm_prev = 0
        noah(t)%swe_prev = 0
        noah(t)%count = 0
     enddo
#if ( ! defined OPENDAP )
      endif
      if ( npes > 1 ) then
         call noah_scatter
      endif
#endif
!EOC      
    end subroutine noah_setup
