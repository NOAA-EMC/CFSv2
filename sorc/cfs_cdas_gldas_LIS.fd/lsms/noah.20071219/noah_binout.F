!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: noah_binout.F90
!
! !DESCRIPTION:  
!  LIS NOAH data writer: Writes noah output in binary format
!
! !REVISION HISTORY:
! 02 Dec 2003; Sujay Kumar, Initial Version
! 
! !INTERFACE:
subroutine noah_binout(ld,ftn)
! !USES:
  use lis_module
  use drv_output_mod, only : drv_writevar_bin
  use noah_varder
  use lisdrv_module, only : gindex
  implicit none
  
  type(lisdec) :: ld
  integer :: ftn
!EOP
  real :: rainf(ld%d%glbnch)
  real :: snowf(ld%d%glbnch)
  real :: ftemp(ld%d%lnc, ld%d%lnr)
  integer :: t,c,r
!BOC
  do t=1,ld%d%glbnch
     if(noah(t)%forcing(1) < 273.15) then
        rainf(t) = 0.0
        snowf(t) = noah(t)%forcing(8)
     else
        rainf(t) = noah(t)%forcing(8)
        snowf(t) = 0.0
     endif
  enddo
!---------------------------------------------------------------------------
! General Energy Balance Components
!---------------------------------------------------------------------------
  ! noah%swnet = noah%swnet/float(noah%count)
!   ftemp = -9999.0
!   do c=1,ld%d%lnc
!      do r=1,ld%d%lnr
!         if(gindex(c,r).ne.-1) then 
!            ftemp(c,r) = noah(gindex(c,r))%swnet
!         endif
!      enddo
!   enddo
!   open(59,file='swnet.bin',form='unformatted')
!   write(59) ftemp
!   close(59)

  ! call drv_writevar_bin(ftn,noah%swnet) !Net shortwave radiation (surface) (W/m2)
  ! noah%lwnet = (-1)*noah%lwnet/float(noah%count)
  ! call drv_writevar_bin(ftn,noah%lwnet)!Net longwave radiation (surface) (W/m2)
  ! noah%qle = noah%qle/float(noah%count)
  ! call drv_writevar_bin(ftn,noah%qle) !Latent Heat Flux (W/m2)
  ! noah%qh = noah%qh/float(noah%count)
  ! call drv_writevar_bin(ftn,noah%qh) !Sensible Heat Flux (W/m2)
  ! noah%qg = noah%qg/float(noah%count)
  ! call drv_writevar_bin(ftn,noah%qg)
!---------------------------------------------------------------------------
! General Water Balance Components
!---------------------------------------------------------------------------
  ! noah%snowf = noah%snowf/float(noah%count)
  ! call drv_writevar_bin(ftn,noah%snowf)
  ! noah%rainf = noah%rainf/float(noah%count)
  ! call drv_writevar_bin(ftn,noah%rainf)
  ! noah%evap = noah%evap/float(noah%count)
  ! call drv_writevar_bin(ftn,noah%evap)
  ! noah%qs = noah%qs/float(noah%count)
  ! call drv_writevar_bin(ftn,noah%qs)
  ! noah%qsb = noah%qsb/float(noah%count)
  ! call drv_writevar_bin(ftn,noah%qsb)
  ! noah%qsm = noah%qsm/float(noah%count)
  ! call drv_writevar_bin(ftn,noah%qsm)
  ! call drv_writevar_bin(ftn,noah%smc(1)*1000.0*0.1+ &
  !      noah%smc(2)*1000.0*0.3 + & 
  !      noah%smc(3)*1000.0*0.6 + & 
  !      noah%smc(4)*1000.0 -noah%soilm_prev)
  ! call drv_writevar_bin(ftn,noah%swe-noah%swe_prev)
!---------------------------------------------------------------------------
! Surface State Variables
!---------------------------------------------------------------------------
   call drv_writevar_bin(ftn,noah%avgsurft)
  ! call drv_writevar_bin(ftn,noah%albedo)
!JESSE 20050127   noah%swe= noah%swe/float(noah%count)
   call drv_writevar_bin(ftn,noah%sneqv*1000.)
!---------------------------------------------------------------------------
! Subsurface State Variables
! JESSE state variables are instantaneous
!---------------------------------------------------------------------------
!   noah%soilmoist1= noah%soilmoist1/float(noah%count)
   call drv_writevar_bin(ftn,noah%smc(1))
!   noah%soilmoist2= noah%soilmoist2/float(noah%count)
   call drv_writevar_bin(ftn,noah%smc(2))
!   noah%soilmoist3= noah%soilmoist3/float(noah%count)
   call drv_writevar_bin(ftn,noah%smc(3))
!   noah%soilmoist4= noah%soilmoist4/float(noah%count)
   call drv_writevar_bin(ftn,noah%smc(4))
!   noah%soilwet= noah%soilwet/float(noah%count)
  ! call drv_writevar_bin(ftn,noah%soilwet)
!---------------------------------------------------------------------------
! Evaporation Components
!---------------------------------------------------------------------------
  ! noah%tveg= noah%tveg/float(noah%count)
  ! call drv_writevar_bin(ftn,noah%tveg)
  ! noah%esoil= noah%esoil/float(noah%count)
  ! call drv_writevar_bin(ftn,noah%esoil)
! !  noah%rootmoist = noah%rootmoist/float(noah%count)
  ! call drv_writevar_bin(ftn, noah%rootmoist)
!---------------------------------------------------------------------------
! Forcing
!---------------------------------------------------------------------------
   if(ld%o%wfor.eq.1) then
      call drv_writevar_bin(ftn, sqrt(noah%forcing(5)*noah%forcing(5)+ & 
           noah%forcing(6)*noah%forcing(6)))
      call drv_writevar_bin(ftn,rainf)
      call drv_writevar_bin(ftn,snowf)
      call drv_writevar_bin(ftn,noah%forcing(1))
      call drv_writevar_bin(ftn,noah%forcing(2))
      call drv_writevar_bin(ftn,noah%forcing(7))
      call drv_writevar_bin(ftn,noah%forcing(3))
      call drv_writevar_bin(ftn,noah%forcing(4))
   endif
!---------------------------------------------------------------------------
! Soil Temperature !jesse 20040423
!---------------------------------------------------------------------------
   call drv_writevar_bin(ftn,noah%stc(1))
   call drv_writevar_bin(ftn,noah%stc(2))
   call drv_writevar_bin(ftn,noah%stc(3))
   call drv_writevar_bin(ftn,noah%stc(4))
!---------------------------------------------------------------------------
! Snow Depth !jesse 20050127
!---------------------------------------------------------------------------
   call drv_writevar_bin(ftn,noah%snowh)
!---------------------------------------------------------------------------
! CH and CM !jesse 20060410
!---------------------------------------------------------------------------
  ! call drv_writevar_bin(ftn,noah%ch)
  ! call drv_writevar_bin(ftn,noah%cm)
!---------------------------------------------------------------------------
! Liquid Soil Moisture !jesse 20060410
!---------------------------------------------------------------------------
   call drv_writevar_bin(ftn,noah%sh2o(1))
   call drv_writevar_bin(ftn,noah%sh2o(2))
   call drv_writevar_bin(ftn,noah%sh2o(3))
   call drv_writevar_bin(ftn,noah%sh2o(4))
!---------------------------------------------------------------------------
! Canopy Moisture [m] !jesse 20070622
!---------------------------------------------------------------------------
   call drv_writevar_bin(ftn,noah%cmc)
!---------------------------------------------------------------------------
!   call drv_writevar_bin(ftn,rainf)
!EOC
 end subroutine noah_binout
 
