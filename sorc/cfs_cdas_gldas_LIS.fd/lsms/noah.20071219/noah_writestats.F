!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: noah_writevars.F90
!
! !DESCRIPTION:  
!  LIS NOAH data writer: Writes noah output
!
! !REVISION HISTORY:
! 02 Dec 2003; Sujay Kumar, Initial Version
! 
! !INTERFACE:
subroutine noah_writestats(ld,ftn_stats)
! !USES:
  use lisdrv_module, only : gindex
  use lis_module
  use noah_varder
 
  implicit none
  
  type(lisdec) :: ld
  integer :: ftn_stats,t
!EOP
  real :: vmean,vstdev,vmin,vmax
  real :: rainf(ld%d%glbnch)
  real :: snowf(ld%d%glbnch)
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
   call stats(noah%swnet,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,999) 'SWnet(W/m2)', &
        vmean,vstdev,vmin,vmax
   call stats(noah%lwnet,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,999) 'LWnet(W/m2)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%qle,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,999) 'Qle(W/m2)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%qh,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,999) 'Qh(W/m2)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%qg,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,999) 'Qg(W/m2)',&
        vmean,vstdev,vmin,vmax
!---------------------------------------------------------------------------
! General Water Balance Components
!---------------------------------------------------------------------------
   call stats(noah%snowf,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,998) 'Snowf(kg/m2s)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%rainf,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,998) 'Rainf(kg/m2s)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%evap,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,998) 'Evap(kg/m2s)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%qs,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,998) 'Qs(kg/m2s)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%qsb,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,998) 'Qsb(kg/m2s)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%smc(1)*1000.0*0.1+ &
        noah%smc(2)*1000.0*0.3 + & 
        noah%smc(3)*1000.0*0.6 + & 
        noah%smc(4)*1000.0 -noah%soilm_prev, & 
        ld%d%udef,ld%d%glbnch,vmean,vstdev,vmin, vmax)
   write(ftn_stats,999) 'DelSoilMoist(kg/m2s)', & 
        vmean,vstdev,vmin,vmax
   call stats( noah%sneqv*1000.0-noah%swe_prev, & 
        ld%d%udef,ld%d%glbnch,vmean,vstdev,vmin, vmax)
   write(ftn_stats,999) 'DelSWE(kg/m2s)', & 
        vmean,vstdev,vmin,vmax
!---------------------------------------------------------------------------
! Surface State Variables
!---------------------------------------------------------------------------
   call stats(noah%avgsurft,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,999) 'AvgSurfT(K)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%albedo,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,998) 'Albedo(-)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%swe,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,998) 'SWE(kg/m2)',&
        vmean,vstdev,vmin,vmax
!---------------------------------------------------------------------------
! Subsurface State Variables
!---------------------------------------------------------------------------
   call stats(noah%soilmoist1,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,999) 'SoilMoist1(kg/m2)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%soilmoist2,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,999) 'SoilMoist2(kg/m2)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%soilmoist3,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,999) 'SoilMoist3(kg/m2)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%soilmoist4,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,999) 'SoilMoist4(kg/m2)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%soilwet,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,998) 'SoilWet(-)',&
        vmean,vstdev,vmin,vmax
!---------------------------------------------------------------------------
! Evaporation Components
!---------------------------------------------------------------------------
   call stats(noah%tveg,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,998) 'TVeg(kg/m2s)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%esoil,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,998) 'ESoil(kg/m2s)',&
        vmean,vstdev,vmin,vmax
   call stats(noah%rootmoist,ld%d%udef,ld%d%glbnch,vmean, & 
        vstdev,vmin, vmax)
   write(ftn_stats,998) 'RootMoist(kg/m2)',&
        vmean,vstdev,vmin,vmax
   if(ld%o%wfor.eq.1) then
      call stats(sqrt(noah%forcing(5)*noah%forcing(5)+ & 
           noah%forcing(6)*noah%forcing(6)), & 
           ld%d%udef,ld%d%glbnch,vmean,vstdev,vmin, vmax)
      write(ftn_stats,999) 'Wind(m/s)', & 
           vmean,vstdev,vmin,vmax
      call stats(rainf, & 
           ld%d%udef,ld%d%glbnch,vmean,vstdev,vmin, vmax)
      write(ftn_stats,998) 'Rainf(kg/m2s)', & 
           vmean,vstdev,vmin,vmax
      call stats(snowf, & 
           ld%d%udef,ld%d%glbnch,vmean,vstdev,vmin, vmax)
      write(ftn_stats,998) 'Snowf(kg/m2s)', & 
           vmean,vstdev,vmin,vmax
      call stats(noah%forcing(1),ld%d%udef,ld%d%glbnch,vmean,vstdev, & 
           vmin, vmax)
      write(ftn_stats,999) 'Tair(K)', & 
           vmean,vstdev,vmin,vmax
      call stats(noah%forcing(2),ld%d%udef,ld%d%glbnch,vmean,vstdev, & 
           vmin, vmax)
      write(ftn_stats,999) 'Qair(kg/kg)', & 
           vmean,vstdev,vmin,vmax
      call stats(noah%forcing(7),ld%d%udef,ld%d%glbnch,vmean,vstdev, & 
           vmin, vmax)
      write(ftn_stats,999) 'PSurf(Pa)', & 
           vmean,vstdev,vmin,vmax
      call stats(noah%forcing(3),ld%d%udef,ld%d%glbnch,vmean,vstdev, & 
           vmin, vmax)
      write(ftn_stats,999) 'SWdown (W/m2)', & 
           vmean,vstdev,vmin,vmax
      call stats(noah%forcing(4),ld%d%udef,ld%d%glbnch,vmean,vstdev, & 
           vmin, vmax)
      write(ftn_stats,999) 'LWdown(W/m2)', & 
           vmean,vstdev,vmin,vmax
   endif
998    FORMAT(1X,A18,4E14.3)
999    FORMAT(1X,A18,4F14.3)
!EOC
 end subroutine noah_writestats
 
