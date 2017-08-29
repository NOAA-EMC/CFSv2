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
subroutine noah_gribout(ld,ftn)
! !USES:
  use lisdrv_module, only : gindex
  use lis_module
  use drv_output_mod, only : drv_writevar_grib
  use noah_varder
  use time_manager, only : tick
 
  implicit none
  
  type(lisdec) :: ld
  integer :: ftn
!EOP
  real :: vmean,vstdev,vmin,vmax
  real :: rainf(ld%d%glbnch)
  real :: snowf(ld%d%glbnch)
  integer :: t,c,r,i,k
  logical*1 :: lismask(ld%d%lnc,ld%d%lnr)
  character*8 :: today, yesterday
  character*1 :: tod(8), yes(8)
  character(len=100) :: temp1
  real*8 :: dummytime   
  real  :: dummygmt,dumcount
  integer:: ss1,ts,mn1,hr1,da1,mo1,yr1,ts1,doy1
  integer :: kpds(25)
  real :: interval
  real :: undef(ld%d%lnc,ld%d%lnr)
!BOC
  
  print*,'MSG: noah_gribout()'
  undef = 9.999E+20
  interval = noahdrv%writeintn
  dumcount=interval*3600./float(ld%t%TS)
  hr1=ld%t%hr
  da1=ld%t%da
  mo1=ld%t%mo
  yr1=ld%t%yr
  mn1=ld%t%mn
  ss1=0
  ts1=-3600*24
  dummygmt=1.0
  dummytime=1.0
  write(unit=temp1,fmt='(i4,i2,i2)')yr1,mo1,da1
  read(unit=temp1,fmt='(8a1)')tod
  do i=1,8
     if(tod(i).eq.(' '))tod(i)='0'
  enddo
  today=tod(1)//tod(2)//tod(3)//tod(4)//tod(5) &
       //tod(6)//tod(7)//tod(8)
  
  call tick(dummytime,doy1,dummygmt,yr1,mo1,da1,hr1,mn1,ss1,ts1)
  write(unit=temp1,fmt='(i4,i2,i2)')yr1,mo1,da1
  read(unit=temp1,fmt='(8a1)')yes
  do i=1,8
     if(yes(i).eq.(' '))yes(i)='0'
  enddo
  yesterday=yes(1)//yes(2)//yes(3)//yes(4)//yes(5) &
       //yes(6)//yes(7)//yes(8)
  do i=1,25
     kpds(i)=0
  enddo
  kpds(1)=7                 !NCEP
  kpds(2)=141               !LDAS
  kpds(3)=255               !T382 is undefined yet.
  kpds(4)=192               !bms flag... 192 = bitmap included
  kpds(12)=0                !assume output time minute always = 0
  kpds(13)=1                !forecast time unit (hours)
  kpds(17)=int((noahdrv%writeintn*3600.0)/ld%t%ts) !number of time steps in
  !averaged/accum variables
  kpds(18)=0                !grib version -- left as 0 in ncep products
  kpds(19)=130              !version number of kpds.tbl for ldas.  
  kpds(20)=0                !none missing from averages/accumulations (always4)
  kpds(23)=4                !EMC
  kpds(24)=0                !does not apply to ldas output
  kpds(25)=0   

!JESSE 20071219 MOVE KPDS_completenoah.tbl TO ./FIX
!  open (unit = 69, file = './src/tables/KPDS_completenoah.tbl')
  open (unit = 69, file = './FIX/KPDS_completenoah.tbl')
  do k = 1, 42
     read(69,*)
  end do
  
  do c=1,ld%d%lnc
     do r=1,ld%d%lnr
        if(gindex(c,r).gt.0) then
           lismask(c,r)=.true.
        else
           lismask(c,r)=.false.
        endif
     enddo
  enddo

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
   call readkpds(69,kpds)
 !  noah%swnet = noah%swnet/float(KPDS(17))
 !  call drv_writevar_grib(ftn,noah%swnet,kpds,lismask,interval,today,yesterday) 

   call readkpds(69,kpds)   
 !  noah%lwnet = (-1.)*noah%lwnet/float(KPDS(17))
 !  call drv_writevar_grib(ftn,noah%lwnet,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
   noah%qle = noah%qle/float(KPDS(17))
   call drv_writevar_grib(ftn,noah%qle,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
 !  noah%qh = noah%qh/float(KPDS(17))
 !  call drv_writevar_grib(ftn,noah%qh,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
 !  noah%qg = noah%qg/float(KPDS(17))
 !  call drv_writevar_grib(ftn,noah%qg,kpds,lismask,interval,today,yesterday)
!---------------------------------------------------------------------------
! General Water Balance Components
!---------------------------------------------------------------------------
   call readkpds(69,kpds)   
   noah%snowf = noah%snowf/float(KPDS(17))
 !  call drv_writevar_grib(ftn,noah%snowf,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
   noah%rainf = noah%rainf/float(KPDS(17))
 !  call drv_writevar_grib(ftn,noah%rainf,kpds,lismask,interval,today,yesterday)
   call drv_writevar_grib(ftn,noah%rainf+noah%snowf,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
   noah%evap = noah%evap/float(KPDS(17))
   call drv_writevar_grib(ftn,noah%evap*3600.*interval,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
   noah%qs = noah%qs/float(KPDS(17))
   call drv_writevar_grib(ftn,noah%qs*3600.*interval,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
   noah%qsb = noah%qsb/float(KPDS(17))
   call drv_writevar_grib(ftn,noah%qsb*3600.*interval,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
   noah%qsm = noah%qsm/float(KPDS(17))
 !  call drv_writevar_grib(ftn,noah%qsm,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
 !  call drv_writevar_grib(ftn,noah%smc(1)*1000.0*0.1+ &
 !       noah%smc(2)*1000.0*0.3 + & 
 !       noah%smc(3)*1000.0*0.6 + & 
 !       noah%smc(4)*1000.0 -noah%soilm_prev,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
 !  call drv_writevar_grib(ftn,noah%swe-noah%swe_prev,kpds,lismask,interval,today,yesterday)
!---------------------------------------------------------------------------
! Surface State Variables
!---------------------------------------------------------------------------
   call readkpds(69,kpds)   
 !  call drv_writevar_grib(ftn,noah%avgsurft,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
 !  call drv_writevar_grib(ftn,noah%albedo,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
! JESSE 20071219 SWE is instantaneous
!   noah%swe= noah%swe/float(KPDS(17))
   call drv_writevar_grib(ftn,noah%swe,kpds,lismask,interval,today,yesterday)

 close(69)
 return

!---------------------------------------------------------------------------
! Subsurface State Variables
!---------------------------------------------------------------------------
   call readkpds(69,kpds)   
   noah%soilmoist1= noah%soilmoist1/float(KPDS(17))
   call drv_writevar_grib(ftn,noah%soilmoist1,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
   noah%soilmoist2= noah%soilmoist2/float(KPDS(17))
   call drv_writevar_grib(ftn,noah%soilmoist2,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
   noah%soilmoist3= noah%soilmoist3/float(KPDS(17))
   call drv_writevar_grib(ftn,noah%soilmoist3,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
   noah%soilmoist4= noah%soilmoist4/float(KPDS(17))
   call drv_writevar_grib(ftn,noah%soilmoist4,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
   noah%soilwet= noah%soilwet/float(KPDS(17))
   call drv_writevar_grib(ftn,noah%soilwet,kpds,lismask,interval,today,yesterday)
!---------------------------------------------------------------------------
! Evaporation Components
!---------------------------------------------------------------------------
   call readkpds(69,kpds)   
   noah%tveg= noah%tveg/float(KPDS(17))
   call drv_writevar_grib(ftn,noah%tveg,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
   noah%esoil= noah%esoil/float(KPDS(17))
   call drv_writevar_grib(ftn,noah%esoil,kpds,lismask,interval,today,yesterday)

   call readkpds(69,kpds)   
   noah%rootmoist = noah%rootmoist/float(KPDS(17))
   call drv_writevar_grib(ftn, noah%rootmoist,kpds,lismask,interval,today,yesterday)
   print*,'Wrote NOAH parameters ',ld%t%TS,interval,ld%o%wfor
!---------------------------------------------------------------------------
! Forcing
!---------------------------------------------------------------------------
   if(ld%o%wfor.eq.1) then
      print*,'Writing FORCING data in GRIB'
      call readkpds(69,kpds)   
      call drv_writevar_grib(ftn, sqrt(noah%forcing(5)*noah%forcing(5)+ & 
           noah%forcing(6)*noah%forcing(6)),kpds,lismask,interval,today,yesterday)

      call readkpds(69,kpds)   
      call drv_writevar_grib(ftn,rainf,kpds,lismask,interval,today,yesterday)

      call readkpds(69,kpds)   
      call drv_writevar_grib(ftn,snowf,kpds,lismask,interval,today,yesterday)

      call readkpds(69,kpds)   
      call drv_writevar_grib(ftn,noah%forcing(1),kpds,lismask,interval,today,yesterday)

      call readkpds(69,kpds)   
      call drv_writevar_grib(ftn,noah%forcing(2),kpds,lismask,interval,today,yesterday)

      call readkpds(69,kpds)   
      call drv_writevar_grib(ftn,noah%forcing(7),kpds,lismask,interval,today,yesterday)

      call readkpds(69,kpds)   
      call drv_writevar_grib(ftn,noah%forcing(3),kpds,lismask,interval,today,yesterday)

      call readkpds(69,kpds)   
      call drv_writevar_grib(ftn,noah%forcing(4),kpds,lismask,interval,today,yesterday)
   endif
   close(69)

!EOC
 end subroutine noah_gribout
 
