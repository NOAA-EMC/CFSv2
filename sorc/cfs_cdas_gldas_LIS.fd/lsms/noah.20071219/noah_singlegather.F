!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
#include <misc.h>
!BOP
!
! !ROUTINE: noah_singlegather.F90
!
! !DESCRIPTION:
!  Gather single variable for output 
!
! !REVISION HISTORY:
! Apr 2003 ; Sujay Kumar, Initial Code
!
! !INTERFACE:
subroutine noah_singlegather(index, var)
!BOP
!
! !USES:
  use lisdrv_module, only : lis
  use tile_spmdMod
  use noah_varder
  use noahpardef_module
  IMPLICIT NONE
! !ARGUMENTS:
  integer :: index              ! Index of Noah variable
  real    :: var(lis%d%glbnch) ! Noah variable being gathered
!EOP
  real :: var_temp(di_array(iam))
  integer :: t
  integer ierr
!BOC
  do t = 1, di_array(iam)
     select case (index)
     case(1)
        var_temp(t) = noah(t)%swnet/float(noah(t)%count)
     case(2)
        var_temp(t) = (-1)*noah(t)%lwnet/float(noah(t)%count)
     case(3)
        var_temp(t) = noah(t)%qle/float(noah(t)%count)
     case(4)
        var_temp(t) = noah(t)%qh/float(noah(t)%count)
     case(5)
        var_temp(t) = noah(t)%qg/float(noah(t)%count)
     case(6)
        var_temp(t) = noah(t)%snowf/float(noah(t)%count)
     case(7)
        var_temp(t) = noah(t)%rainf/float(noah(t)%count)
     case(8)
        var_temp(t) = noah(t)%evap/float(noah(t)%count)         
     case(9)
        var_temp(t) = noah(t)%qs/float(noah(t)%count)
     case(10)
        var_temp(t) = noah(t)%qsb/float(noah(t)%count)
     case(11)
        var_temp(t) = noah(t)%qsm/float(noah(t)%count)
     case(12)
        var_temp(t) = noah(t)%smc(1)*1000.0*0.1+ & 
             noah(t)%smc(2)*1000.0*0.3 + & 
             noah(t)%smc(3)*1000.0*0.6 + & 
             noah(t)%smc(4)*1000.0 - noah(t)%soilm_prev
     case(13) 
        var_temp(t) = noah(t)%sneqv*1000.0-noah(t)%swe_prev
     case(14)
        var_temp(t) = noah(t)%avgsurft
     case(15)
        var_temp(t) = noah(t)%albedo
     case(16)
        var_temp(t) = noah(t)%swe/float(noah(t)%count)
     case(17)
        var_temp(t) = noah(t)%soilmoist1/float(noah(t)%count)
     case(18)
        var_temp(t) = noah(t)%soilmoist2/float(noah(t)%count)
     case(19)
        var_temp(t) = noah(t)%soilmoist3/float(noah(t)%count)
     case(20)
        var_temp(t) = noah(t)%soilmoist4/float(noah(t)%count)
     case(21)
        var_temp(t) = noah(t)%soilwet/float(noah(t)%count)
     case(22)
        var_temp(t) = noah(t)%tveg/float(noah(t)%count)
     case(23)
        var_temp(t) = noah(t)%esoil/float(noah(t)%count)
     case(24)
        var_temp(t) = noah(t)%rootmoist/float(noah(t)%count)
     case(25) 
        if(lis%o%wfor.eq.1) then
           var_temp(t) = sqrt(noah(t)%forcing(5)*noah(t)%forcing(5)+ & 
                noah(t)%forcing(6)*noah(t)%forcing(6))
        endif
     case(26) 
        if(lis%o%wfor.eq.1) then
           if(noah(t)%forcing(1) < 273.15) then 
              var_temp(t) = 0.0
           else 
              var_temp(t) = noah(t)%forcing(8)
           endif
        endif
     case(27) 
        if(lis%o%wfor.eq.1) then
           if(noah(t)%forcing(1) < 273.15) then 
              var_temp(t) = noah(t)%forcing(8)
           else 
              var_temp(t) = 0.0
           endif
        endif
     case(28) 
        if(lis%o%wfor.eq.1) then
           var_temp(t) = noah(t)%forcing(1)
        endif
     case(29) 
        if(lis%o%wfor.eq.1) then
           var_temp(t) = noah(t)%forcing(2)
        endif
     case(30) 
        if(lis%o%wfor.eq.1) then
           var_temp(t) = noah(t)%forcing(7)
        endif
     case(31) 
        if(lis%o%wfor.eq.1) then
           var_temp(t) = noah(t)%forcing(3)
        endif
     case(32) 
        if(lis%o%wfor.eq.1) then
           var_temp(t) = noah(t)%forcing(4)
        endif
     end select
  enddo
  
  call MPI_GATHERV(var_temp(1:di_array(iam)), &
       di_array(iam), & 
       MPI_REAL,var,di_array,displs,MPI_REAL, & 
       0,MPI_COMM_WORLD, ierr)
!EOC  
end subroutine noah_singlegather
