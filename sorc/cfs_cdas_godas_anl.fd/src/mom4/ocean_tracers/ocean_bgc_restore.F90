!
!<OVERVIEW>
! Ocean Carbon Model Intercomparison Study II: Biotic module
! NOTE: This module is NOT available in the current release
!</OVERVIEW>
module  ocean_bgc_restore_mod  !{
use ocean_types_mod,    only: ocean_thickness_type
public  :: ocean_bgc_restore_bbc
public  :: ocean_bgc_restore_end
public  :: ocean_bgc_restore_init
public  :: ocean_bgc_restore_sbc
public  :: ocean_bgc_restore_source
public  :: ocean_bgc_restore_start
public  :: ocean_bgc_restore_tracer
logical, public :: do_ocean_bgc_restore
contains

subroutine ocean_bgc_restore_bbc  
return
end subroutine  ocean_bgc_restore_bbc  !}

subroutine ocean_bgc_restore_end(Thickness)  !{ 
type(ocean_thickness_type), intent(in) :: Thickness
return
end subroutine  ocean_bgc_restore_end  !}

subroutine ocean_bgc_restore_sbc(robert)  !{
real, intent(in)        :: robert     
return
end subroutine  ocean_bgc_restore_sbc  !}

subroutine ocean_bgc_restore_init  !{
return
end subroutine ocean_bgc_restore_init  !}

subroutine ocean_bgc_restore_source(Thickness)  !{
type(ocean_thickness_type), intent(in) :: Thickness
return
end subroutine  ocean_bgc_restore_source  !}

subroutine ocean_bgc_restore_start  !{
return
end subroutine  ocean_bgc_restore_start  

subroutine ocean_bgc_restore_tracer  !{
return
end subroutine  ocean_bgc_restore_tracer  !}

end module  ocean_bgc_restore_mod  !}
