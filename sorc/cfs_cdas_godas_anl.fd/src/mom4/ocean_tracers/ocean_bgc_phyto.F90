
!<OVERVIEW>
! Ocean Carbon Model Intercomparison Study II: Biotic module
! NOTE: This module is NOT available in the current release
!</OVERVIEW>


module  ocean_bgc_phyto_mod 
use ocean_types_mod,    only: ocean_thickness_type
logical, public :: do_ocean_bgc_phyto
public  :: ocean_bgc_phyto_bbc
public  :: ocean_bgc_phyto_end
public  :: ocean_bgc_phyto_init
public  :: ocean_bgc_phyto_sbc
public  :: ocean_bgc_phyto_source
public  :: ocean_bgc_phyto_start
public  :: ocean_bgc_phyto_tracer
contains

subroutine ocean_bgc_phyto_bbc 
return
end subroutine  ocean_bgc_phyto_bbc 

subroutine ocean_bgc_phyto_end(Thickness)  
type(ocean_thickness_type), intent(in) :: Thickness
return
end subroutine  ocean_bgc_phyto_end 

subroutine ocean_bgc_phyto_sbc(robert) 
real, intent(in)        :: robert 
return
end subroutine  ocean_bgc_phyto_sbc 

subroutine ocean_bgc_phyto_init
return
end subroutine ocean_bgc_phyto_init 

subroutine ocean_bgc_phyto_source(Thickness) 
type(ocean_thickness_type), intent(in) :: Thickness
return
end subroutine  ocean_bgc_phyto_source 

subroutine ocean_bgc_phyto_start  
return
end subroutine  ocean_bgc_phyto_start  

subroutine ocean_bgc_phyto_tracer 
return
end subroutine  ocean_bgc_phyto_tracer 

end module  ocean_bgc_phyto_mod 
