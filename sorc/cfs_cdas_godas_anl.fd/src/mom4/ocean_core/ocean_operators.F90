module ocean_operators_mod
! 
  
!<CONTACT EMAIL="Ronald.Pacanowski@noaa.gov"> R.C. Pacanowski
!</CONTACT>
!
!<REVIEWER EMAIL="Tony.Rosati@noaa.gov"> A. Rosati 
!</REVIEWER>
!
!<REVIEWER EMAIL="Stephen.Griffies@noaa.gov"> S.M. Griffies
!</REVIEWER>
!
!<REVIEWER EMAIL="Zhi.Liang@noaa.gov"> Zhi Liang 
!</REVIEWER>
!
!<OVERVIEW>
! Operators for MOM4
!</OVERVIEW>
!
!<DESCRIPTION>
! This module computes finite difference operators that are of 
! use throughout MOM4.  
!</DESCRIPTION>
!
! <INFO>
!
!<NOTE>
! All operators will be replaced by generic forms when Fortran
! can properly support functions of allocatable arrays
!
! The problems are are as follows:
! Allocatable arrays cannot be inside of derived types.
! Only pointers to allocatable arrays can be inside derived types.
! Supposedly the former will be allowed in Fortran 95
! Also, functions cannot be typed as a derived type without conflicts
! which preclude using function as general operators operating on
! derived types.
! </NOTE>
!
! <NOTE>
! Mnemonics for simple operators
!
! 1st letter (direction of operation)
! <BR/>
! F => Forward direction with respect to the index.
! <BR/>
! B => Backward direction with respect to the index.
!
! 2nd letter (operation)
! <BR/>
! D => Derivative
! <BR/> 
! A => Average
! <BR/>
! M => Minimum
!
! 3rd letter (axis)
!
! X => along the X axis
! <BR/>
! Y => along the Y axis
! <BR/>
! Z => along the Z axis
!
! 4th letter (placement of quantity being operated on)
!
! E => East face
! <BR/>
! N => North face
! <BR/>
! B => Bottom face
! <BR/>
! P => Point (grid point within cell)
!
! 5th letter (type of grid cell)
!
! U => U-cell
! <BR/>
! T => T-cell
! </NOTE>
!
! </INFO>
  
  use constants_mod,     only: rho0r 
  use mpp_domains_mod,   only: mpp_update_domains, CGRID_NE
  use mpp_mod,           only: mpp_error, FATAL, stdlog, stdout
  use ocean_domains_mod, only: get_local_indices, get_halo_sizes    
  use ocean_domains_mod, only: set_ocean_domain
  use ocean_types_mod,   only: ocean_grid_type, ocean_domain_type, ocean_thickness_type 

  implicit none

  private

  public FAX, FAY ! Forward averaging operators
  public BAX, BAY ! Backward averaging operators
  public FMX, FMY ! Forward minimum operators

  public  FDZ_PT           ! Forward derivative operator 
  private FDX_ZTP, FDY_ZTP ! Forward derivative operators

  public FDX_PU, FDY_PU    ! Forward derivative operators 
  public FDX_PT, FDY_PT    ! Forward derivative operators
  public FDX_NT, FDY_ET    ! Forward derivative operators

  public BDX_ET, BDY_NT    ! Backward derivative operators
  public BDX_EU, BDY_NU    ! Backward derivative operators

  public FDX_PT_flat, FDY_PT_flat ! Forward derivative operators for fields at same depth 

  ! Not-so-simple operators (no mnemonics) 
  public REMAP_NT_TO_NU    ! Conservatively remaps thickness weighted advective velocity on north face of T-cells to U-cells 
  public REMAP_ET_TO_EU    ! Conservatively remaps thickness weighted advective velocity on east face of T-cells to U-cells 
  public REMAP_BT_TO_BU    ! Conservatively remaps T-cell thickness or vertical velocity on base of T-cells to U-cells
  public REMAP_T_TO_V_NOCONSERVE    ! Non-conservatively remap from a tracer point to a velocity point  
  public DIV_UD            ! Divergence of thickness weighted barotropic velocity
  public GRAD_SURF_P       ! rho0r*Gradient of surface pressure
  public S2D               ! Smooth with 2D version of [1/4, 1/2, 1/4] filter
  public LAP_T             ! Horizontal Laplacian of T-cell fields living at same depth, weighted by a diffusivity 

  character (len=128) :: version = &
       '$Id: ocean_operators.F90,v 1.4 2004/12/10 19:35:16 gtn Exp $'

  character (len=128) :: tagname = &
       '$Name: mom4p0d $'

  type(ocean_grid_type), pointer      :: Grd =>NULL()
  type(ocean_domain_type), pointer    :: Dom =>NULL()
  type(ocean_thickness_type), pointer :: Thk =>NULL()

  type(ocean_domain_type), save       :: Dom_flux

#include <ocean_memory.h>
#ifdef STATIC_MEMORY
  real, dimension(isd:ied,jsd:jed,nk) :: fmx_tmask
  real, dimension(isd:ied,jsd:jed,nk) :: fmy_tmask
  integer, parameter                  :: halo = 1
#else
  real, dimension(:,:,:), allocatable :: fmx_tmask
  real, dimension(:,:,:), allocatable :: fmy_tmask
  integer                             :: halo
#endif

  logical :: module_is_initialized = .FALSE.

  public ocean_operators_init

contains


!#######################################################################
! <SUBROUTINE NAME="ocean_operators_init">
!
! <DESCRIPTION>
! Initialize the operator module
! </DESCRIPTION>
!
subroutine ocean_operators_init(Grid, Domain, Thickness)

    type(ocean_grid_type), intent(in), target      :: Grid
    type(ocean_domain_type), intent(in), target    :: Domain
    type(ocean_thickness_type), intent(in), target :: Thickness

    integer :: xhalo
    integer :: yhalo
    integer :: k

    Grd => Grid
    Dom => Domain
    Thk => Thickness

    call set_ocean_domain(Dom_flux, Grid, name='horz diff flux')

#ifndef STATIC_MEMORY
    call get_local_indices(Domain, isd, ied, jsd, jed, isc, iec, jsc, jec)
    call get_halo_sizes(Domain,xhalo, yhalo)
    if (xhalo /= yhalo) then 
      call mpp_error(FATAL,'==>Error in ocean_operators_mod (ocean_operators_init): with static memory, xhalo must equal yhalo')
    endif 
    nk = Grd%nk
    allocate(fmx_tmask(isd:ied,jsd:jed,nk))
    allocate(fmy_tmask(isd:ied,jsd:jed,nk))
    halo = xhalo
#endif
    do k=1,nk
       fmx_tmask(:,:,k) = FMX(Grd%tmask(:,:,k))
       fmy_tmask(:,:,k) = FMY(Grd%tmask(:,:,k))
    enddo

    write( stdlog(),'(/a/)') trim(version)

    return

  end subroutine ocean_operators_init
! </SUBROUTINE> NAME="ocean_operators_init"


!#######################################################################
! <FUNCTION NAME="REMAP_NT_TO_NU">
!
! <DESCRIPTION>
! REMAP_NT_TO_NU conservatively remaps a normal flux at the north 
! face of T-cells to the north face of U-cells
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be remapped 
! </IN>
!
function REMAP_NT_TO_NU(a) 
    real, intent(in), dimension(isd:ied,jsd:jed) :: a
    real, dimension(isd:ied,jsd:jed) :: REMAP_NT_TO_NU
    integer :: i, j

    do j=jsc-halo,jec+halo-1
       do i=isc-halo,iec+halo-1
          REMAP_NT_TO_NU(i,j) = ((a(i,j)*Grd%duw(i,j) + a(i+1,j)*Grd%due(i,j))*Grd%dus(i,j+1) +&
               ((a(i,j+1)*Grd%duw(i,j+1) + a(i+1,j+1)*Grd%due(i,j+1)))*Grd%dun(i,j))*Grd%dater(i,j+1)
       enddo
    enddo
    REMAP_NT_TO_NU(iec+halo,:) = 0.0
    REMAP_NT_TO_NU(:,jec+halo) = 0.0

end function REMAP_NT_TO_NU
! </FUNCTION> NAME="REMAP_NT_TO_NU"


!#######################################################################
! <FUNCTION NAME="REMAP_ET_TO_EU">
!
! <DESCRIPTION>
! REMAP_ET_TO_EU conservatively remaps a normal flux at the east 
! face of T-cells to the east face of U-cells
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be remapped 
! </IN>
!
function REMAP_ET_TO_EU(a) 

    real, intent(in), dimension(isd:ied,jsd:jed) :: a
    real, dimension(isd:ied,jsd:jed) :: REMAP_ET_TO_EU
    integer :: i, j

    do j=jsc-halo,jec+halo-1
       do i=isc-halo,iec+halo-1
          REMAP_ET_TO_EU(i,j) = ((a(i,j)*Grd%dus(i,j) + a(i,j+1)*Grd%dun(i,j))*Grd%duw(i+1,j) +&
               ((a(i+1,j)*Grd%dus(i+1,j) + a(i+1,j+1)*Grd%dun(i+1,j)))*Grd%due(i,j))*Grd%datnr(i+1,j)
       enddo
    enddo
    REMAP_ET_TO_EU(iec+halo,:) = 0.0
    REMAP_ET_TO_EU(:,jec+halo) = 0.0

end function REMAP_ET_TO_EU
! </FUNCTION> NAME="REMAP_ET_TO_EU"


!#######################################################################
! <FUNCTION NAME="REMAP_BT_TO_BU">
!
! <DESCRIPTION>
! REMAP_BT_TO_BU conservatively remaps a T-cell thickness or 
! vertical velocity on the base of T-cells to U-cells
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be remapped 
! </IN>
function REMAP_BT_TO_BU(a) 

    real, intent(in), dimension(isd:ied,jsd:jed) :: a
    real, dimension(isd:ied,jsd:jed) :: REMAP_BT_TO_BU
    integer :: i, j

    do j=jsc-halo,jec+halo-1
       do i=isc-halo,iec+halo-1
          REMAP_BT_TO_BU(i,j) = (a(i,j)*Grd%dte(i,j)*Grd%dus(i,j) + a(i+1,j)*Grd%dtw(i+1,j)*Grd%dus(i,j)&
               + a(i,j+1)*Grd%dte(i,j+1)*Grd%dun(i,j) + a(i+1,j+1)*Grd%dtw(i+1,j+1)*Grd%dun(i,j))*Grd%daur(i,j)
       enddo
    enddo
    REMAP_BT_TO_BU(iec+halo,:) = 0.0
    REMAP_BT_TO_BU(:,jec+halo) = 0.0

end function REMAP_BT_TO_BU
! </FUNCTION> NAME="REMAP_BT_TO_BU"


!#######################################################################
! <FUNCTION NAME="DIV_UD">
!
! <DESCRIPTION>
! Compute divergence of vertically integrated velocity.  Following is 
! a speedier version of
! <BR/>
! uhy(:,:) = BAY(ud(:,:,1)*dyu(:,:))
! <BR/>
! vhx(:,:) = BAX(ud(:,:,2)*dxu(:,:))
! <BR/>
! DIV_UD(ud) = BDX_ET(uhy(:,:)/dyte(:,:)) + BDY_NT(vhx(:,:)/dxtn(:,:))
! </DESCRIPTION>
!
! <IN NAME="ud" UNITS="m^2/sec" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Vertically integrated horizontal velocity field 
! </IN>
!
function DIV_UD (ud)

    real, intent(in), dimension(isd:ied,jsd:jed,2) :: ud
    real, dimension(isd:ied,jsd:jed) :: DIV_UD
    integer :: i, j
    real :: uh, uhim, uhjm, uhimjm, vh, vhim, vhjm, vhimjm
    real :: uh_bay, vh_bax, uhim_bay, vhjm_bax


    DIV_UD = 0.d0
    do j=jsc,jec+halo
       i = isc
       uhim = ud(i-1,j,1)*Grd%dyu(i-1,j)
       vhim = ud(i-1,j,2)*Grd%dxu(i-1,j)
       uhimjm = ud(i-1,j-1,1)*Grd%dyu(i-1,j-1)
       vhimjm = ud(i-1,j-1,2)*Grd%dxu(i-1,j-1)
       uhim_bay = 0.5*(uhim+uhimjm)
       do i=isc,iec+halo
          uh  = ud(i,j,1)*Grd%dyu(i,j)
          vh  = ud(i,j,2)*Grd%dxu(i,j)

          uhjm = ud(i,j-1,1)*Grd%dyu(i,j-1)
          vhjm = ud(i,j-1,2)*Grd%dxu(i,j-1)

          uh_bay = 0.5*(uh+uhjm)
          vh_bax = 0.5*(vh+vhim)

          vhjm_bax = 0.5*(vhjm+vhimjm)

          DIV_UD(i,j) = (uh_bay - uhim_bay + vh_bax - vhjm_bax)*Grd%datr(i,j)

          uhim = uh
          vhim = vh
          uhimjm = uhjm
          vhimjm = vhjm
          uhim_bay = uh_bay
       enddo
    enddo
    DIV_UD(:,:jsc-1) = 0.0
    DIV_UD(:isc-1,:) = 0.0

end function DIV_UD
! </FUNCTION> NAME="DIV_UD"


!#######################################################################
! <FUNCTION NAME="GRAD_SURF_P">
!
! <DESCRIPTION>
! Compute horizontal gradient of the pressure field associated with 
! the free surface height. The algorithm is a speedier version of
! <BR/>
! grad_ps(:,:,1) = rho0r*FDX_NT(FAY(ps(:,:)))
! <BR/>
! grad_ps(:,:,2) = rho0r*FDY_ET(FAX(ps(:,:)))
! </DESCRIPTION>
!
! <IN NAME="ps" UNITS="kg/m/s^2" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Pressure field associated with the free surface height
! </IN>
!
function GRAD_SURF_P (ps)

    real, intent(in), dimension(isd:ied,jsd:jed) :: ps
    real, dimension(isd:ied,jsd:jed,2) :: GRAD_SURF_P
    integer :: i, j
    real :: p_fay, p_fax, pip_fay, pjp_fax

    do j=jsc-halo,jec
       do i=isc-halo,iec
          p_fay = 0.5*(ps(i,j) + ps(i,j+1))
          p_fax = 0.5*(ps(i,j) + ps(i+1,j))
          pip_fay = 0.5*(ps(i+1,j) + ps(i+1,j+1))
          pjp_fax = 0.5*(ps(i,j+1) + ps(i+1,j+1))

          GRAD_SURF_P(i,j,1) = rho0r*(pip_fay - p_fay)*Grd%dxur(i,j)*Grd%umask(i,j,1)
          GRAD_SURF_P(i,j,2) = rho0r*(pjp_fax - p_fax)*Grd%dyur(i,j)*Grd%umask(i,j,1)
       enddo
    enddo
    GRAD_SURF_P(iec+1:,:,:) = 0.0
    GRAD_SURF_P(:,jec+1:,:) = 0.0

end function GRAD_SURF_P
! </FUNCTION> NAME="GRAD_SURF_P"


!#######################################################################
! <FUNCTION NAME="S2D">
!
! <DESCRIPTION>
! Smooth a 2D field with a 2D version of a 1D filter with weights (1/4, 1/2, 1/4)
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be smoothed 
! </IN>
!
function S2D(a) 

    real, intent(in), dimension(isd:ied,jsd:jed) :: a
    real, dimension(isd:ied,jsd:jed) :: S2D
    real, dimension(-1:1,-1:1) :: fir
    integer :: i, j, iq, jq

    fir(-1,-1) = 1.0/16.0
    fir(0,-1)  = 1.0/8.0
    fir(1,-1)  = 1.0/16.0
    fir(-1,0)  = 1.0/8.0
    fir(0,0)   = 1.0/4.0
    fir(1,0)   = 1.0/8.0
    fir(-1,1)  = 1.0/16.0
    fir(0,1)   = 1.0/8.0
    fir(1,1)   = 1.0/16.0         
    do j=jsc-halo+1,jec+halo-1
       do i=isc-halo+1,iec+halo-1
          S2D(i,j) = 0.0
          do jq=-1,1
             do iq=-1,1
                S2D(i,j) = S2D(i,j) + fir(iq,jq)*a(i+iq,j+jq)
             enddo
          enddo
       enddo
    enddo
    S2D(iec+halo,:) = 0.0
    S2D(isc-halo,:) = 0.0
    S2D(:,jec+halo) = 0.0
    S2D(:,jsc-halo) = 0.0

end function S2D
! </FUNCTION> NAME="S2D"


!#######################################################################
! <FUNCTION NAME="LAP_T">
!
! <DESCRIPTION>
!  Compute horizontal 5-point Laplacian operator on eta_t.
!  Result lives at T-cell center. 
!
!  Redundancy update for tripolar is needed to conserve  
!  total volume and tracer.  It is likely unimportant 
!  when call LAP_T from within the barotropic loop. Yet it 
!  is essential when call LAP_T from ocean_surface_smooth.
! </DESCRIPTION>
!
function LAP_T (a, mix, update)

    real, intent(in),    dimension(isd:ied,jsd:jed) :: a
    real, intent(in),    dimension(isd:ied,jsd:jed) :: mix
    logical, intent(in),                   optional :: update

    real, dimension(isd:ied,jsd:jed) :: fx, fy, LAP_T
    real                             :: chg

    integer :: i, j, k
    logical :: update_domains

       k = 1

    if (PRESENT(update)) then 
       update_domains = update
    else 
       update_domains = .false.
    endif  

    do j = jsc-halo, jec+halo
       do i = isc-halo,iec+halo-1
          fx(i,j) = mix(i,j)*((a(i+1,j)-a(i,j))*Grd%dxter(i,j))*fmx_tmask(i,j,k)
       enddo
    enddo
    fx(iec+halo,:) = 0.0

    do j = jsc-halo, jec+halo-1
       do i = isc-halo,iec+halo
          fy(i,j) = mix(i,j)*((a(i,j+1)-a(i,j))*Grd%dytnr(i,j))*fmy_tmask(i,j,k)
       enddo
    enddo
    fy(:,jec+halo) = 0.0

    if(Grd%tripolar .and. update_domains) then
          call mpp_update_domains(fx, fy, Dom_flux%domain2d, gridtype=CGRID_NE)
    endif 

    LAP_T(:,:) = 0.0

    do j=jsc,jec
       do i=isc,iec
          chg        = (Grd%dyte(i,j)*fx(i,j) - Grd%dyte(i-1,j)*fx(i-1,j))*Grd%datr(i,j) &
                     + (Grd%dxtn(i,j)*fy(i,j) - Grd%dxtn(i,j-1)*fy(i,j-1))*Grd%datr(i,j)
          LAP_T(i,j) = Grd%tmask(i,j,k)*chg
       enddo
    enddo
 
end function LAP_T
! </FUNCTION> NAME="LAP_T_test">


!#######################################################################
! <FUNCTION NAME="FAX">
!
! <DESCRIPTION>
! Forwards average in the i-direction on the X-axis.
! If input is a(i,j) then output is defined at (i+1/2,j)
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be averaged  
! </IN>
function FAX(a) 

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: FAX
  integer :: i, j

  do j=jsc-halo,jec+halo
     do i=isc-halo,iec+halo-1
        FAX(i,j) = 0.5*(a(i,j) + a(i+1,j))
     enddo
     FAX(iec+halo,j) = 0.0
  enddo
end function FAX
! </FUNCTION> NAME="FAX"


!#######################################################################
! <FUNCTION NAME="BAX">
!
! <DESCRIPTION>
! Backwards average in the i-direction along the X-axis.
! If input is a(i,j) then output is defined at (i-1/2,j)
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be averaged  
! </IN>
!
function BAX(a) 

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: BAX
  integer :: i, j

  do j=jsc-halo,jec+halo
     do i=isc-halo+1,iec+halo
        BAX(i,j) = 0.5*(a(i,j) + a(i-1,j))
     enddo
     BAX(isc-halo,j) = 0.0
  enddo
end function BAX
! </FUNCTION> NAME="BAX"


!#######################################################################
! <FUNCTION NAME="FAY">
!
! <DESCRIPTION>
! Forwards average in the j-direction on the Y-axis
! If input is a(i,j) then output is defined at (i,j+1/2)
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be averaged  
! </IN>
function FAY(a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: FAY
  integer :: i, j

  do j=jsc-halo,jec+halo-1
     do i=isc-halo,iec+halo
        FAY(i,j) = 0.5*(a(i,j) + a(i,j+1))
     enddo
  enddo
  FAY(:,jec+halo) = 0.0

end function FAY
! </FUNCTION> NAME="FAY"


!#######################################################################
! <FUNCTION NAME="BAY">
!
! <DESCRIPTION>
! Backwards average in the j-direction along the Y-axis
! If input is a(i,j) then output is defined at (i,j-1/2)
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be averaged  
! </IN>
function BAY(a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: BAY
  integer :: i, j

  do j=jsc-halo+1,jec+halo
     do i=isc-halo,iec+halo
        BAY(i,j) = 0.5*(a(i,j) + a(i,j-1))
     enddo
  enddo
  BAY(:,jsc-halo) = 0.0

end function BAY
! </FUNCTION> NAME="BAY"


!#######################################################################
! <FUNCTION NAME="BDX_EU">
!
! <DESCRIPTION>
! Backwards Derivative in X of a quantity defined on the East face of a U-cell
! If input is a(i,j) then output is defined at (i-1/2,j)
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
function BDX_EU(a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: BDX_EU
  integer :: i, j

  do j=jsc-halo,jec+halo
     do i=isc-halo+1,iec+halo
        BDX_EU(i,j) = (Grd%dyue(i,j)*a(i,j) - Grd%dyue(i-1,j)*a(i-1,j))*Grd%daur(i,j)
     enddo
     BDX_EU(isc-halo,j) = 0.0
  enddo

end function BDX_EU
! </FUNCTION> NAME="BDX_EU"


!#######################################################################
! <FUNCTION NAME="BDX_ET">
!
! <DESCRIPTION>
! Backwards derivative in X of a quantity defined on the East face of a T-cell.
! If input is a(i,j) then output is defined at (i-1/2,j)
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
function BDX_ET(a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: BDX_ET
  integer :: i, j

  do j=jsc-halo,jec+halo
     do i=isc-halo+1,iec+halo
        BDX_ET(i,j) = (Grd%dyte(i,j)*a(i,j) - Grd%dyte(i-1,j)*a(i-1,j))*Grd%datr(i,j)   ! generalized system
     enddo
     BDX_ET(isc-halo,j) = 0.0
  enddo

end function BDX_ET
! </FUNCTION> NAME="BDX_ET"


!#######################################################################
! <FUNCTION NAME="FDX_PU">
!
! <DESCRIPTION>
! Forward Derivative in X of a quantity defined on the grid point of a U-cell.
! If input is a(i,j) then output is defined at (i+1/2,j).
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
function FDX_PU(a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: FDX_PU
  integer :: i, j

  do j=jsc-halo,jec+halo
     do i=isc-halo,iec+halo-1
        FDX_PU(i,j) = (a(i+1,j) - a(i,j))*Grd%dxuer(i,j)
     enddo
     FDX_PU(iec+halo,j) = 0.0
  enddo

end function FDX_PU
! </FUNCTION> NAME="FDX_PU"


!#######################################################################
! <FUNCTION NAME="FDY_ZTP">
!
! <DESCRIPTION>
! Forward Derivative in Y of the depth of the grid Point of a T-cell.
! Input a(i,j) is at the depth of a grid point within T-cell.
! Output is defined at (i,j+1/2) which is at the north face in a T-cell.
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
function FDY_ZTP(a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed)             :: FDY_ZTP
  integer :: i, j

  do j=jsc-halo,jec+halo-1
     do i=isc-halo,iec+halo
        FDY_ZTP(i,j) = (a(i,j+1) - a(i,j))*Grd%dytnr(i,j)   
     enddo
  enddo
  FDY_ZTP(:,jec+halo) = 0.0

end function  FDY_ZTP
! </FUNCTION> NAME="FDY_ZTP"


!#######################################################################
! <FUNCTION NAME="FDX_ZTP">
!
! <DESCRIPTION>
! Forward Derivative in X of the depth of the grid Point of a T-cell.
! Input a(i,j) is at the depth of a grid point within T-cell.
! Output is defined at (i+1/2,j) which is at the east face in a T-cell.
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
function FDX_ZTP(a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed)             :: FDX_ZTP
  integer :: i, j

  do j=jsc-halo,jec+halo
     do i=isc-halo,iec+halo-1
        FDX_ZTP(i,j) = (a(i+1,j) - a(i,j))*Grd%dxter(i,j) 
     enddo
     FDX_ZTP(iec+halo,j) = 0.0
  enddo
end function  FDX_ZTP
! </FUNCTION> NAME="FDX_ZTP"


!#######################################################################
! <FUNCTION NAME="FDX_PT">
!
! <DESCRIPTION>
! Forward Derivative in X of a quantity defined on the grid Point of a T-cell.
! To be used for tracer derivatives when grid points are not all at the same depth. 
! Input a(i,j,1) is at the grid point of a T-cell at level k-1.
! Input a(i,j,2) is at the grid point of a T-cell at level k.
! Output is defined at (i+1/2,j) which is at the east face in a T-cell at level k.
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
! <IN NAME="k" TYPE="integer">
! Depth level 
! </IN>
function FDX_PT(a,k)

  real, intent(in), dimension(isd:ied,jsd:jed,2) :: a
  real, dimension(isd:ied,jsd:jed) :: FDX_PT
  integer, intent(in)  :: k
  integer :: i, j

  do i=isc-halo,iec+halo-1
     FDX_PT(i,:) = (a(i+1,:,2) - a(i,:,2))*Grd%dxter(i,:)
  enddo
  FDX_PT(iec+halo,:) = 0.0

!   expanded form of 
!   FDX_PT(:,:) = FDX_PT(:,:) - FAX(FDZ_PT(a(:,:,1:2),k-1))*FDX_ZTP(Grd%ztp(:,:,k))

  do j=jsc-halo,jec+halo
     do i=isc-halo,iec+halo-1   
        FDX_PT(i,j) = FDX_PT(i,j) - 0.5*( -(a(i,  j,2) - a(i,  j,1))/Thk%dhwt(i,  j,k-1)   &
                                          -(a(i+1,j,2) - a(i+1,j,1))/Thk%dhwt(i+1,j,k-1) ) &
                                   *((Thk%ztp(i+1,j,k) - Thk%ztp(i,j,k))*Grd%dxter(i,j))
     enddo
  enddo

end function FDX_PT
! </FUNCTION> NAME="FDX_PT"


!#######################################################################
! <FUNCTION NAME="FDX_PT_flat">
!
! <DESCRIPTION>
! Forward Derivative in X of a quantity defined on a tracer grid point.
! To be used for pure lateral derivatives--no "correction term". 
! Input a(i,j) is at the grid point of a T-cell
! Output is defined at (i+1/2,j) which is at the east face in a T-cell.
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
function FDX_PT_flat(a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: FDX_PT_flat
  integer :: i, j

  do i=isc-halo,iec+halo-1
     FDX_PT_flat(i,:) = (a(i+1,:) - a(i,:))*Grd%dxter(i,:)
  enddo
  FDX_PT_flat(iec+halo,:) = 0.0

end function FDX_PT_flat
! </FUNCTION> NAME="FDX_PT_flat"


!#######################################################################
! <FUNCTION NAME="FDY_PT">
!
! <DESCRIPTION>
! Forward Derivative in Y of a quantity defined on a tracer grid point.
! To be used for tracer derivatives when grid points are not all at the same depth. 
! Input a(i,j,1) is at the grid point of a T-cell at level k-1.
! Input a(i,j,2) is at the grid point of a T-cell at level k.
! Output is defined at (i,j+1/2) which is at the north face in a T-cell at level k.
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
! <IN NAME="k" TYPE="integer">
! Depth level 
! </IN>
function FDY_PT(a,k)

  real, intent(in), dimension(isd:ied,jsd:jed,2) :: a
  real, dimension(isd:ied,jsd:jed) ::  FDY_PT
  integer, intent(in)  :: k
  integer :: i, j

  do j=jsc-halo,jec+halo-1
     do i=isc-halo,iec+halo
        FDY_PT(i,j) = (a(i,j+1,2) - a(i,j,2))*Grd%dytnr(i,j)
     enddo
  enddo
  FDY_PT(:,jec+halo) = 0.0

! expanded form of 
! FDY_PT(:,:) = FDY_PT(:,:) - FAY(FDZ_PT(a(:,:,1:2),k-1))*FDY_ZTP(Grd%ztp(:,:,k))

  do j=jsc-halo,jec+halo-1
     do i=isc-halo,iec+halo
        FDY_PT(i,j) = FDY_PT(i,j) - 0.5*( -(a(i,  j,2) - a(i,  j,1))/Thk%dhwt(i,  j,k-1)   &
                                          -(a(i,j+1,2) - a(i,j+1,1))/Thk%dhwt(i,j+1,k-1) ) &
                                    *((Thk%ztp(i,j+1,k) - Thk%ztp(i,j,k))*Grd%dytnr(i,j))
     enddo
  enddo


end function FDY_PT
! </FUNCTION> NAME="FDY_PT"


!#######################################################################
! <FUNCTION NAME="FDY_PT_flat">
!
! <DESCRIPTION>
! Forward Derivative in Y of a quantity defined on the grid Point of a T-cell.
! To be used for pure lateral derivatives--no "correction term". 
! Input a(i,j) is at the grid point of a T-cell.
! Output is defined at (i,j+1/2) which is at the north face in a T-cell.
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
function FDY_PT_flat(a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) ::  FDY_PT_flat
  integer :: i, j

  do j=jsc-halo,jec+halo-1
     do i=isc-halo,iec+halo
        FDY_PT_flat(i,j) = (a(i,j+1) - a(i,j))*Grd%dytnr(i,j)
     enddo
  enddo
  FDY_PT_flat(:,jec+halo) = 0.0

end function FDY_PT_flat
! </FUNCTION> NAME="FDY_PT_flat"


!#######################################################################
! <FUNCTION NAME="FDX_NT">
!
! <DESCRIPTION>
! Forward Derivative in X of a quantity defined on the North face of a T-cell.
! If input is a(i,j) then output is defined at (i+1/2,j).
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
function FDX_NT(a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: FDX_NT
  integer :: i, j

  do j=jsc-halo,jec+halo
     do i=isc-halo,iec+halo-1
        FDX_NT(i,j) = (a(i+1,j) - a(i,j))*Grd%dxur(i,j)  ! ok for scalars
     enddo
     FDX_NT(iec+halo,j) = 0.0
  enddo

end function FDX_NT
! </FUNCTION> NAME="FDX_NT"


!#######################################################################
! <FUNCTION NAME="BDY_NU">
!
! <DESCRIPTION>
! Backward Derivative in Y of a quantity defined on the North face of a U-cell.
! If input is a(i,j) then output is defined at (i,j-1/2).
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
function BDY_NU(a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: BDY_NU
  integer :: i, j

  do j=jsc-halo+1,jec+halo
     do i=isc-halo,iec+halo
        BDY_NU(i,j) = (Grd%dxun(i,j)*a(i,j) - Grd%dxun(i,j-1)*a(i,j-1))*Grd%daur(i,j)
     enddo
  enddo
  BDY_NU(:,jsc-halo) = 0.0

end function BDY_NU
! </FUNCTION> NAME="BDY_NU"


!#######################################################################
! <FUNCTION NAME="BDY_NT">
!
! <DESCRIPTION>
! Backward Derivative in Y of a quantity defined on the North face of a T-cell.
! If input is a(i,j) then output is defined at (i,j-1/2).
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
function BDY_NT(a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: BDY_NT
  integer :: i, j

  do j=jsc-halo+1,jec+halo
     do i=isc-halo,iec+halo
        BDY_NT(i,j) = (Grd%dxtn(i,j)*a(i,j) - Grd%dxtn(i,j-1)*a(i,j-1))*Grd%datr(i,j)  
     enddo
  enddo
  BDY_NT(:,jsc-halo) = 0.0

end function BDY_NT
! </FUNCTION> NAME="BDY_NT"


!#######################################################################
! <FUNCTION NAME="FDY_PU">
!
! <DESCRIPTION>
! Forward Derivative in Y of a quantity defined on the grid Point of a U-cell.
! If input is a(i,j) then output is defined at (i,j+1/2).
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
function FDY_PU(a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: FDY_PU
  integer :: i, j

  do j=jsc-halo,jec+halo-1
     do i=isc-halo,iec+halo
        FDY_PU(i,j) = (a(i,j+1) - a(i,j))*Grd%dyunr(i,j)
     enddo
  enddo
  FDY_PU(:,jec+halo) = 0.0

end function FDY_PU
! </FUNCTION> NAME="FDY_PU"


!#######################################################################
! <FUNCTION NAME="FDY_ET">
!
! <DESCRIPTION>
! Forward Derivative in Y of a quantity defined on the East face of a T-cell.
! If input is a(i,j) then output is defined at (i,j+1/2).
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
function FDY_ET(a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: FDY_ET
  integer :: i, j

  do j=jsc-halo,jec+halo-1
     do i=isc-halo,iec+halo
        FDY_ET(i,j) = (a(i,j+1) - a(i,j))*Grd%dyur(i,j)
     enddo
  enddo
  FDY_ET(:,jec+halo) = 0.0

end function FDY_ET
! </FUNCTION> NAME="FDY_ET"


!#######################################################################
! <FUNCTION NAME="FDZ_PT">
!
! <DESCRIPTION>
! Forward Derivative in Z of a quantity defined on the grid point of a T-cell at level k.
! input a(i,j,1) is at the grid point of a T-cell at level k.
! input a(i,j,2) is at the grid point of a T-cell at level k+1.
! output is defined at (i,j,3/2) which is at bottom face of a T-cell at level k.
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to be finite differenced 
! </IN>
! <IN NAME="k" TYPE="integer">
! Depth level index 
! </IN>
!
function FDZ_PT(a, k)

  real, intent(in), dimension(isd:ied,jsd:jed,2) :: a
  real, dimension(isd:ied,jsd:jed) :: FDZ_PT
  integer, intent(in) :: k

  FDZ_PT(:,:) =  -(a(:,:,2) - a(:,:,1))/Thk%dhwt(:,:,k) 

end function FDZ_PT
! </FUNCTION> NAME="FDZ_PT"


!#######################################################################
! <FUNCTION NAME="FMX">
!
! <DESCRIPTION>
! Forwards Minimum in the X direction.
! If input is a(i,j) then output is defined at (i+1/2,j).
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to find minimum
! </IN>
!
function FMX (a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: FMX
  integer :: i, j

  do j=jsc-halo,jec+halo
     do i=isc-halo,iec+halo-1
        FMX(i,j) = min(a(i+1,j), a(i,j))
     enddo
     FMX(iec+halo,j) = 0.0
  enddo

end function FMX
! </FUNCTION> NAME="FMX"


!#######################################################################
! <FUNCTION NAME="FMY">
!
! <DESCRIPTION>
! Forwards Minimum in the Y direction.
! If input is a(i,j) then output is defined at (i,j+1/2).
! </DESCRIPTION>
!
! <IN NAME="a" TYPE="real" DIM="(isd:ied,jsd:jed)">
! Field to find minimum
! </IN>
function FMY (a)

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  real, dimension(isd:ied,jsd:jed) :: FMY
  integer :: i, j

  do j=jsc-halo,jec+halo-1
     do i=isc-halo,iec+halo
        FMY(i,j) = min(a(i,j+1), a(i,j))
     enddo
  enddo
  FMY(:,jec+halo) = 0.0

end function FMY
! </FUNCTION> NAME="FMY"


!#######################################################################
! <FUNCTION NAME="REMAP_T_TO_V_NOCONSERVE">
!
! <DESCRIPTION>
! T-cell field to U-cells without any grid factors included.  
! Needed for getting rho_nbu from rho_nbt. 
! </DESCRIPTION>
!
function REMAP_T_TO_V_NOCONSERVE(a,k) 

  use constants_mod, only: epsln

  real, intent(in), dimension(isd:ied,jsd:jed) :: a
  integer, intent(in)                          :: k
  real, dimension(isd:ied,jsd:jed)             :: REMAP_T_TO_V_NOCONSERVE
  integer                                      :: i, j
  real                                         :: active_cells 

  do j=jsd,jed-1
     do i=isd,ied-1
        active_cells = Grd%tmask(i,j,k) + Grd%tmask(i+1,j,k) + Grd%tmask(i,j+1,k) + Grd%tmask(i+1,j+1,k) + epsln   
        REMAP_T_TO_V_NOCONSERVE(i,j) = (a(i,j) + a(i+1,j) + a(i,j+1) + a(i+1,j+1))/active_cells 
     enddo
  enddo
  REMAP_T_TO_V_NOCONSERVE(ied,:) = 0.0
  REMAP_T_TO_V_NOCONSERVE(:,jed) = 0.0

end function REMAP_T_TO_V_NOCONSERVE
! </FUNCTION> NAME="REMAP_T_TO_V_NOCONSERVE"


end module ocean_operators_mod

