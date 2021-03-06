module ocean_domains_mod
!  
!<CONTACT EMAIL="Matthew.Harrison@noaa.gov"> Matt Harrison 
!</CONTACT>
!
!<OVERVIEW>
! Set the ocean domain parameters. 
!</OVERVIEW>
!
!<DESCRIPTION>
! The module computes the horizontal domain parameters needed to 
! run mom4 in a parallel computational environment. 
! </DESCRIPTION>
!
! <INFO>
!
! <REFERENCE>
! S.M. Griffies, M.J. Harrison,  R.C. Pacanowski, and A. Rosati
! A Technical Guide to MOM4 (2003)
! </REFERENCE>
!
! </INFO>
!
!<NAMELIST NAME="ocean_domains_nml">
!  <DATA NAME="halo" UNITS="dimensionless" TYPE="integer">
!  For specifying the halo size by hand.
!  </DATA> 
!</NAMELIST>

use fms_mod,         only: open_namelist_file, write_version_number, check_nml_error, close_file
use mpp_domains_mod, only: mpp_define_layout, mpp_define_domains, mpp_domains_set_stack_size
use mpp_domains_mod, only: mpp_get_compute_domain, mpp_get_data_domain, mpp_get_global_domain
use mpp_domains_mod, only: FOLD_NORTH_EDGE, CYCLIC_GLOBAL_DOMAIN
use mpp_mod,         only: mpp_max, mpp_min, mpp_npes, mpp_error, FATAL, stdout, stdlog

use ocean_types_mod, only: ocean_domain_type, ocean_grid_type
use ocean_types_mod, only: ocean_prog_tracer_type, ocean_velocity_type

implicit none

private

integer, private               :: halo=1       ! size (cells) of halo region
integer, private, dimension(2) :: domain_layout=(/1,1/)

character(len=128) :: version='$Id: ocean_domains.F90,v 1.5 2004/12/10 19:35:16 gtn Exp $'
character(len=128) :: tagname='$Name: mom4p0d $'

character(len=32) :: name_default = 'mom_domain'
  
public set_ocean_domain
public get_local_indices
public get_halo_sizes
public get_global_indices
public get_active_indices
public reduce_active_domain
public get_domain_offsets

namelist /ocean_domains_nml/ halo

contains

!#######################################################################
! <SUBROUTINE NAME="set_ocean_domain">
!
! <DESCRIPTION>
! For setting the ocean domain layout and associated parameters. 
! </DESCRIPTION>
!
subroutine set_ocean_domain (Domain, Grid, xhalo, yhalo, name, layout)

  type(ocean_grid_type), intent(in)      :: Grid
  type(ocean_domain_type), intent(inout) :: Domain
  integer, intent(in), optional          :: xhalo, yhalo, layout(2)
  character(len=*), intent(in), optional :: name

  real              :: ph, pc
  integer           :: n, ioun, io_status, nhp, ncp, ncp_max, ncp_min, ncpx, ncpy, lay_out(2), xsiz, ysiz
  integer           :: mpp_stack_size=-1 ! temporary - need to call domains_init before tracer_init 
  integer           :: max_tracers = 10  ! temporary - need to call domains_init before tracer_init 
  character(len=32) :: name_
  character(len=4)  :: char_lay1, char_lay2, char_npes, char_xsiz, char_ysiz

  call write_version_number(version, tagname)

  ! provide for namelist over-ride
  ioun = open_namelist_file()
  read  (ioun, ocean_domains_nml,iostat=io_status)
  write (stdlog(),ocean_domains_nml)
  write (stdout(),'(/)')
  write (stdout(),ocean_domains_nml)
  call close_file(ioun)

  if (PRESENT(layout)) domain_layout = layout
  if (domain_layout(1) == 1 .and. domain_layout(2) == 1) then
     call mpp_define_layout ((/1,Grid%ni,1,Grid%nj/), mpp_npes(), lay_out)
  else
     if (domain_layout(1)*domain_layout(2) /= mpp_npes())  then 
        write( char_lay1,'(i4)' )domain_layout(1)
        write( char_lay2,'(i4)' )domain_layout(2)
        write( char_npes,'(i4)' )mpp_npes()
        call mpp_error(FATAL, '==>Error in ocean_domains_mod: domain_layout (PEx*PEy =' &
                              // trim(char_lay1) // ' * ' // trim(char_lay2) // &
                              ') .ne. number of processors (' // trim(char_npes) // ')' )
     else
        lay_out = domain_layout
     endif 
  endif

  Domain%layout = lay_out

  if (PRESENT(name)) then
     name_ = trim(name)
  else
     name_ = trim(name_default)
  endif

  if (PRESENT(xhalo)) then
     Domain%xhalo=xhalo
  else
     Domain%xhalo=halo
  endif

  if (PRESENT(yhalo)) then
     Domain%yhalo=yhalo
  else
     Domain%yhalo=halo
  endif


! setup 2D domains for local x-y regions (including halos) on each processor.

  if (Grid%cyclic) then
     if (Grid%tripolar) then
        call mpp_define_domains( (/1,Grid%ni,1,Grid%nj/), lay_out, Domain%domain2d&
             , xflags = cyclic_global_domain, yflags = FOLD_NORTH_EDGE, xhalo=Domain%xhalo, yhalo=Domain%yhalo,name=name_)
        Domain%xflags = cyclic_global_domain
        Domain%yflags = FOLD_NORTH_EDGE
     else
        call mpp_define_domains( (/1,Grid%ni,1,Grid%nj/), lay_out, Domain%domain2d&
             , xflags = cyclic_global_domain, xhalo=Domain%xhalo, yhalo=Domain%yhalo,name=name_)
        Domain%xflags = cyclic_global_domain
        Domain%yflags = 0
     endif
  else
     call mpp_define_domains( (/1,Grid%ni,1,Grid%nj/), lay_out, Domain%domain2d&
          , xhalo=Domain%xhalo, yhalo=Domain%yhalo,name=name_)
        Domain%xflags = 0
        Domain%yflags = 0
  endif


  ! define data and compute indices for local domain on this processor

  call mpp_get_compute_domain (Domain%domain2d, Domain%isc, Domain%iec, &
       Domain%jsc, Domain%jec)

  call mpp_get_data_domain (Domain%domain2d, Domain%isd, Domain%ied, &
       Domain%jsd, Domain%jed)

  call mpp_get_global_domain (Domain%domain2d, Domain%isg, Domain%ieg, &
       Domain%jsg, Domain%jeg)

  Domain%isa = Domain%isc
  Domain%iea = Domain%iec
  Domain%jsa = Domain%jsc
  Domain%jea = Domain%jec

  Domain%ioff = 0
  Domain%joff = 0
  
#ifdef STATIC_MEMORY

  if (Domain%xhalo == 1 .AND. Domain%yhalo == 1) then
     xsiz = Domain%iec - Domain%isc + 1
     ysiz = Domain%jec - Domain%jsc + 1
     Domain%ioff = Domain%isc - 1     
     Domain%isc = 1
     Domain%isd = 0
     Domain%iec = xsiz
     Domain%ied = xsiz+1
     Domain%joff = Domain%jsc - 1          
     Domain%jsc = 1
     Domain%jsd = 0
     Domain%jec = ysiz
     Domain%jed = ysiz+1
     
     if (xsiz /= NI_LOCAL_ .OR. ysiz /= NJ_LOCAL_) then
        write( char_xsiz,'(i4)' ) xsiz
        write( char_ysiz,'(i4)' ) ysiz
        call mpp_error(FATAL,'==>Error in ocean_domains_mod: domain size (xsiz,ysiz) = ('&
        //trim(char_xsiz)//','//trim(char_ysiz)// ') does not match size set by preprocessor macro.&
          & Disable STATIC_MEMORY cpp-preprocessor option to use dynamic allocation. ')
     endif
 endif

#endif


  ncp_max = (Domain%iec-Domain%isc+1)*(Domain%jec-Domain%jsc+1)
  ncp_min = ncp_max
  call mpp_max(ncp_max)
  call mpp_min(ncp_min)

  ncp = (Domain%iec-Domain%isc+1)*(Domain%jec-Domain%jsc+1)
  nhp = 4*halo**2 + 2*(Domain%iec-Domain%isc+1 + Domain%jec-Domain%jsc+1)*halo
  ph = 100.0*nhp/(nhp+ncp)
  pc = 100.0 - ph

  ! estimate stack size
  if (mpp_stack_size <= 0) then 
    ncpx = Domain%iec-Domain%isc+1
    ncpy = Domain%jec-Domain%jsc+1
    call mpp_max (ncpx)
    call mpp_max (ncpy)
    mpp_stack_size = 2*(ncpx + 2*max(Domain%xhalo,Domain%yhalo))*&
                    (ncpy + 2*max(Domain%xhalo,Domain%yhalo))*Grid%nk*(max_tracers) 
  endif

  call mpp_domains_set_stack_size(mpp_stack_size)

end subroutine set_ocean_domain
! </SUBROUTINE> NAME="set_ocean_domain"


!#######################################################################
! <SUBROUTINE NAME="get_local_indices">
!
! <DESCRIPTION>
! For getting local indices from domain derived type. 
! </DESCRIPTION>
!
subroutine get_local_indices(Domain, isd, ied, jsd, jed, isc, iec, jsc, jec)

  type(ocean_domain_type), intent(in) :: Domain
  integer, intent(out)                :: isd, ied, jsd, jed, isc, iec, jsc, jec
  
  isd=Domain%isd;ied=Domain%ied;jsd=Domain%jsd;jed=Domain%jed
  isc=Domain%isc;iec=Domain%iec;jsc=Domain%jsc;jec=Domain%jec

  
end subroutine get_local_indices
! </SUBROUTINE> NAME="get_local_indices"


!#######################################################################
! <SUBROUTINE NAME="get_domain_offsets">
!
! <DESCRIPTION>
! For getting domain offsets from domain derived type. 
! </DESCRIPTION>
!
subroutine get_domain_offsets(Domain, ioff, joff)

  type(ocean_domain_type), intent(in) :: Domain
  integer, intent(out)                :: ioff, joff
  
  ioff = Domain%ioff
  joff = Domain%joff
  
end subroutine get_domain_offsets
! </SUBROUTINE> NAME="get_domain_offsets"


!#######################################################################
! <SUBROUTINE NAME="get_active_indices">
!
! <DESCRIPTION>
! For getting active domain indices from domain derived type. 
! </DESCRIPTION>
!
subroutine get_active_indices(Domain, isa, iea, jsa, jea)

  type(ocean_domain_type), intent(in) :: Domain
  integer, intent(out)                :: isa, iea, jsa, jea

  isa=Domain%isa;iea=Domain%iea;jsa=Domain%jsa;jea=Domain%jea

end subroutine get_active_indices
! </SUBROUTINE> NAME="get_active_indices"


!#######################################################################
! <SUBROUTINE NAME="get_global_indices">
!
! <DESCRIPTION>
! For getting global indices from domain derived type. 
! </DESCRIPTION>
!
subroutine get_global_indices(Domain, isg, ieg, jsg, jeg)

  type(ocean_domain_type), intent(in) :: Domain
  integer, intent(out)                :: isg, ieg, jsg, jeg

  isg=Domain%isg;ieg=Domain%ieg;jsg=Domain%jsg;jeg=Domain%jeg
  
end subroutine get_global_indices
! </SUBROUTINE> NAME="get_global_indices"



!#######################################################################
! <SUBROUTINE NAME="reduce_active_domain">
!
! <DESCRIPTION>
! For getting reducing the active domain 
! </DESCRIPTION>
!
subroutine reduce_active_domain(Domain,inc_x,inc_y)

  type(ocean_domain_type), intent(inout) :: Domain
  integer, intent(in), optional          :: inc_x, inc_y
  integer                                :: inc

  if (PRESENT(inc_x)) then
     if (inc_x < 1) call mpp_error(FATAL,'==>Error: active domain decrement must be positive')
     inc = inc_x
  else
     inc = 1
  endif

  Domain%isa = Domain%isa + inc
  Domain%iea = Domain%iea - inc

  if (PRESENT(inc_y)) then
     if (inc_y < 1) call mpp_error(FATAL,'==>Error: active domain decrement must be positive')
     inc = inc_y
  else
     inc = 1
  endif

  Domain%jsa = Domain%jsa + inc
  Domain%jea = Domain%jea - inc

end subroutine reduce_active_domain
! </SUBROUTINE> NAME="reduce_active_domain"



!#######################################################################
! <SUBROUTINE NAME="get_halo_sizes">
!
! <DESCRIPTION>
! For getting halo sizes from domain derived type. 
! </DESCRIPTION>
!
subroutine get_halo_sizes(Domain, xhalo, yhalo)

  type(ocean_domain_type), intent(in) :: Domain
  integer, intent(out), optional      :: xhalo, yhalo

  if (PRESENT(xhalo)) xhalo = Domain%xhalo
  if (PRESENT(yhalo)) yhalo = Domain%yhalo

end subroutine get_halo_sizes
! </SUBROUTINE> NAME="get_halo_sizes"

  
end module ocean_domains_mod
