module ocean_age_tracer_mod  !{
! ----------------------------------------------------------------
!                   GNU General Public License                        
! This file is a part of MOM.                                                                 
!                                                                      
! MOM is free software; you can redistribute it and/or modify it and  
! are expected to follow the terms of the GNU General Public License  
! as published by the Free Software Foundation; either version 2 of   
! the License, or (at your option) any later version.                 
!                                                                      
! MOM is distributed in the hope that it will be useful, but WITHOUT    
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  
! or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public    
! License for more details.                                           
!                                                                      
! For the full text of the GNU General Public License,                
! write to: Free Software Foundation, Inc.,                           
!           675 Mass Ave, Cambridge, MA 02139, USA.                   
! or see:   http://www.gnu.org/licenses/gpl.html                      
!-----------------------------------------------------------------------
!
! 
!<CONTACT EMAIL="Richard.Slater@noaa.gov"> Richard D. Slater
!</CONTACT>
!
!<REVIEWER EMAIL="John.Dunne@noaa.gov"> John P. Dunne
!</REVIEWER>
!
!<REVIEWER EMAIL="Stephen.Griffies@noaa.gov"> Stephen M. Griffies 
!</REVIEWER>
!
!<OVERVIEW>
! Ocean age tracer module
!</OVERVIEW>
!
!<DESCRIPTION>
!       This module will perform the necessary operations to handle
!       a series of ocean age tracers. The following types are allowed:
!
!       normal: the age of the designated surface area is fixed at
!               0, while all other grid points increase in age.
!               Therefore, the resultant age is the length of time
!               since that water has been in contact with the
!               designated area of surface. (This is the default type.)
!
!      Note that the tracer "concentration" known as "age" is 
!      in units of years, hence the multiplier "secs_in_year_r"
!      applied to t_prog(index)%th_tendency.  This multiplier IS NOT 
!      needed for any other terms, since they all have units with 
!      meter/sec, and this is multiplied by dtime(secs) when 
!      time stepping.  
!
!</DESCRIPTION>
!
! $Id: ocean_age_tracer.F90,v 1.6 2005/05/12 21:41:05 gtn Exp $
!

!
!       modules
!

use field_manager_mod,  only: fm_string_len, fm_path_name_len, fm_field_name_len
use field_manager_mod,  only: fm_get_length, fm_get_value, fm_new_value
use mpp_mod,            only: stdout, stderr, stdlog, mpp_error, FATAL
use time_manager_mod,   only : get_date

use ocean_tpm_util_mod, only: otpm_set_tracer_package, otpm_set_prog_tracer, otpm_set_value
use ocean_tpm_util_mod, only: otpm_start_namelist, otpm_end_namelist
use ocean_tpm_util_mod, only: otpm_get_string, otpm_get_string_array
use ocean_tpm_util_mod, only: otpm_get_logical, otpm_get_logical_array
use ocean_tpm_util_mod, only: otpm_get_real_array, otpm_check_for_bad_fields
use ocean_tpm_util_mod, only: domain, grid, time
use ocean_tpm_util_mod, only: isc, iec, jsc, jec, nk, isd, ied, jsd, jed 
use ocean_tpm_util_mod, only: t_prog, t_diag

use ocean_types_mod,    only: ocean_thickness_type


!
!       force all variables to be "typed"
!

implicit none

!
!       Set all variables to be private by default

private

!
!       Public routines
!

public  :: ocean_age_tracer_end
public  :: ocean_age_tracer_init
public  :: ocean_age_tracer_source
public  :: ocean_age_tracer_start
public  :: ocean_age_tracer_tracer

!
!       Private routines
!

private :: set_array

!
!       Public parameters
!


!
!       Private parameters
!

integer, parameter                              :: num_types = 1
character(len=fm_field_name_len), parameter     :: package_name = 'ocean_age_tracer'
character(len=48), parameter                    :: mod_name = 'ocean_age_tracer_mod'
character(len=fm_string_len), parameter         :: default_file_in = 'INPUT/ocean_age.res.nc'
character(len=fm_string_len), parameter         :: default_file_out = 'RESTART/ocean_age.res.nc'
real, parameter                                 :: secs_in_year_r = 1.0 / (86400.0 * 365.25)

!
!       Public variables
!

logical, public :: do_ocean_age_tracer

!
!       Private types
!

type age_type
  character(len=fm_field_name_len)      :: name
  character(len=fm_string_len)          :: age_tracer_type
  real, dimension(:,:,:), pointer       :: bc => NULL()
  integer                               :: index
  real, dimension(:), pointer           :: elon => NULL()
  real, dimension(:), pointer           :: nlat => NULL()
  real, dimension(:), pointer           :: slat => NULL()
  real, dimension(:), pointer           :: wlon => NULL()
  logical                               :: coastal_only
  logical, dimension(:), pointer        :: t_mask => NULL()
end type age_type

!
!       Private variables
!

type(age_type), dimension(:), allocatable               :: age
character(len=fm_string_len), dimension(num_types)      :: age_types
logical, dimension(12)                                  :: t_mask
integer                                                 :: instances
integer                                                 :: package_index

!
!       time variables
!

integer :: day
integer :: hour
integer :: minute
integer :: month
integer :: second
integer :: year

contains

!#######################################################################
! <SUBROUTINE NAME="ocean_age_tracer_end">
!
! <DESCRIPTION>
!       Finish up calculations for the tracer packages,
!       possibly writing out non-field restart information
! </DESCRIPTION>
!

subroutine ocean_age_tracer_end  !{

!
!       local parameters
!

character(len=64), parameter    :: sub_name = 'ocean_age_tracer_end'
character(len=256), parameter   :: error_header =                               &
     '==>Error from ' // trim(mod_name) // '(' // trim(sub_name) // '):'
character(len=256), parameter   :: warn_header =                                &
     '==>Warning from ' // trim(mod_name) // '(' // trim(sub_name) // '):'
character(len=256), parameter   :: note_header =                                &
     '==>Note from ' // trim(mod_name) // '(' // trim(sub_name) // '):'

!
!       finish up the run
!

return

end subroutine ocean_age_tracer_end  !}
! </SUBROUTINE> NAME="ocean_age_tracer_end"


!#######################################################################
! <SUBROUTINE NAME="ocean_age_tracer_init">
!
! <DESCRIPTION>
!       Set up any extra fields needed by the tracer packages
!
!       Save pointers to various "types", such as Grid and Domains.
! </DESCRIPTION>
!

subroutine ocean_age_tracer_init  !{

!
!       local parameters
!

character(len=64), parameter    :: sub_name = 'ocean_age_tracer_init'
character(len=256), parameter   :: error_header =                               &
     '==>Error from ' // trim(mod_name) // '(' // trim(sub_name) // '):'
character(len=256), parameter   :: warn_header =                                &
     '==>Warning from ' // trim(mod_name) // '(' // trim(sub_name) // '):'
character(len=256), parameter   :: note_header =                                &
     '==>Note from ' // trim(mod_name) // '(' // trim(sub_name) // '):'

!
!-----------------------------------------------------------------------
!       arguments
!-----------------------------------------------------------------------
!

!
!-----------------------------------------------------------------------
!       local variables
!-----------------------------------------------------------------------
!

integer                                                 :: n
character(len=fm_field_name_len)                        :: name
character(len=fm_path_name_len)                         :: path_to_names
character(len=fm_field_name_len+1)                      :: suffix
character(len=fm_field_name_len+3)                      :: long_suffix
character(len=256)                                      :: caller_str
character(len=fm_string_len), pointer, dimension(:)     :: good_list

!
!       Initialize the ocean age tracer package
!

package_index = otpm_set_tracer_package(package_name,                           &
     units = 'yr', flux_units = 'm',                                            &
     min_tracer_limit = 0.0, max_tracer_limit = 1.0e+20,                        &
     file_in = default_file_in, file_out = default_file_out,                    &
     caller = trim(mod_name) // '(' // trim(sub_name) // ')')

!
!       Check whether to use this package
!

path_to_names = '/ocean_mod/tracer_packages/' // trim(package_name) // '/names'
instances = fm_get_length(path_to_names)
if (instances .lt. 0) then  !{
  call mpp_error(FATAL, trim(error_header) // ' Could not get number of instances')
endif  !}

!
!       Check some things
!

write (stdout(),*)
if (instances .eq. 0) then  !{
  write (stdout(),*)                                            &
       trim(note_header), ' No instances'
  do_ocean_age_tracer = .false.
else  !}{
  write (stdout(),*) trim(note_header), ' ', instances, ' instances'
  do_ocean_age_tracer = .true.
endif  !}

!
!       Return if we don't want to use this package
!

if (.not. do_ocean_age_tracer) then  !{
  return
endif  !}

!
!       allocate the age array
!

allocate ( age(instances) )

!
!       loop over the names, saving them into the age array
!

do n = 1, instances  !{

  if (fm_get_value(path_to_names, name, index = n)) then  !{
    age(n)%name = name
  else  !}{
    write (name,*) n
    call mpp_error(FATAL, trim(error_header) //        &
         ' Bad field name for index ' // trim(name))
  endif  !}

enddo  !} n

do n = 1, instances  !{

!
!       allocate storage for this instance
!

  allocate( age(n)%bc(domain%isd:domain%ied,domain%jsd:domain%jed,12) )

!
!       determine the tracer name for this instance
!
  name = age(n)%name
  if (name(1:1) .eq. '_') then  !{
    suffix = ' '
    long_suffix = ' '
  else  !}{
    suffix = '_' // name
    long_suffix = ' (' // trim(name) // ')'
  endif  !}

  age(n)%index = otpm_set_prog_tracer('age' // trim(suffix), package_name,      &
       longname = 'Age' // trim(long_suffix),                                   &
       caller = trim(mod_name)//'('//trim(sub_name)//')')

enddo  !} n

!
!-----------------------------------------------------------------------
!       Set up the instance age namelists
!-----------------------------------------------------------------------
!

!
!       Add the package name to the list of good namelists, to be used
!       later for a consistency check
!

if (fm_new_value('/ocean_mod/GOOD/good_namelists', package_name, append = .true.) .le. 0) then  !{
  call mpp_error(FATAL, trim(error_header) //                           &
       ' Could not add ' // trim(package_name) // ' to "good_namelists" list')
endif  !}

caller_str = trim(mod_name) // '(' // trim(sub_name) // ')'

t_mask(:) = .true.

do n = 1, instances  !{

!
!       create the instance namelist
!

  call otpm_start_namelist(package_name, age(n)%name, caller = caller_str, no_overwrite = .true., &
       check = .true.)

  call otpm_set_value('coastal_only', .false.)
  call otpm_set_value('t_mask', t_mask, size(t_mask))
  call otpm_set_value('age_tracer_type', 'not used')
  call otpm_set_value('wlon', 0.0, index = 0)
  call otpm_set_value('elon', 0.0, index = 0)
  call otpm_set_value('slat', 0.0, index = 0)
  call otpm_set_value('nlat', 0.0, index = 0)

  call otpm_end_namelist(package_name, age(n)%name, check = .true., caller = caller_str)

enddo  !} n

!
!       Check for any errors in the number of fields in the namelists for this package
!

good_list => otpm_get_string_array('/ocean_mod/GOOD/namelists/' // trim(package_name) // '/good_values',   &
     caller = trim(mod_name) // '(' // trim(sub_name) // ')')
if (associated(good_list)) then  !{
  call otpm_check_for_bad_fields('/ocean_mod/namelists/' // trim(package_name), good_list,       &
       caller = trim(mod_name) // '(' // trim(sub_name) // ')')
  deallocate(good_list)
else  !}{
  call mpp_error(FATAL,trim(error_header) // ' Empty "' // trim(package_name) // '" list')
endif  !}


return

end subroutine ocean_age_tracer_init  !}
! </SUBROUTINE> NAME="ocean_age_tracer_init"


!#######################################################################
! <SUBROUTINE NAME="ocean_age_tracer_source">
!
! <DESCRIPTION>
!       Calculate the source arrays for the tracer packages
! </DESCRIPTION>
!

subroutine ocean_age_tracer_source(Thickness)
!
!       modules
!
type(ocean_thickness_type), intent(in) :: Thickness

!
!       local parameters
!

character(len=64), parameter    :: sub_name = 'ocean_age_tracer_source'
character(len=256), parameter   :: error_header =                               &
     '==>Error from ' // trim(mod_name) // '(' // trim(sub_name) // '):'
character(len=256), parameter   :: warn_header =                                &
     '==>Warning from ' // trim(mod_name) // '(' // trim(sub_name) // '):'
character(len=256), parameter   :: note_header =                                &
     '==>Note from ' // trim(mod_name) // '(' // trim(sub_name) // '):'

!
!       local variables
!

integer :: index
integer :: n

!
!       set the source values for the age tracers
!

!
!       get the index for the current month for use
!       for setting the surface age and source
!

call get_date(time%model_time, year, month, day, hour, minute, second)

do n = 1, instances  !{

  index = age(n)%index
  t_prog(index)%th_tendency(:,:,1) = t_prog(index)%th_tendency(:,:,1) +   &
       Thickness%dht(:,:,1,time%tau)*age(n)%bc(:,:,month) * secs_in_year_r
  t_prog(index)%th_tendency(:,:,2:) = t_prog(index)%th_tendency(:,:,2:) + &
       Thickness%dht(:,:,2:,time%tau) * secs_in_year_r

enddo  !} n

return

end subroutine ocean_age_tracer_source 
! </SUBROUTINE> NAME="ocean_age_tracer_source"


!#######################################################################
! <SUBROUTINE NAME="ocean_age_tracer_start">
!
! <DESCRIPTION>
!       Start the ocean age tracer package
!
!       Age tracer surface area specification
!
!  wlon         : western longitude of surface age
!                 region
!  elon         : eastern longitude of surface age
!                 region
!  slat         : southern latitude of surface age
!                 region
!  nlat         : northern latitude of surface age
!                 region
!  coastal_only : if true, then only apply the changes in
!                 coastal boxes
!  t_mask       : logical array controlling whether to apply
!                 the following inhibitions and depletions to
!                 each month (true means set the masks,
!                 false means use the defaults everywhere)
!
!
!       To set the surface areas, a number of namelists are read,
!       each containing the above values. You may specify up to
!       num_region rectangles bounded by (wlon,elon,nlat,slat).
!       Any grid box whose center is in one of these rectangles will
!       be considered to be part of the surface area where the
!       age is reset to zero every time-step.
!
!       These masks may be time-dependent by specifying t_mask.
!       For any month that t_mask is true (1=Jan), the rectangular
!       regions will be set, otherwise they will be skipped.
!
!       nlat may not equal slat, and wlon may not equal elon
!
!       If slat > nlat, then nothing will be done for that rectangle
!
!       The initial surface area is empty, with the default rectangle
!       setting the surface area to be empty
!
!       More than num_regions rectangles may be used to specify
!       the area by using more than one namelist
! </DESCRIPTION>
!
subroutine ocean_age_tracer_start  !{

!
!       modules
!

!
!       local parameters
!

character(len=64), parameter    :: sub_name = 'ocean_age_tracer_start'
character(len=256), parameter   :: error_header =                               &
     '==>Error from ' // trim(mod_name) // '(' // trim(sub_name) // '):'
character(len=256), parameter   :: warn_header =                                &
     '==>Warning from ' // trim(mod_name) // '(' // trim(sub_name) // '):'
character(len=256), parameter   :: note_header =                                &
     '==>Note from ' // trim(mod_name) // '(' // trim(sub_name) // '):'

!
!       local variables
!

integer                         :: done
integer                         :: l
integer                         :: n
character(len=256)              :: caller_str
integer                         :: len_w
integer                         :: len_e
integer                         :: len_s
integer                         :: len_n

!
!       set all of the values to the default
!

do n = 1, instances  !{
  age(n)%bc(:,:,:) = 1.0
enddo  !} n

!
!-----------------------------------------------------------------------
!       save the instance namelist values
!-----------------------------------------------------------------------
!

caller_str = trim(mod_name) // '(' // trim(sub_name) // ')'

do n = 1, instances !{

  call otpm_start_namelist(package_name, age(n)%name, caller = caller_str)

  age(n)%age_tracer_type =  otpm_get_string        ('age_tracer_type', scalar = .true.)
  age(n)%coastal_only    =  otpm_get_logical       ('coastal_only', scalar = .true.)
  age(n)%wlon            => otpm_get_real_array    ('wlon')
  age(n)%elon            => otpm_get_real_array    ('elon')
  age(n)%slat            => otpm_get_real_array    ('slat')
  age(n)%nlat            => otpm_get_real_array    ('nlat')
  age(n)%t_mask          => otpm_get_logical_array ('t_mask')

  call otpm_end_namelist(package_name, age(n)%name, caller = caller_str)

!
!       Check some things
!

  if (.not. associated(age(n)%wlon) .or.        &
      .not. associated(age(n)%elon) .or.        &
      .not. associated(age(n)%slat) .or.        &
      .not. associated(age(n)%nlat)) then  !{
    call mpp_error(FATAL, trim(error_header) // ' Must specify a region')
  endif  !}

  len_w = size(age(n)%wlon)
  len_e = size(age(n)%elon)
  len_s = size(age(n)%slat)
  len_n = size(age(n)%nlat)

  if (len_e .ne. len_w .or. len_w .ne. len_s .or. len_s .ne. len_n) then  !{
    call mpp_error(FATAL, trim(error_header) // ' Region sizes are not equal')
  endif  !}

  if (size(age(n)%t_mask) .ne. 12) then  !{
    call mpp_error(FATAL, trim(error_header) // ' t_mask size is not 12')
  endif  !}

!
!       set values for this time-level
!

  done = 0
  do l = 1, 12  !{
    if (age(n)%t_mask(l)) then  !{
      if (done .eq. 0) then  !{

!
!       set the values via the input values, saving this time index
!       afterwards
!
        write (stdout(),*) trim(note_header), ' Assigning month ', l
        call set_array(n,l,                                                 &
                len_e, age(n)%wlon, age(n)%elon, age(n)%slat, age(n)%nlat,      &
                0.0, 1.0,                                                       &
                t_prog(age(n)%index)%name, age(n)%coastal_only)
        done = l
      else  !}{

!
!       Duplicate the values for a previous time-level
!

        write (stdout(),*) trim(note_header), ' Duplicating month ', done, ' as ', l
        age(n)%bc(:,:,l) = age(n)%bc(:,:,done)
      endif  !}
    endif  !}
  enddo  !} l

enddo  !} n

return

end subroutine ocean_age_tracer_start  !}
! </SUBROUTINE> NAME="ocean_age_tracer_start"


!#######################################################################
! <SUBROUTINE NAME="ocean_age_tracer_tracer">
!
! <DESCRIPTION>
!       Subroutine to do calculations needed every time-step after
!       the continuity equation has been integrated
! </DESCRIPTION>
!

subroutine ocean_age_tracer_tracer !{

!
!       local parameters
!

character(len=64), parameter    :: sub_name = 'ocean_age_tracer_tracer'
character(len=256), parameter   :: error_header =                               &
     '==>Error from ' // trim(mod_name) // '(' // trim(sub_name) // '):'
character(len=256), parameter   :: warn_header =                                &
     '==>Warning from ' // trim(mod_name) // '(' // trim(sub_name) // '):'
character(len=256), parameter   :: note_header =                                &
     '==>Note from ' // trim(mod_name) // '(' // trim(sub_name) // '):'

!
!       local variables
!

integer :: index
integer :: n

!
!       fix the surface values
!
do n = 1, instances  !{

  index = age(n)%index

  t_prog(index)%field(:,:,1,time%taup1) =                       &
       t_prog(index)%field(:,:,1,time%taup1) * age(n)%bc(:,:,month)

enddo  !} n

return

end subroutine ocean_age_tracer_tracer  !}
! </SUBROUTINE> NAME="ocean_age_tracer_tracer"


!#######################################################################
! <SUBROUTINE NAME="set_array">
!
! <DESCRIPTION>
!       Set up an array covering the model domain with a user-specified
!       value, in user-specified regions. There are a given number of
!       2-d regions specified by the values slat, nlat, wlon and elon.
!       The longitudes are for a cyclic domain, and if wlon and elon
!       are on opposite sides of the cut, the correct thing will
!       be done. Elon is considered to be east of wlon, so if elon is
!       less than wlon, then the region east of elon to the cut will be
!       filled, and the region from the cut to wlon will be filled.
!
!       After setting up the array in this routine, it may prove useful
!       to allow fine-tuning the settings via an array in a namelist.
!
!       Arguments:
!         Input:
!      num_regions  number of user-specified regions which will be filled
!             wlon  1-d array of western (starting) longitudes for the
!                   rectangular regions
!             elon  1-d array of eastern (ending) longitudes for the
!                   rectangular regions
!             slat  1-d array of southern (starting) latitudes for the
!                   rectangular regions
!             nlat  1-d array of northern (ending) latitudes for the
!                   rectangular regions
!                       Note: if slat >= nlat, then nothing is done
!                             for that region
!        set_value  the value to assign to array in the user-specified
!                   regions
!      unset_value  the value to assign to array outside of the
!                   user-specified regions
!             name  character variable used in informative messages
!     coastal_only  true to limit changes only to coastal points (i.e.,
!                   at least one bordering point is land)
!
!         Output:
!            array  2-d array which will contain the set- and unset-
!                   values. The array is assumed to have a border
!                   one unit wide on all edges, ala MOM. A cyclic
!                   boundary condition will be set if requested.
! </DESCRIPTION>
!
!

subroutine set_array(index,l,                                   &
                     num_regions, wlon_in, elon_in, slat, nlat, &
                     set_value, unset_value, name,              &
                     coastal_only)  !{

!
!       local parameters
!

character(len=64), parameter    :: sub_name = 'set_array'
character(len=256), parameter   :: error_header =                               &
     '==>Error from ' // trim(mod_name) // '(' // trim(sub_name) // '):'
character(len=256), parameter   :: warn_header =                                &
     '==>Warning from ' // trim(mod_name) // '(' // trim(sub_name) // '):'
character(len=256), parameter   :: note_header =                                &
     '==>Note from ' // trim(mod_name) // '(' // trim(sub_name) // '):'

!
!       arguments
!

integer, intent(in)                             :: num_regions
integer, intent(in)                             :: index
integer, intent(in)                             :: l
!real, dimension(:,:), intent(out)              :: array
real, dimension(num_regions), intent(in)        :: wlon_in
real, dimension(num_regions), intent(in)        :: elon_in
real, dimension(num_regions), intent(in)        :: slat
real, dimension(num_regions), intent(in)        :: nlat
real, intent(in)                                :: set_value
real, intent(in)                                :: unset_value
character*(*), intent(in)                       :: name
logical, intent(in)                             :: coastal_only

!
!       local variables
!

integer :: i, j, n
real, dimension(:), allocatable :: wlon
real, dimension(:), allocatable :: elon

!
!       save the longitudes in case they need to be modified
!

allocate(wlon(num_regions))
allocate(elon(num_regions))

wlon(:) = wlon_in(:)
elon(:) = elon_in(:)

!
! loop over the regions, applying changes as necessary
!

do n = 1, num_regions  !{

  if (nlat(n) .ge. slat(n)) then  !{
    write (stdout(),*)
    write (stdout(),*) trim(note_header), ' ', trim(name), ' region: ', n

!
!       make sure that all longitudes are in the range [0,360]
!

    do while (wlon(n) .gt. 360.0)  !{
      wlon(n) = wlon(n) - 360.0
    enddo  !}
    do while (wlon(n) .lt. 0.0)  !{
      wlon(n) = wlon(n) + 360.0
    enddo  !}
    do while (elon(n) .gt. 360.0)  !{
      elon(n) = elon(n) - 360.0
    enddo  !}
    do while (elon(n) .lt. 0.0)  !{
      elon(n) = elon(n) + 360.0
    enddo  !}

!
!       if the southern and northern latitudes are the same, then
!       find the grid box which encompasses them ...
!

    if (slat(n) .eq. nlat(n)) then  !{

     call mpp_error(FATAL, trim(error_header) // ' Equal latitudes not supported')

    elseif (wlon(n) .eq. elon(n)) then  !}{

     call mpp_error(FATAL, trim(error_header) // ' Equal longitudes not supported')

    else  !}{

!
!       ... else find all boxes where the center lies in the
!       rectangular region
!

      do j = domain%jsd, domain%jed  !{
        do i = domain%isd, domain%ied  !{
          if (nlat(n) .ge. grid%yt(i,j) .and.                   &
              slat(n) .le. grid%yt(i,j) .and.                   &
              lon_between(grid%xt(i,j), wlon(n), elon(n))) then  !{
            age(index)%bc(i,j,l) = set_value
          endif  !}
        enddo  !} i
      enddo  !} j

    endif  !}

  endif  !}

enddo  !} n

!
!       if desired only apply mask to coastal regions
!

if (coastal_only) then  !{
  do j = domain%jsd, domain%jed  !{
    do i = domain%isd, domain%ied  !{
      if (grid%kmt(i,j) .ne. 0 .and.                            &
          age(index)%bc(i,j,l) .eq. set_value) then  !{

!
!       if all the surrounding points are ocean, then this is not
!       a coastal point, therefore reset the mask
!

        if (grid%kmt(i-1,j) .ne. 0 .and.                        &
            grid%kmt(i+1,j) .ne. 0 .and.                        &
            grid%kmt(i,j-1) .ne. 0 .and.                        &
            grid%kmt(i,j+1) .ne. 0) then  !{
          age(index)%bc(i,j,l) = unset_value
        endif  !}
      endif  !}
    enddo  !} i
  enddo  !} j
endif  !}

!
!       clean up
!

deallocate(wlon)
deallocate(elon)

return

contains

!
!       Return true if w <= x_in <= e, taking into account the
!       periodicity of longitude.
!
!       x_in    value to test
!       w       west longitude of boundary
!       e       east longitude of boundary
!

function lon_between(x_in, w, e)  !{

!
!       function definition
!

logical :: lon_between

!
!       arguments
!

real, intent(in)                :: x_in
real, intent(in)                :: w
real, intent(in)                :: e

!
!       local variables
!

real                    :: x

!
!       Save input values so we may modify them safely
!

x = x_in

!
!       make sure that all longitudes are in the range [0,360]
!

do while (x .gt. 360.0)  !{
  x = x - 360.0
enddo  !}
do while (x .lt. 0.0)  !{
  x = x + 360.0
enddo  !}
 
if (w .gt. e) then  !{
  lon_between = w .le. x .or. x .le. e
else  !}{
  lon_between = w .le. x .and. x .le. e
endif  !}

return

end function  lon_between  !}

end subroutine  set_array  !}
! </SUBROUTINE> NAME="set_array"

end module ocean_age_tracer_mod  !}
