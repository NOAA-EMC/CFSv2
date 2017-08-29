module godas_clim_rstr_mod
!
!<CONTACT EMAIL="david.behringer@noaa.gov"> David Behringer
!</CONTACT>
!
!<OVERVIEW>
! This module manages restoration fields for GODAS.
!</OVERVIEW>
!
!<DESCRIPTION>
! This module manages restoration fields for GODAS.
! Initialization should be called after GODAS initialization.
!</DESCRIPTION>
!
use fms_mod,                    only: FATAL, WARNING, NOTE, stdout, stdlog
use fms_mod,                    only: file_exist, read_data
use mpp_mod,                    only: mpp_error, mpp_pe
use mpp_io_mod,                 only: mpp_open, mpp_close
use mpp_io_mod,                 only: MPP_WRONLY, MPP_RDONLY
use ocean_types_mod,            only: ocean_grid_type, ocean_domain_type
use ocean_types_mod,            only: ocean_time_type
use ocean_parameters_mod,       only: missing_value
use ocean_domains_mod,          only: get_local_indices
use time_interp_external_mod,   only: time_interp_external, init_external_field
! use diag_manager_mod,           only: register_diag_field
use time_manager_mod,           only: get_date

use godas_types_mod,            only: ocean_prog_tracer_type
use godas_types_mod,            only: ocean_clim_rstr_tracer_type
use godas_data_mod,             only: num_c_rstr_tracers, id_c_rstr
use godas_data_mod,             only: tz_c_damp, sz_c_damp
!
implicit none

private

logical :: godas_clim_rstr_module_initialized = .false.

character(len=256) :: version = '$Id: godas_clim_rstr.F90,v 1.0 2008/01/05 12:28:00 gtn Exp $'
character(len=256) :: tagname = 'Tag $Name: gds1p0d $'
character(len=48), parameter          :: mod_name = 'godas_clim_rstr_mod'

#include <ocean_memory.h>

type(ocean_grid_type), pointer   :: Grd =>NULL()
type(ocean_domain_type), pointer :: Dom =>NULL()

integer, allocatable, dimension(:) :: id_tie_c_rstr
integer         :: index_temp=-1
integer         :: index_salt=-1

type data_type
   character(len=3) :: gridname
   character(len=128) :: fieldname_code ! used in user's code (e.g. temp, salt)
   character(len=128) :: fieldname_file ! fieldname used in the data file (not used)
   character(len=128) :: file_name      ! name of data file
   logical :: ongrid                    ! false, not relevant, here for compatibility
   real :: factor                       ! For unit conversion, default=1
end type data_type

integer, parameter :: max_table=10
real, allocatable, dimension(:,:,:) :: rdata

type(data_type), dimension(max_table) :: data_table

! for ascii output
integer :: unit=6

public  godas_clim_rstr_init
public  godas_clim_rstr_comp

contains

!#######################################################################
! <FUNCTION NAME="godas_clim_rstr_init">
!
! <DESCRIPTION>
! Initialization code for restoration fields
! </DESCRIPTION>
!
function godas_clim_rstr_init (Grid, Domain, Time, T_prog) result (T_c_rstr)

  type(ocean_grid_type), intent(in), target   :: Grid
  type(ocean_domain_type), intent(in), target :: Domain
  type(ocean_time_type), intent(in)           :: Time
  type(ocean_prog_tracer_type), intent(in)    :: T_prog(:)

  ! return value
  type(ocean_clim_rstr_tracer_type), dimension(:), pointer :: T_c_rstr

  integer               :: n, nf, ntable, num_c_rstr, num_c_rstr_files
  integer               :: nu, pe, num_prog_tracers
  character(len=256)    :: record
  type(data_type)       :: default_table, data_entry

  character(len=48),  parameter :: sub_name = 'godas_clim_rstr_init'
  character(len=256), parameter :: error_header = '==>Error from ' // trim(mod_name) // &
                                                  '(' // trim(sub_name) // '): '
  character(len=256), parameter :: warn_header = '==>Warning from ' // trim(mod_name) // &
                                                 '(' // trim(sub_name) // '): '
  character(len=256), parameter :: note_header = '==>Note from ' // trim(mod_name) // &
                                                 '(' // trim(sub_name) // '): '

  real, dimension(2)                    :: range_array

  if (godas_clim_rstr_module_initialized) then
    call mpp_error(FATAL, trim(error_header) // ' GODAS CLIM RSTR already initialized')
  endif

  pe = mpp_pe()

  nullify(T_c_rstr)

  if (num_c_rstr_tracers < 1) return

  ! allocate T_c_rstr
  allocate( T_c_rstr  (num_c_rstr_tracers) )

  do n=1,num_c_rstr_tracers-1
    T_c_rstr(n)%complete=.false.
  enddo
  T_c_rstr(num_c_rstr_tracers)%complete=.true.

  Grd => Grid
  Dom => Domain

  call get_local_indices(Dom, isd, ied, jsd, jed, isc, iec, jsc, jec)
  nk=Grd%nk

  do n=1,num_c_rstr_tracers
#ifndef STATIC_MEMORY
    allocate( T_c_rstr(n)%frstr(isd:ied,jsd:jed,nk) )
#endif
    T_c_rstr(n)%frstr(:,:,:)        = 0.0
  enddo

  num_prog_tracers = size(T_prog)
  do n=1, num_prog_tracers
     if (T_prog(n)%name == 'temp') index_temp = n
     if (T_prog(n)%name == 'salt') index_salt = n
  enddo

  allocate( rdata (isd:ied,jsd:jed,nk) )
  allocate( id_c_rstr (num_c_rstr_tracers) )
  allocate( id_tie_c_rstr (num_c_rstr_tracers) )
  id_c_rstr(:) = -1
  id_tie_c_rstr(:) = -1

! initialize data table
  default_table%gridname = 'none'
  default_table%fieldname_code = 'none'
  default_table%fieldname_file = 'none'
  default_table%file_name = 'none'
  default_table%ongrid = .FALSE.
  default_table%factor = 1.
  do n = 1,max_table
    data_table(n) = default_table
  enddo

! read restoration table
  call mpp_open(nu, 'data_table', action=MPP_RDONLY)
  ntable = 0
  num_c_rstr_files = 0
  do while (.true.)
    read(nu,'(a)',end=9) record
    if (record(1:1) == '#') cycle
    if (record(1:10) == '          ') cycle
    read(record,*,err=7) data_entry
    if (data_entry%gridname(1:3) .eq. 'CRS') then
      ntable=ntable+1
      data_table(ntable) = data_entry
    endif
  enddo
7 call mpp_error(FATAL,'error reading data_table')
9 continue
  call mpp_close(nu)
  if (ntable .eq. 0) then
    call mpp_error(FATAL,'no CRS entry in data_table')
  endif
  num_c_rstr_files = 0
  do nf=1,ntable
    if (data_table(nf)%fieldname_code(1:4) .eq. 'temp') num_c_rstr_files = num_c_rstr_files + 1
    if (data_table(nf)%fieldname_code(1:4) .eq. 'salt') num_c_rstr_files = num_c_rstr_files + 1
  enddo

  num_c_rstr = 0
  if (num_c_rstr_files .gt. 0) then
    do nf=1,ntable
      if (data_table(nf)%fieldname_code(1:4) .eq. 'temp') then
        if (file_exist(trim(data_table(nf)%file_name))) then
            id_tie_c_rstr(index_temp) = init_external_field(trim(data_table(nf)%file_name), &
                 trim(T_prog(index_temp)%name), domain=Dom%domain2d)
            write(stdout(),*) '==>Note from GODAS: applying surface temp restoring'
            if (id_tie_c_rstr(index_temp) == -1) call mpp_error(FATAL,'==>Error in GODAS: cannot find restore field')
        endif
        num_c_rstr = num_c_rstr + 1
      else if (data_table(nf)%fieldname_code(1:4) .eq. 'salt') then
        if (file_exist(trim(data_table(nf)%file_name))) then
            id_tie_c_rstr(index_salt) = init_external_field(trim(data_table(nf)%file_name), &
                 trim(T_prog(index_salt)%name), domain=Dom%domain2d)
            write(stdout(),*) '==>Note from GODAS: applying surface salt restoring'
            if (id_tie_c_rstr(index_salt) == -1) call mpp_error(FATAL,'==>Error in GODAS: cannot find restore field')
        endif
        num_c_rstr = num_c_rstr + 1
      endif
    enddo
  endif

  if (num_c_rstr /= num_c_rstr_tracers) call mpp_error(FATAL,'==>Error in GODAS: number of restore fields in namelist and data_table disagree')

! ----------------------------------------------
! register diagnostics
! ----------------------------------------------
!
  do n=1,num_c_rstr_tracers
    if (n == index_temp) then
      T_c_rstr(n)%name='tcrstr'
      T_c_rstr(n)%units='Deg_C'
      T_c_rstr(n)%longname='potential temperature restoration'
      T_c_rstr(n)%min_range=-10.0
      T_c_rstr(n)%max_range=100.0
      T_c_rstr(n)%init=.false.
      T_c_rstr(n)%file_in='INPUT/ocean_c_rstr.res.nc'
      T_c_rstr(n)%file_out='RESTART/ocean_c_rstr.res.nc'
      T_c_rstr(n)%name_in='tcrstr'
    else if (n == index_salt) then
      T_c_rstr(n)%name='scrstr'
      T_c_rstr(n)%units='psu'
      T_c_rstr(n)%longname='salinity restoration'
      T_c_rstr(n)%min_range=-10.0
      T_c_rstr(n)%max_range=100.0
      T_c_rstr(n)%init=.false.
      T_c_rstr(n)%file_in='INPUT/ocean_c_rstr.res.nc'
      T_c_rstr(n)%file_out='RESTART/ocean_c_rstr.res.nc'
      T_c_rstr(n)%name_in='scrstr'
    endif
  enddo

  godas_clim_rstr_module_initialized = .true.

end function godas_clim_rstr_init
! </FUNCTION> NAME="godas_clim_rstr_init">


! <SUBROUTINE NAME="godas_clim_rstr_comp">
!
! <DESCRIPTION>
! Computes the surface restoration fields.
! </DESCRIPTION>
!
subroutine godas_clim_rstr_comp (Time, T_prog, T_c_rstr)

  type(ocean_time_type), intent(in)            :: Time
  type(ocean_prog_tracer_type), intent(in)     :: T_prog(:)
  type(ocean_clim_rstr_tracer_type), intent(inout)  :: T_c_rstr(:)

  integer         :: i, j, k, n, taup1
  integer         :: year, month, day, hour, minute, second

  taup1   = Time%taup1

! use near-frazil condition as a proxy for where sea-ice is present

  call get_date(Time%model_time, year, month, day, hour, minute, second)

! T climate restoration
  
  if (id_tie_c_rstr(index_temp) > 0) then
    call time_interp_external(id_tie_c_rstr(index_temp), Time%model_time, rdata)
    do k = 1,nk
      do j = jsc,jec
        do i = isc,iec
          T_c_rstr(index_temp)%frstr(i,j,k) = &
            tz_c_damp*Grd%tmask(i,j,k)*(rdata(i,j,k)-T_prog(index_temp)%field(i,j,k,taup1))
        enddo
      enddo
    enddo
  endif
  
! S climate restoration
  
  if (id_tie_c_rstr(index_salt) > 0) then
    call time_interp_external(id_tie_c_rstr(index_salt), Time%model_time, rdata)
    do k = 1,nk
      do j = jsc,jec
        do i = isc,iec
          T_c_rstr(index_salt)%frstr(i,j,k) = &
            sz_c_damp*Grd%tmask(i,j,k)*(rdata(i,j,k)-T_prog(index_salt)%field(i,j,k,taup1))
        enddo
      enddo
    enddo
  endif

  return

end subroutine godas_clim_rstr_comp
! </SUBROUTINE> NAME="godas_clim_rstr_comp"

end module godas_clim_rstr_mod
