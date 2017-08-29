module godas_mod
!
!<CONTACT EMAIL="david.behringer@noaa.gov"> David Behringer
!</CONTACT>
!
!<OVERVIEW>
! This module conducts an assimilation cycle.
!</OVERVIEW>
!
!<DESCRIPTION>
! This module conducts an assimilation cycle.
! Initialization for godas is done as well.
!</DESCRIPTION>
!
!<NAMELIST NAME="godas_nml">
!
!  <DATA NAME="debug_godas" TYPE="logical">
!  For debugging the godas module.
!  </DATA>
!</NAMELIST>
!
use fms_mod,           only: read_data, write_data, open_namelist_file, check_nml_error, close_file
use fms_mod,           only: FATAL, WARNING, NOTE, stdout, stdlog
use mpp_mod,           only: mpp_error, mpp_pe, mpp_npes, mpp_root_pe
use mpp_mod,           only: mpp_sync, ALL_PES
use mpp_domains_mod,   only: mpp_update_domains
use mpp_domains_mod,   only: mpp_global_sum, BITWISE_EXACT_SUM
use mpp_comm_mod,      only: mpp_broadcast, mpp_transmit
use mpp_domains_util_mod,      only: mpp_get_compute_domains
use mpp_io_mod,        only: mpp_open, mpp_close
use mpp_io_mod,        only: MPP_WRONLY, MPP_RDONLY, MPP_IEEE32, MPP_DIRECT, MPP_SEQUENTIAL
use mpp_io_mod,        only: MPP_SINGLE, MPP_MULTI
use time_manager_mod,  only: time_type, set_date, get_date, set_time, get_time, print_date
use time_manager_mod,  only: increment_time, decrement_time, repeat_alarm
use time_manager_mod,  only: operator(-), operator(>), operator(<)
use diag_manager_mod,  only: register_diag_field, send_data
use constants_mod,     only: pi

use ocean_domains_mod,          only: get_local_indices, get_global_indices
use ocean_types_mod,            only: ocean_grid_type, ocean_domain_type
use ocean_types_mod,            only: ocean_prog_tracer_type
use ocean_types_mod,            only: ocean_external_mode_type
use ocean_types_mod,            only: ocean_time_type
use ocean_types_mod,            only: missing_value
use godas_types_mod,            only: ocean_cor_tracer_type
use godas_types_mod,            only: ocean_obsz_type, ocean_obs0_type
use godas_data_mod,             only: num_cor_tracers
use godas_data_mod,             only: kass, kass2, nwkobs, maxits, npits
use godas_data_mod,             only: no_asm_rep, asm_cnt
use godas_data_mod,             only: gds_step, acoef, hrscl, vcvn, vsclf
use godas_data_mod,             only: xcb, xce, xcsz, ycb, yce, ycsz
use godas_data_mod,             only: s2, s1, wgns, elipt
use godas_data_mod,             only: wcn, wea, wwe, wso, wno, wgta
use godas_data_mod,             only: cvn, cvnsalt, vtmp, vsal, covsr, ev, wrkk
use godas_data_mod,             only: eta_clm, cdnz, cdnzs
use godas_data_mod,             only: num_obsz, num_obs0, num_obsa
use godas_data_mod,             only: temp_code, salt_code, sst_code, sss_code, altm_code
use godas_data_mod,             only: dtemp_max, dtemp_elm, dsalt_max, dsalt_elm
use godas_data_mod,             only: daltm_max, daltm_elm
use godas_data_mod,             only: tz_wndw_fwd, tz_wndw_bwd, rtzw
use godas_data_mod,             only: sz_wndw_fwd, sz_wndw_bwd, rszw
use godas_data_mod,             only: t0_wndw_fwd, t0_wndw_bwd, rt0w
use godas_data_mod,             only: s0_wndw_fwd, s0_wndw_bwd, rs0w
use godas_data_mod,             only: al_wndw_fwd, al_wndw_bwd, ralw
use godas_data_mod,             only: g_cg, d_cg, f_cg, e_cg, t_cg, h_cg
use godas_data_mod,             only: gds_freq, alrm_dur
use godas_data_mod,             only: assrestrt, debug_godas, ovr_alrm
use godas_data_mod,             only: spd
!
implicit none

private

logical :: godas_module_initialized = .false.

character(len=256) :: version = '$Id: godas.F90,v 1.0 2006/11/21 08:47:00 gtn Exp $'
character(len=256) :: tagname = 'Tag $Name: gds2p0d $'
character(len=48), parameter          :: mod_name = 'godas_mod'

#include <ocean_memory.h>

type(ocean_grid_type), pointer   :: Grd =>NULL()
type(ocean_domain_type), pointer :: Dom =>NULL()

integer         :: index_temp
integer         :: index_salt

type data_type
   character(len=3) :: gridname
   character(len=128) :: fieldname_code ! used in user's code (e.g. mdl_tvv, mdl_svv, etc.)
   character(len=128) :: fieldname_file ! fieldname used in the data file (not used)
   character(len=128) :: file_name      ! name of data file
   logical :: ongrid                    ! false, not relevant, here for compatibility
   real :: factor                       ! For unit conversion, default=1
end type data_type

integer, parameter :: max_table=10

type(data_type), dimension(max_table) :: data_table

real          :: aeval, dbsq

! for diagnostics
logical :: used
integer, allocatable, dimension(:) :: id_cor

! for ascii output
integer :: unit=6

public  godas_init
public  godas_increment
public  godas_end

namelist /godas_nml/ num_cor_tracers, kass, nwkobs, maxits, npits, &
                     gds_step, no_asm_rep, acoef, hrscl, vcvn, vsclf, &
                     tz_wndw_fwd, tz_wndw_bwd, sz_wndw_fwd, sz_wndw_bwd, &
                     t0_wndw_fwd, t0_wndw_bwd, s0_wndw_fwd, s0_wndw_bwd, &
                     al_wndw_fwd, al_wndw_bwd, assrestrt, debug_godas

contains


!#######################################################################
! <FUNCTION NAME="godas_init">
!
! <DESCRIPTION>
! Initialization code for godas, returning a pointer to
! the T_cor array.
! </DESCRIPTION>
!
function godas_init (Grid, Domain, Time, T_prog, num_cor, debug) &
                    result (T_cor)

  type(ocean_grid_type), intent(in), target   :: Grid
  type(ocean_domain_type), intent(in), target :: Domain
  type(ocean_time_type), intent(in)           :: Time
  type(ocean_prog_tracer_type), intent(in)    :: T_prog(:)
  integer, intent(out)                        :: num_cor

  logical, intent(in), optional               :: debug

  integer :: n

  ! return value
  type(ocean_cor_tracer_type), dimension(:), pointer :: T_cor

  integer               :: pe, ierr, mtss, mtsd
  integer               :: year, month, day, hour, minute, second
  integer               :: i, j, k, ig, jg, kg
  integer               :: num_prog_tracers
  integer               :: ioun, io_status
  character(len=256)    :: record
  type(data_type)       :: default_table, data_entry
  integer               :: nu, nf, ntable, num_var_files, num_alt_files, num_afx_files
  real(kind=4), dimension(:,:,:), allocatable   :: buf
  real(kind=4), dimension(:,:), allocatable   :: buf2
  real(kind=4), dimension(:), allocatable   :: buf1

  character(len=48),  parameter :: sub_name = 'godas_init'
  character(len=256), parameter :: error_header = '==>Error from ' // trim(mod_name) //   &
                                                  '(' // trim(sub_name) // '): '
  character(len=256), parameter :: warn_header = '==>Warning from ' // trim(mod_name) //  &
                                                 '(' // trim(sub_name) // '): '
  character(len=256), parameter :: note_header = '==>Note from ' // trim(mod_name) //     &
                                                 '(' // trim(sub_name) // '): '

  real, dimension(2)                    :: range_array

  if (godas_module_initialized) then
    call mpp_error(FATAL, trim(error_header) // ' GODAS already initialized')
  endif

  nullify(T_cor)

  write( stdlog(),'(/a/)') trim(version)

  num_prog_tracers = size(T_prog)
  do n=1, num_prog_tracers
     if (T_prog(n)%name == 'temp') index_temp = n
     if (T_prog(n)%name == 'salt') index_salt = n
  enddo

  pe = mpp_pe()

! set namelist defaults (see godas_data for descriptions)

  num_cor_tracers     = 2
  kass                = 30         ! standard for 40-level MOM; use 35 for deep
  nwkobs              = 5          ! standard for 5-day, 1-day runs
  maxits              = 3
  npits               = 200
  gds_step            = 43200
  no_asm_rep          = 3
  acoef               = 0.01
  hrscl               = 3.99
  vsclf               = 0.5
  vcvn                = 1.0
  tz_wndw_fwd         = 14
  tz_wndw_bwd         = 14
  sz_wndw_fwd         = 14
  sz_wndw_bwd         = 14
  t0_wndw_fwd         = 7
  t0_wndw_bwd         = 7
  s0_wndw_fwd         = 7
  s0_wndw_bwd         = 7
  al_wndw_fwd         = 14
  al_wndw_bwd         = 14
  assrestrt           = .true.
  debug_godas         = .false.

! provide for namelist over-ride

  ioun = open_namelist_file()
  read  (ioun, godas_nml,iostat=io_status)
  write (stdout(),'(/)')
  write (stdout(), godas_nml)
  write (stdlog(), godas_nml)
  ierr = check_nml_error(io_status,'godas_nml')
  call close_file (ioun)
  num_cor = num_cor_tracers
  kass2 = num_cor * kass

  if (tz_wndw_fwd > tz_wndw_bwd) then
    rtzw = 1.0 / real(tz_wndw_fwd)
  else
    rtzw = 1.0 / real(tz_wndw_bwd)
  endif
  if (sz_wndw_fwd > sz_wndw_bwd) then
    rszw = 1.0 / real(sz_wndw_fwd)
  else
    rszw = 1.0 / real(sz_wndw_bwd)
  endif
  if (t0_wndw_fwd > t0_wndw_bwd) then
    rt0w = 1.0 / real(t0_wndw_fwd)
  else
    rt0w = 1.0 / real(t0_wndw_bwd)
  endif
  if (s0_wndw_fwd > s0_wndw_bwd) then
    rs0w = 1.0 / real(s0_wndw_fwd)
  else
    rs0w = 1.0 / real(s0_wndw_bwd)
  endif
  if (al_wndw_fwd > al_wndw_bwd) then
    ralw = 1.0 / real(al_wndw_fwd)
  else
    ralw = 1.0 / real(al_wndw_bwd)
  endif

  ovr_alrm = assrestrt

  if (PRESENT(debug) .and. .not. debug_godas) then
    debug_godas = debug
  endif

  gds_freq = set_time(gds_step, 0)
  call get_time(Time%Time_step, mtss, mtsd)
  alrm_dur = set_time(mtss, 0)

  ! allocate T_cor
  allocate( T_cor  (num_cor_tracers) )
  allocate( id_cor (num_cor_tracers) )

  id_cor(:) = -1

  do n=1,num_cor_tracers-1
    T_cor(n)%complete=.false.
  enddo
  T_cor(num_cor_tracers)%complete=.true.

  ! set local array indices
  Grd => Grid
  Dom => Domain

  call get_local_indices(Dom, isd, ied, jsd, jed, isc, iec, jsc, jec)
  call get_global_indices(Dom, isg, ieg, jsg, jeg)
  nk=Grd%nk

  do n=1,num_cor_tracers
#ifndef STATIC_MEMORY
    allocate( T_cor(n)%fcor(isd:ied,jsd:jed,nk) )
#endif
    T_cor(n)%fcor(:,:,:)        = 0.0
  enddo
!
! ----------------------------------------------
! allocate various computational arrays
! ----------------------------------------------
!
  allocate (d_cg(isd:ied,jsd:jed,kass2))
  allocate (f_cg(isd:ied,jsd:jed,kass2))
  allocate (g_cg(isd:ied,jsd:jed,kass2))
  allocate (e_cg(isd:ied,jsd:jed,kass2))
  allocate (t_cg(isd:ied,jsd:jed,kass2))
  allocate (h_cg(isd:ied,jsd:jed,kass2))
!
! ----------------------------------------------
! begin calculating weights for horizontal smoother
! ----------------------------------------------
!
  call init_wghts()
!
! ----------------------------------------------
! compute vertical covariance matrix
!-----------------------------------------------
!
  allocate (cvn(kass,kass))
  allocate (cvnsalt(kass,kass))
!
  call vertical_covariance()
!
! ----------------------------------------------
! read in time-constant but geo-varying error variance
! ----------------------------------------------
!
  allocate (vtmp(isd:ied,jsd:jed,kass))
  allocate (vsal(isd:ied,jsd:jed,kass))
!
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

! read observations table
  call mpp_open(nu, 'data_table', action=MPP_RDONLY)
  ntable = 0
  num_var_files = 0
  num_alt_files = 0
  num_afx_files = 0
  do while (.true.)
    read(nu,'(a)',end=9) record
    if (record(1:1) == '#') cycle
    if (record(1:10) == '          ') cycle
    read(record,*,err=7) data_entry
    if (data_entry%gridname(1:3) .eq. 'OBS') then
      ntable=ntable+1
      data_table(ntable) = data_entry
    endif
  enddo
7 call mpp_error(FATAL,'error reading data_table')
9 continue
  call mpp_close(nu)
  if (ntable .eq. 0) then
    call mpp_error(FATAL,'no MODEL VARIANCE entry in data_table')
  endif
  num_var_files = 0
  do nf=1,ntable
    if (data_table(nf)%fieldname_code(1:7) .eq. 'mdl_tvv') num_var_files = num_var_files + 1
    if (data_table(nf)%fieldname_code(1:7) .eq. 'mdl_svv') num_var_files = num_var_files + 1
    if (data_table(nf)%fieldname_code(1:4) .eq. 'altm') num_alt_files = num_alt_files + 1
    if (data_table(nf)%fieldname_code(1:4) .eq. 'etac') num_afx_files = num_afx_files + 1
    if (data_table(nf)%fieldname_code(1:4) .eq. 'cdnz') num_afx_files = num_afx_files + 1
  enddo
  if (num_var_files .eq. 0) then
    call mpp_error(FATAL,'error: no model variance files')
  else if (num_var_files .ne. num_cor_tracers) then
    call mpp_error(FATAL,'error: no. of var files != no. of tracers to correct')
  else
    allocate (buf(isg:ieg,jsg:jeg,kass))
    do nf=1,ntable
      if (data_table(nf)%fieldname_code(1:7) .eq. 'mdl_tvv') then
        call mpp_open(nu,trim(data_table(nf)%file_name),action=MPP_RDONLY,form=MPP_IEEE32, &
                        access=MPP_SEQUENTIAL,threading=MPP_MULTI,fileset=MPP_SINGLE)
        read (nu) year, month, day
        read (nu) ig, jg, kg
        read (nu) buf
        close(nu)
!
        do k=1,kass
          do j=jsd,jed
            do i=isd,ied
              vtmp(i,j,k) = buf(i,j,k)
            enddo
          enddo
        enddo
!
      else if (data_table(nf)%fieldname_code(1:7) .eq. 'mdl_svv') then
        call mpp_open(nu,trim(data_table(nf)%file_name),action=MPP_RDONLY,form=MPP_IEEE32, &
                        access=MPP_SEQUENTIAL,threading=MPP_MULTI,fileset=MPP_SINGLE)
        read (nu) year, month, day
        read (nu) ig, jg, kg
        read (nu) buf
        close(nu)
!
        do k=1,kass
          do j=jsd,jed
            do i=isd,ied
              vsal(i,j,k) = buf(i,j,k)
            enddo
          enddo
        enddo
!
      endif
    enddo
    deallocate (buf)
  endif
!
  if (num_alt_files .gt. 0) then
    if (num_afx_files .ne. 2) then
      call mpp_error(FATAL,'altm entry in OBS data_table but no etac and/or no cdnz entry')
    endif
    allocate (cdnz(kass))
    allocate (cdnzs(kass))
    allocate (eta_clm(isd:ied,jsd:jed))
    allocate (buf1(kass))
    allocate (buf2(isg:ieg,jsg:jeg))
    do nf=1,ntable
      if (data_table(nf)%fieldname_code(1:4) .eq. 'cdnz') then
        call mpp_open(nu,trim(data_table(nf)%file_name),action=MPP_RDONLY,form=MPP_IEEE32, &
                        access=MPP_SEQUENTIAL,threading=MPP_MULTI,fileset=MPP_SINGLE)
        read (nu) buf1
        do k=1,kass
          cdnz(k) = buf1(k)
        enddo
        read (nu) buf1
        do k=1,kass
          cdnzs(k) = buf1(k)
        enddo
        close(nu)
        deallocate (buf1)
      else if (data_table(nf)%fieldname_code(1:4) .eq. 'etac') then
        call mpp_open(nu,trim(data_table(nf)%file_name),action=MPP_RDONLY,form=MPP_IEEE32, &
                        access=MPP_SEQUENTIAL,threading=MPP_MULTI,fileset=MPP_SINGLE)
        read (nu) buf2
        close(nu)
!
        do j=jsd,jed
          do i=isd,ied
            eta_clm(i,j) = buf2(i,j)
          enddo
        enddo
        deallocate (buf2)
      endif
    enddo
  endif
!
  allocate (covsr(kass,kass))
  allocate (ev(kass))
  allocate (wrkk(kass2))

! ----------------------------------------------
! register diagnostics
! ----------------------------------------------
!
  do n=1,num_cor_tracers
    if (n == index_temp) then
      T_cor(n)%name='tcor'
      T_cor(n)%units='Deg_C'
      T_cor(n)%longname='potential temperature correction'
      T_cor(n)%min_range=-10.0
      T_cor(n)%max_range=100.0
      T_cor(n)%init=.false.
      T_cor(n)%file_in='ocean_cor.res.nc'
      T_cor(n)%file_out='ocean_cor.res.nc'
      T_cor(n)%name_in='tcor'
    else if (n == index_salt) then
      T_cor(n)%name='scor'
      T_cor(n)%units='psu'
      T_cor(n)%longname='salinity correction'
      T_cor(n)%min_range=-10.0
      T_cor(n)%max_range=100.0
      T_cor(n)%init=.false.
      T_cor(n)%file_in='ocean_cor.res.nc'
      T_cor(n)%file_out='ocean_cor.res.nc'
      T_cor(n)%name_in='scor'
    endif
  enddo

  ! register diagnostics

  do n=1,num_cor_tracers
    range_array(1) = T_cor(n)%min_range
    range_array(2) = T_cor(n)%max_range
    id_cor(n) = register_diag_field ('ocean_model', trim(T_cor(n)%name), &
         Grd%tracer_axes(1:3),                                             &
         Time%model_time, trim(T_cor(n)%longname), trim(T_cor(n)%units), &
         missing_value=missing_value, range=range_array)
  enddo

  godas_module_initialized = .true.

end function godas_init
! </FUNCTION> NAME="godas_init">


!#######################################################################
! <SUBROUTINE NAME="godas_increment">
!
! <DESCRIPTION>
! Apply corrections from analysis.  Update analysis at specified interval.
! </DESCRIPTION>
!
subroutine godas_increment (Time, T_prog, Ext_mode, T_cor, obs_Z, obs_0, obs_A)

  type(ocean_time_type), intent(in)                    :: Time
  type(ocean_prog_tracer_type), intent(inout)          :: T_prog(:)
  type(ocean_external_mode_type), intent(inout)        :: Ext_mode
  type(ocean_cor_tracer_type), intent(inout)           :: T_cor(num_cor_tracers)
  type(ocean_obsz_type), intent(inout)                 :: obs_Z(:)
  type(ocean_obs0_type), intent(inout)                 :: obs_0(:)
  type(ocean_obs0_type), intent(inout)                 :: obs_A(:)

  integer         :: n, taup1, pe
  integer         :: year, month, day, hour, minute, second

  pe = mpp_pe()

  call get_date(Time%model_time, year, month, day, hour, minute, second)

  if (repeat_alarm(Time%model_time, gds_freq, alrm_dur) .or. ovr_alrm) then
    call godas_analysis (Time, T_prog, Ext_mode, T_cor, obs_Z, obs_0, obs_A)
    if (ovr_alrm) then
      asm_cnt = 2
      ovr_alrm = .false.
    else
      asm_cnt = 1
    endif
if (pe .eq. 0) then
call print_date(Time%model_time,'GODAS:',6)
endif
write(6,'(a,i6)') 'GODAS: NEW for increment ocean state ALARM',asm_cnt
  else if (asm_cnt .lt. no_asm_rep) then
    call godas_analysis (Time, T_prog, Ext_mode, T_cor, obs_Z, obs_0, obs_A)
    asm_cnt = asm_cnt + 1
if (pe .eq. 0) then
call print_date(Time%model_time,'GODAS:',6)
endif
write(6,'(a,i6)') 'GODAS: NEW for increment ocean state COUNT',asm_cnt
  else
    asm_cnt = asm_cnt + 1
if (pe .eq. 0) then
call print_date(Time%model_time,'GODAS:',6)
endif
write(6,'(a,i6)') 'GODAS: OLD for increment ocean state',asm_cnt
  endif

! apply increments

  taup1   = Time%taup1
  do n=1,num_cor_tracers
    T_prog(n)%field(:,:,:,taup1) = T_prog(n)%field(:,:,:,taup1) + T_cor(n)%fcor(:,:,:)
  enddo

! send corrections to diag_manager

  do n=1,num_cor_tracers
    if (id_cor(n) > 0) used = send_data (id_cor(n), T_cor(n)%fcor(:,:,:), &
                                Time%model_time,rmask=Grd%tmask(:,:,:), &
                                is_in=isc, js_in=jsc, ks_in=1, ie_in=iec, je_in=jec, ke_in=nk)
  enddo

end subroutine godas_increment
! </SUBROUTINE> NAME="godas_increment">


!#######################################################################
! <SUBROUTINE NAME="godas_analysis">
!
! <DESCRIPTION>
! Apply corrections from analysis.  Update analysis at specified interval.
! </DESCRIPTION>
!
subroutine godas_analysis (Time, T_prog, Ext_mode, T_cor, obs_Z, obs_0, obs_A)

  type(ocean_time_type), intent(in)                    :: Time
  type(ocean_prog_tracer_type), intent(inout)          :: T_prog(:)
  type(ocean_external_mode_type), intent(inout)        :: Ext_mode
  type(ocean_cor_tracer_type), intent(inout)           :: T_cor(num_cor_tracers)
  type(ocean_obsz_type), intent(inout)                 :: obs_Z(:)
  type(ocean_obs0_type), intent(inout)                 :: obs_0(:)
  type(ocean_obs0_type), intent(inout)                 :: obs_A(:)

  integer         :: pe, n
  integer         :: i, j, k, iter
  real            :: alpha, beta, gh_old, gh_new, df_val
integer :: ii, jj

  ni    = Grd%ni
  nj    = Grd%nj
  nk    = Grd%nk
  pe = mpp_pe()

!-----------------------------------------------------------------------
!  Find the first interation of the gradient of the functional (g^1)
!  by setting the intial guess for the correction field to zero (T^1 = 0),
!  comparing the model with the observations, weighting their difference
!  with the inverse of the observation error covariance (F) and projecting
!  this onto the model grid.
!       T^1 = 0
!       g^1 = -trnsD invF To
!-----------------------------------------------------------------------
!
  t_cg = 0.0

  call init_grad (Time, T_prog, Ext_mode, obs_Z, obs_0, obs_A)

!-----------------------------------------------------------------------
!  Do the first multiplication of the gradient by the background
!  error covariance matrix (E).
!       h^1 = E g^1
!  In this version a laplace smoother is used.
!-----------------------------------------------------------------------

  call eg_lpsmthr ()

!-----------------------------------------------------------------------
!  Set the initial search directions to zero.
!       d^0 = 0
!       e^0 = 0
!-----------------------------------------------------------------------

  d_cg = 0.0
  e_cg = 0.0

!-----------------------------------------------------------------------
!  Set the initial value of beta to zero
!-----------------------------------------------------------------------

  beta = 0.0

!-----------------------------------------------------------------------
!  Begin the iteration loop
!-----------------------------------------------------------------------

maxits=3
  do iter=1,maxits

!-----------------------------------------------------------------------
!  Update the search directions
!-----------------------------------------------------------------------

    d_cg = beta * d_cg - h_cg
    e_cg = beta * e_cg - g_cg

!-----------------------------------------------------------------------
!  Compute f
!      f^n = e^n + trnsD invF D d^n
!-----------------------------------------------------------------------

    call comp_f (obs_Z, obs_0, obs_A)

!-----------------------------------------------------------------------
!  Compute the inner products <g,h  and <d,f and update alpha
!  (only over the computational part of the processor domain)
!-----------------------------------------------------------------------

    gh_new = mpp_global_sum(Dom%domain2d,g_cg(:,:,:)*h_cg(:,:,:)*Grd%tmask(:,:,:),BITWISE_EXACT_SUM)
    df_val = mpp_global_sum(Dom%domain2d,d_cg(:,:,:)*f_cg(:,:,:)*Grd%tmask(:,:,:),BITWISE_EXACT_SUM)

    alpha = gh_new / df_val

!-----------------------------------------------------------------------
!  Update the field correction (T) and the gradient (g)
!      T^(n+1) = T^n + alpha d^n
!      g^(n+1) = g^n + alpha f^n
!-----------------------------------------------------------------------

    t_cg = t_cg + alpha * d_cg

    if (iter .lt. maxits) then
      g_cg = g_cg + alpha * f_cg

!-----------------------------------------------------------------------
!  Update h by multiplying the new gradient ( g^(n+1) ) by the
!  background error covariance E.
!       h^(n+1) = E g^(n+1)
!  In this version a laplace smoother is used.
!-----------------------------------------------------------------------

      call eg_lpsmthr ()

!-----------------------------------------------------------------------
!  Compute a new inner product <g,h and update beta
!  (only over the computational part of the processor domain)
!-----------------------------------------------------------------------

      gh_old = gh_new
      gh_new = mpp_global_sum(Dom%domain2d,g_cg(:,:,:)*h_cg(:,:,:)*Grd%tmask(:,:,:),BITWISE_EXACT_SUM)

      beta = gh_new / gh_old

    endif
  enddo

  do n=1,num_cor_tracers
    T_cor(n)%fcor(:,:,:) = 0.0
    if (n .eq. index_temp) then
      do k=1,kass
        T_cor(n)%fcor(:,:,k) = t_cg(:,:,k)
      enddo
    else if (n .eq. index_salt) then
      do k=1,kass
        T_cor(n)%fcor(:,:,k) = t_cg(:,:,k+kass)
      enddo
    endif
  enddo

end subroutine godas_analysis
! </SUBROUTINE> NAME="godas_analysis"


!#######################################################################
! <SUBROUTINE NAME="godas_end">
!
! <DESCRIPTION>
! Write GODAS restarts
! </DESCRIPTION>
!
subroutine godas_end(Time, T_cor)

  integer :: pe

  type(ocean_time_type)         :: Time
  type(ocean_cor_tracer_type)   :: T_cor(:)

  pe = mpp_pe()
  write(stdout(), '(a)') 'Later a GODAS restart will be written'

end subroutine godas_end
! </SUBROUTINE> NAME="godas_end"


!!#######################################################################
!! <SUBROUTINE NAME="init_wghts">
!!
!! <DESCRIPTION>
!! Compute the the weights for the lp-smoother
!! </DESCRIPTION>
!!
subroutine init_wghts

  integer       :: pe, pes, npes, len
  integer       :: i, j, n, ii, jj, np, npid2
  integer       :: jbg, jfn, jgbg, jgfn
  real          :: cuj, cujm, ctjr
  real          :: re, re2, con, col, acon, wsnd(2)
  real          :: b2                    !  = (hrscl * pi / 360.0)**2


  pe = mpp_pe()
  npes = mpp_npes()
  ni    = Grd%ni
  nj    = Grd%nj
  nk    = Grd%nk

  allocate (xcb(npes),xce(npes),xcsz(npes))
  allocate (ycb(npes),yce(npes),ycsz(npes))
  call mpp_get_compute_domains(Dom%domain2d,xcb,xce,xcsz,ycb,yce,ycsz)

  aeval = 0.0
  do n=1,npits-1
    aeval=1./float(n)+aeval
  enddo
  aeval=aeval/float(npits)
  dbsq = 0.5*aeval*aeval

  re = 6370.0e3
  re2 = re**2

! hrscl = hrscl * 111.324e3
! b2 = 0.25*hrscl**2
  b2 = (hrscl * pi / 360.0)**2
  acon = b2/float(npits)
  if (pe .eq. 0) write(6,'(a,i3,1p4e12.3)') 'GODAS_C',pe, b2, acon, aeval, acoef

  allocate( s1(isd:ied,jsd:jed) )
  allocate( s2(isd:ied,jsd:jed) )
  allocate( wgns(isd:ied,jsd:jed) )
  s1 = 0.0
  s2 = 0.0
  wgns = 1.0

  allocate( elipt(isd:ied,jsd:jed) )
  elipt = 1.0
  do j = jsd, jed
    do i = isd,ied
      if (Grd%yt(i,j) .lt. -10.0) then
        elipt(i,j) = 1.0 - 0.75*exp(-((Grd%yt(i,j)+10.0)**2/900.0))
      else if (Grd%yt(i,j) .gt. 10.0) then
        elipt(i,j) = 1.0 - 0.75*exp(-((Grd%yt(i,j)-10.0)**2/900.0))
      else
        elipt(i,j) = 0.25
      endif
    enddo
  enddo

  allocate( wgta(isd:ied,jsd:jed) )
  allocate( wcn(isd:ied,jsd:jed) )
  allocate( wea(isd:ied,jsd:jed) )
  allocate( wwe(isd:ied,jsd:jed) )
  allocate( wno(isd:ied,jsd:jed) )
  allocate( wso(isd:ied,jsd:jed) )
  wcn(:,:) = 0.0
  wea(:,:) = 0.0
  wwe(:,:) = 0.0
  wno(:,:) = 0.0
  wso(:,:) = 0.0
  do j = jsc,jec
    do i = isc,iec
      cuj = cos(Grd%phiu(i,j))
      cujm = cos(Grd%phiu(i,j-1))
      ctjr = 1.0/cos(Grd%phit(i,j))
      wea(i,j) = re2*cuj*cuj*ctjr*Grd%dxter(i,j)*Grd%dxtr(i,j)*acon/elipt(i,j)
      wno(i,j) = re2*cuj*cuj*ctjr*Grd%dytnr(i,j)*Grd%dytr(i,j)*acon
      wwe(i,j) = re2*cuj*cuj*ctjr*Grd%dxter(i-1,j)*Grd%dxtr(i,j)*acon/elipt(i,j)
      wso(i,j) = re2*cujm*cuj*ctjr*Grd%dytnr(i,j-1)*Grd%dytr(i,j)*acon
    enddo
  enddo
  call mpp_update_domains (wea(:,:), Dom%domain2d)
  call mpp_update_domains (wno(:,:), Dom%domain2d)
  call mpp_update_domains (wwe(:,:), Dom%domain2d)
  call mpp_update_domains (wso(:,:), Dom%domain2d)
  do j = jsd, jed
    do i = isd,ied
      wcn(i,j) = 1.0 - wso(i,j) - wno(i,j) - wea(i,j) - wwe(i,j)
    enddo
  enddo
  if (jsc .eq. jsg) then
    do i=isc,iec
      con = wso(i,jsc)
      col = con*con/((con+aeval)*con+dbsq)
      wcn(i,jsc) = wcn(i,jsc)+con*col
    enddo
  endif
  if (jec .eq. jeg) then
    do i=isc,iec
      con = wno(i,jec)
      col = con*con/((con+aeval)*con+dbsq)
      wcn(i,jec) = wcn(i,jec)+con*col
    enddo
  endif

  npid2=npits/2
  
  jgbg = 2
  jgfn = jeg - 1
  jbg = jsc
  if (jbg .eq. 1) jbg = 2
  jfn = jec
  if (jfn .eq. nj) jfn = nj - 1
 ii = (isg+ieg)/2
  do jj=jgbg,jgfn
!   do ii=isg,ieg
      s1(:,:) = 0.0
      s2(:,:) = 0.0
      pes = -1
      do n=1,npes
        if (ii .ge. xcb(n) .and. ii .le. xce(n) .and. jj .ge. ycb(n) .and. jj .le. yce(n)) then
          pes = n - 1
        endif
      enddo
      if (pes .lt. 0) call mpp_error(FATAL,'ERROR in INIT_WGHTS')
      if (pe .eq. pes) s1(ii,jj) = 1.0
      do np=1,npid2
        do j=jbg,jfn
          do i=isc,iec
            s2(i,j) = wcn(i,j) * s1(i,j) + wso(i,j) * s1(i,j-1) + wno(i,j) * s1(i,j+1) &
                                           + wwe(i,j) * s1(i-1,j) + wea(i,j) * s1(i+1,j)
          enddo
        enddo
 ! DBG
! call mpp_sync()
 ! DBG
        call mpp_update_domains (s2, Dom%domain2d)
        do j=jbg,jfn
          do i=isc,iec
            s1(i,j) = wcn(i,j) * s2(i,j) + wno(i,j-1) * s2(i,j-1) + wso(i,j+1) * s2(i,j+1) &
                                           + wwe(i,j) * s2(i-1,j) + wea(i,j) * s2(i+1,j)
          enddo
        enddo
 ! DBG
! call mpp_sync()
 ! DBG
        call mpp_update_domains (s1, Dom%domain2d)
      enddo
      wsnd = 0.0
      if (pe .eq. pes) then
        wsnd(1) = s1(ii,jj)
      endif
      len = 2
!     call mpp_transmit (wsnd,len,ALL_PES,wsnd,len,pes)
      call mpp_broadcast(wsnd,len,pes)
      if (jj .ge. jbg .and. jj .le. jfn) then
        do i=isc,iec
          wgns(i,jj) = wsnd(1)
        enddo
      endif
!   enddo
  enddo

 call mpp_sync()
  do j=jbg,jfn
    do i=isc,iec
      if (wgns(i,j) .lt. 0.0) then
        wgns(i,j) = 0.00001
      endif
      wgta(i,j)=sqrt(acoef/wgns(i,j))
    enddo
  enddo

  deallocate (elipt)
  deallocate(wgns)

end subroutine init_wghts
! </SUBROUTINE> NAME="init_wghts"


!!
!!#######################################################################
!! <SUBROUTINE NAME="vertical_covariance"
!!
!! <DESCRIPTION>
!! Compute the vertical covariance matrix.
!! A smoother is used.  The scale is proportional to layer thickness.
!! </DESCRIPTION>
!!
  subroutine vertical_covariance

  integer, parameter                :: kex = 10, nitz = 100
  integer                           :: k, kmp, kmpm1, kssm1, ksg
  integer                           :: ks, kk, n, ni2
  real                              :: scl, ascl 
  real, allocatable, dimension(:)   :: wm, wp, w, scz, scz2
!
  kmp = kass + 2*kex
  kmpm1 = kmp - 1
  kssm1 = kass - 1
!
  allocate ( wm(kmp) )
  allocate ( wp(kmp) )
  allocate ( w(kmp) )
  allocate ( scz(kmp) )
  allocate ( scz2(kmp) )
!
  do k=1,kssm1
    scl = vsclf*Grd%dzt(k)
    ascl = scl*scl / float(nitz);
    if (k .eq. 1) then
      wm(k+kex) = ascl / (2.0*Grd%dzw(k-1)*Grd%dzt(k))
    else
      wm(k+kex) = ascl / (Grd%dzw(k-1)*Grd%dzt(k))
    endif
    wp(k+kex) = ascl / (Grd%dzw(k)*Grd%dzt(k))
    w(k+kex) = 1.0 - wm(k+kex) - wp(k+kex)
  enddo
  do k=1,kex
    wm(k) = wm(kex+1)
    wp(k) = wp(kex+1)
    w(k) = w(kex+1)
  enddo
  do k=kass+kex,kmp
    wm(k) = wm(kex+kssm1)
    wp(k) = wp(kex+kssm1)
    w(k) = w(kex+kssm1)
  enddo
!
  ni2 = nitz / 2
!
  do ks=1,kass
    do k=1,kmp
      scz(k) = 0.0
    enddo
    ksg = ks + kex
    scz(ksg) = 1.0
    do n=1,ni2
      do k=2,kmpm1
        scz2(k) = w(k)*scz(k) + wm(k)*scz(k-1) + wp(k)*scz(k+1)
      enddo
      scz2(1) = scz2(2)
      scz2(kmp) = scz2(kmpm1)

      do k=2,kmpm1
       scz(k) = w(k)*scz2(k) + wp(k-1)*scz2(k-1) + wm(k+1)*scz2(k+1)
      enddo
      scz(1) = scz(2)
      scz(kmp) = scz(kmpm1)
    enddo
!
    ascl = scz(ksg)
    do k=1,kmp
      scz(k) = scz(k) / ascl
    enddo
!
    do k=1,kass
      cvn(ks,k) = scz(k+kex)
    enddo
  enddo
!
  do k=1,kass
    cvn(k,k) = vcvn
    do kk=k+1,kass
      ascl = 0.5*(cvn(k,kk) + cvn(kk,k))*vcvn
      cvn(k,kk) = ascl
      cvn(kk,k) = ascl
    enddo
  enddo
!
  cvnsalt = cvn
!
  deallocate ( wm )
  deallocate ( wp )
  deallocate ( w )
  deallocate ( scz )
  deallocate ( scz2 )
!
  end subroutine vertical_covariance
! </SUBROUTINE> NAME="vertical_covariance"

!!
!!#######################################################################
!! <SUBROUTINE NAME="init_grad">
!!
!! <DESCRIPTION>
!! Compute the initial estimate of the gradient of the functional (g)
!! </DESCRIPTION>
!!
subroutine init_grad (Time, T_prog, Ext_mode, obs_Z, obs_0, obs_A)
!
  type(ocean_time_type), intent(in)                 :: Time
  type(ocean_prog_tracer_type), intent(in)          :: T_prog(:)
  type(ocean_external_mode_type), intent(in)        :: Ext_mode
  type(ocean_obsz_type), intent(inout)              :: obs_Z(:)
  type(ocean_obs0_type), intent(inout)              :: obs_0(:)
  type(ocean_obs0_type), intent(inout)              :: obs_A(:)
!
  integer         :: i, ip, j, jp, k, kks, n, taup1, pe
  integer         :: year, month, day, hour, minute, second
  real            :: ov, aerr, aov
  type(time_type) :: diff_time, wndw_fwd, wndw_bwd
  integer         :: dsec, dday
  real            :: time_sep, time_adj
 integer :: tobc, sobc
 real :: tsum, ssum

!-----------------------------------------------------------------------
!  data types are encoded in obs%code
!      T(z)        1 <= code <= 10
!      S(z)       11 <= code <= 20
!      SST        21 <= code <= 22
!      SSS        23 <= code <= 23
!      ALTM       24 <= code <= 26
!  these are set in godas_data_mod, and can be modified if needed
!-----------------------------------------------------------------------
!
  pe = mpp_pe()

  taup1   = Time%taup1

!-----------------------------------------------------------------------
!   Set g to zero.
!-----------------------------------------------------------------------
!
  g_cg = 0.0
!
!-----------------------------------------------------------------------
!  For each observation
!   1) interpolate model forecast to observation position
!   2) compute innovation
!   3) adjust error inverse assigned to observation
!       i) adjust for time separation
!      ii) adjust for obs too far from model
!   4) multiply error inverse times inovation
!   5) project result back onto model grid
!   6) sum the gridded result in g_cg
!-----------------------------------------------------------------------
!  The profile observations
!-----------------------------------------------------------------------
!
 tobc = 0
 tsum = 0.0
 sobc = 0
 ssum = 0.0
  do n=1,num_obsz
    i = obs_Z(n)%io
    ip = i + 1
    j = obs_Z(n)%jo
    jp = j + 1
    if (obs_Z(n)%code .eq. temp_code) then
      wndw_fwd = increment_time(Time%model_time, 0, tz_wndw_fwd)
      wndw_bwd = decrement_time(Time%model_time, 0, tz_wndw_bwd)
      if (obs_Z(n)%obs_time < wndw_fwd .and. obs_Z(n)%obs_time > wndw_bwd) then
        diff_time = Time%model_time - obs_Z(n)%obs_time
        call get_time (diff_time, dsec, dday)
        time_sep = real(dday) + real(dsec)/real(spd)
        time_adj = (1.0-time_sep*rtzw)
        obs_Z(n)%win = .true.
        do k=1,obs_Z(n)%kd
          ov = obs_Z(n)%a00 * T_prog(index_temp)%field(i,j,k,taup1) + &
               obs_Z(n)%a01 * T_prog(index_temp)%field(i,jp,k,taup1) + &
               obs_Z(n)%a11 * T_prog(index_temp)%field(ip,jp,k,taup1) + &
               obs_Z(n)%a10 * T_prog(index_temp)%field(ip,j,k,taup1)
          ov = ov - obs_Z(n)%val(k)
          aerr = obs_Z(n)%err(k)*time_adj
          aov = abs(ov)
          if (aov .lt. dtemp_max) then
            if (aov .gt. dtemp_elm) then
              aerr = aerr/(1.0+aov-dtemp_elm)**2
            endif
            g_cg(i,j,k) = g_cg(i,j,k) + ov*aerr*obs_Z(n)%a00
            g_cg(i,jp,k) = g_cg(i,jp,k) + ov*aerr*obs_Z(n)%a01
            g_cg(ip,jp,k) = g_cg(ip,jp,k) + ov*aerr*obs_Z(n)%a11
            g_cg(ip,j,k) = g_cg(ip,j,k) + ov*aerr*obs_Z(n)%a10
            obs_Z(n)%aerr(k) = aerr
          else
            obs_Z(n)%aerr(k) = 0.0
          endif
        enddo
      else
        time_adj = 0.0
        obs_Z(n)%win = .false.
      endif
    else if (obs_Z(n)%code .eq. salt_code) then
      wndw_fwd = increment_time(Time%model_time, 0, sz_wndw_fwd)
      wndw_bwd = decrement_time(Time%model_time, 0, sz_wndw_bwd)
      if (obs_Z(n)%obs_time < wndw_fwd .and. obs_Z(n)%obs_time > wndw_bwd) then
        diff_time = Time%model_time - obs_Z(n)%obs_time
        call get_time (diff_time, dsec, dday)
        time_sep = real(dday) + real(dsec)/real(spd)
        time_adj = (1.0-time_sep*rszw)
        obs_Z(n)%win = .true.
        do k=1,obs_Z(n)%kd
          ov = obs_Z(n)%a00 * T_prog(index_salt)%field(i,j,k,taup1) + &
               obs_Z(n)%a01 * T_prog(index_salt)%field(i,jp,k,taup1) + &
               obs_Z(n)%a11 * T_prog(index_salt)%field(ip,jp,k,taup1) + &
               obs_Z(n)%a10 * T_prog(index_salt)%field(ip,j,k,taup1)
          ov = ov - obs_Z(n)%val(k)
          aerr = obs_Z(n)%err(k)*time_adj
          aov = abs(ov)
          if (aov .lt. dsalt_max) then
            if (aov .gt. dsalt_elm) then
              aerr = aerr/(1.0+aov-dsalt_elm)**2
            endif
            g_cg(i,j,k+kass) = g_cg(i,j,k+kass) + ov*aerr*obs_Z(n)%a00
            g_cg(i,jp,k+kass) = g_cg(i,jp,k+kass) + ov*aerr*obs_Z(n)%a01
            g_cg(ip,jp,k+kass) = g_cg(ip,jp,k+kass) + ov*aerr*obs_Z(n)%a11
            g_cg(ip,j,k+kass) = g_cg(ip,j,k+kass) + ov*aerr*obs_Z(n)%a10
            obs_Z(n)%aerr(k) = aerr
          else
            obs_Z(n)%aerr(k) = 0.0
          endif
        enddo
      else
        time_adj = 0.0
        obs_Z(n)%win = .false.
      endif
    endif
  enddo

!
!-----------------------------------------------------------------------
!  The surface observations (exlcuding altimetry)
!-----------------------------------------------------------------------
!
  do n=1,num_obs0
    i = obs_0(n)%io
    ip = i + 1
    j = obs_0(n)%jo
    jp = j + 1
    if (obs_0(n)%code .eq. sst_code) then
      wndw_fwd = increment_time(Time%model_time, 0, t0_wndw_fwd)
      wndw_bwd = decrement_time(Time%model_time, 0, t0_wndw_bwd)
      if (obs_0(n)%obs_time < wndw_fwd .and. obs_0(n)%obs_time > wndw_bwd) then
        diff_time = Time%model_time - obs_0(n)%obs_time
        call get_time (diff_time, dsec, dday)
        time_sep = real(dday) + real(dsec)/real(spd)
        time_adj = (1.0-time_sep*rt0w)
        obs_0(n)%win = .true.
        ov = obs_0(n)%a00 * T_prog(index_temp)%field(i,j,1,taup1) + &
             obs_0(n)%a01 * T_prog(index_temp)%field(i,jp,1,taup1) + &
             obs_0(n)%a11 * T_prog(index_temp)%field(ip,jp,1,taup1) + &
             obs_0(n)%a10 * T_prog(index_temp)%field(ip,j,1,taup1)
        ov = ov - obs_0(n)%val
        aerr = obs_0(n)%err*time_adj
        aov = abs(ov)
        if (aov .lt. dtemp_max) then
          if (aov .gt. dtemp_elm) then
            aerr = aerr/(1.0+aov-dtemp_elm)**2
            obs_0(n)%aerr = aerr
          endif
          g_cg(i,j,1) = g_cg(i,j,1) + ov*aerr*obs_0(n)%a00
          g_cg(i,jp,1) = g_cg(i,jp,1) + ov*aerr*obs_0(n)%a01
          g_cg(ip,jp,1) = g_cg(ip,jp,1) + ov*aerr*obs_0(n)%a11
          g_cg(ip,j,1) = g_cg(ip,j,1) + ov*aerr*obs_0(n)%a10
        else
          obs_0(n)%aerr = 0.0
        endif
      else
        time_adj = 0.0
        obs_0(n)%win = .false.
      endif
    else if (obs_0(n)%code .eq. sss_code) then
      wndw_fwd = increment_time(Time%model_time, 0, s0_wndw_fwd)
      wndw_bwd = decrement_time(Time%model_time, 0, s0_wndw_bwd)
      if (obs_0(n)%obs_time < wndw_fwd .and. obs_0(n)%obs_time > wndw_bwd) then
        diff_time = Time%model_time - obs_0(n)%obs_time
        call get_time (diff_time, dsec, dday)
        time_sep = real(dday) + real(dsec)/real(spd)
        time_adj = (1.0-time_sep*rs0w)
        obs_0(n)%win = .true.
        ov = obs_0(n)%a00 * T_prog(index_salt)%field(i,j,1,taup1) + &
             obs_0(n)%a01 * T_prog(index_salt)%field(i,jp,1,taup1) + &
             obs_0(n)%a11 * T_prog(index_salt)%field(ip,jp,1,taup1) + &
             obs_0(n)%a10 * T_prog(index_salt)%field(ip,j,1,taup1)
        ov = ov - obs_0(n)%val
        aerr = obs_0(n)%err*time_adj
        aov = abs(ov)
        if (aov .lt. dsalt_max) then
          if (aov .gt. dsalt_elm) then
            aerr = aerr/(1.0+aov-dsalt_elm)**2
            obs_0(n)%aerr = aerr
          endif
          g_cg(i,j,1+kass) = g_cg(i,j,1+kass) + ov*aerr*obs_0(n)%a00
          g_cg(i,jp,1+kass) = g_cg(i,jp,1+kass) + ov*aerr*obs_0(n)%a01
          g_cg(ip,jp,1+kass) = g_cg(ip,jp,1+kass) + ov*aerr*obs_0(n)%a11
          g_cg(ip,j,1+kass) = g_cg(ip,j,1+kass) + ov*aerr*obs_0(n)%a10
        else
          obs_0(n)%aerr = 0.0
        endif
      else
        time_adj = 0.0
        obs_0(n)%win = .false.
      endif
    endif
  enddo
!
!-----------------------------------------------------------------------
!  The altimeter observations
!-----------------------------------------------------------------------
!
  do n=1,num_obsa
    i = obs_A(n)%io
    ip = i + 1
    j = obs_A(n)%jo
    jp = j + 1
    if (obs_A(n)%code .eq. altm_code) then
      wndw_fwd = increment_time(Time%model_time, 0, al_wndw_fwd)
      wndw_bwd = decrement_time(Time%model_time, 0, al_wndw_bwd)
      if (obs_A(n)%obs_time < wndw_fwd .and. obs_A(n)%obs_time > wndw_bwd) then
        diff_time = Time%model_time - obs_A(n)%obs_time
        call get_time (diff_time, dsec, dday)
        time_sep = real(dday) + real(dsec)/real(spd)
        time_adj = (1.0-time_sep*ralw)
        obs_A(n)%win = .true.
        ov = obs_A(n)%a00 * (Ext_mode%eta_t(i,j,taup1) - eta_clm(i,j)) + &
             obs_A(n)%a01 * (Ext_mode%eta_t(i,jp,taup1) - eta_clm(i,jp)) + &
             obs_A(n)%a11 * (Ext_mode%eta_t(ip,jp,taup1) - eta_clm(ip,jp)) + &
             obs_A(n)%a10 * (Ext_mode%eta_t(ip,j,taup1) - eta_clm(ip,j))
        ov = ov - obs_A(n)%val
        aerr = obs_A(n)%err*time_adj
        aov = abs(ov)
        if (aov .lt. daltm_max) then
          if (aov .gt. daltm_elm) then
            aerr = aerr/(1.0+aov-daltm_elm)**2
          endif
          do k=1,kass
            g_cg(i,j,k) = g_cg(i,j,k) + ov*aerr*obs_A(n)%a00*cdnz(k)
            g_cg(i,jp,k) = g_cg(i,jp,k) + ov*aerr*obs_A(n)%a01*cdnz(k)
            g_cg(ip,jp,k) = g_cg(ip,jp,k) + ov*aerr*obs_A(n)%a11*cdnz(k)
            g_cg(ip,j,k) = g_cg(ip,j,k) + ov*aerr*obs_A(n)%a10*cdnz(k)
            kks = k+kass
            g_cg(i,j,kks) = g_cg(i,j,kks) + ov*aerr*obs_A(n)%a00*cdnzs(k)
            g_cg(i,jp,kks) = g_cg(i,jp,kks) + ov*aerr*obs_A(n)%a01*cdnzs(k)
            g_cg(ip,jp,kks) = g_cg(ip,jp,kks) + ov*aerr*obs_A(n)%a11*cdnzs(k)
            g_cg(ip,j,kks) = g_cg(ip,j,kks) + ov*aerr*obs_A(n)%a10*cdnzs(k)
          enddo
          obs_A(n)%aerr = aerr
        else
          obs_A(n)%aerr = 0.0
        endif
      else
        time_adj = 0.0
        obs_A(n)%win = .false.
      endif
    endif
  enddo
!
  call mpp_update_domains (g_cg, Dom%domain2d)
!
  end subroutine init_grad
! </SUBROUTINE> NAME="init_grad"


!!
!!#######################################################################
!! <SUBROUTINE NAME="comp_f">
!!
!! <DESCRIPTION>
!! Compute the 
!! </DESCRIPTION>
!!
subroutine comp_f (obs_Z, obs_0, obs_A)
!
  type(ocean_obsz_type), intent(in)        :: obs_Z(:)
  type(ocean_obs0_type), intent(in)        :: obs_0(:)
  type(ocean_obs0_type), intent(in)        :: obs_A(:)
!
  integer :: i, ip, j, jp, k, ks1, n, pe
  real    :: ov, aerr
!-----------------------------------------------------------------------
!  data types are encoded in obs%code
!      T(z)        1 <= code <= 10
!      S(z)       11 <= code <= 20
!      SST        21 <= code <= 22
!      SSS        23 <= code <= 23
!      ALTM       24 <= code <= 26
!  these are set in godas_data_mod, and can be modified if needed
!-----------------------------------------------------------------------
!
  pe = mpp_pe()

!-----------------------------------------------------------------------
!   Set f to zero.
!-----------------------------------------------------------------------
!
  f_cg = 0.0
!
!-----------------------------------------------------------------------
!  For each observation
!   1) interpolate d_cg to observation position
!   2) multiply error inverse times interpolated d_cg
!   3) project result back onto model grid
!   4) sum the gridded result in f_cg
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!  Profile observations
!-----------------------------------------------------------------------
!
  do n=1,num_obsz
    i = obs_Z(n)%io
    ip = i + 1
    j = obs_Z(n)%jo
    jp = j + 1
    if (obs_Z(n)%code .eq. temp_code .and. obs_Z(n)%win) then
      do k=1,obs_Z(n)%kd
        ov = obs_Z(n)%a00 * d_cg(i,j,k) + &
             obs_Z(n)%a01 * d_cg(i,jp,k) + &
             obs_Z(n)%a11 * d_cg(ip,jp,k) + &
             obs_Z(n)%a10 * d_cg(ip,j,k)
        aerr = obs_Z(n)%aerr(k)
        f_cg(i,j,k) = f_cg(i,j,k) + ov*aerr*obs_Z(n)%a00
        f_cg(i,jp,k) = f_cg(i,jp,k) + ov*aerr*obs_Z(n)%a01
        f_cg(ip,jp,k) = f_cg(ip,jp,k) + ov*aerr*obs_Z(n)%a11
        f_cg(ip,j,k) = f_cg(ip,j,k) + ov*aerr*obs_Z(n)%a10
      enddo
    else if (obs_Z(n)%code .eq. salt_code .and. obs_Z(n)%win) then
      do k=1,obs_Z(n)%kd
        ov = obs_Z(n)%a00 * d_cg(i,j,k+kass) + &
             obs_Z(n)%a01 * d_cg(i,jp,k+kass) + &
             obs_Z(n)%a11 * d_cg(ip,jp,k+kass) + &
             obs_Z(n)%a10 * d_cg(ip,j,k+kass)
        aerr = obs_Z(n)%aerr(k)
        f_cg(i,j,k+kass) = f_cg(i,j,k+kass) + ov*aerr*obs_Z(n)%a00
        f_cg(i,jp,k+kass) = f_cg(i,jp,k+kass) + ov*aerr*obs_Z(n)%a01
        f_cg(ip,jp,k+kass) = f_cg(ip,jp,k+kass) + ov*aerr*obs_Z(n)%a11
        f_cg(ip,j,k+kass) = f_cg(ip,j,k+kass) + ov*aerr*obs_Z(n)%a10
      enddo
    endif
  enddo
!
!-----------------------------------------------------------------------
!  Surface observations (excluding altimetry)
!-----------------------------------------------------------------------
!
  do n=1,num_obs0
    i = obs_0(n)%io
    ip = i + 1
    j = obs_0(n)%jo
    jp = j + 1
    if (obs_0(n)%code .eq. sst_code .and. obs_0(n)%win) then
      ov = obs_0(n)%a00 * d_cg(i,j,1) + &
           obs_0(n)%a01 * d_cg(i,jp,1) + &
           obs_0(n)%a11 * d_cg(ip,jp,1) + &
           obs_0(n)%a10 * d_cg(ip,j,1)
      aerr = obs_0(n)%aerr
      f_cg(i,j,1) = f_cg(i,j,1) + ov*aerr*obs_0(n)%a00
      f_cg(i,jp,1) = f_cg(i,jp,1) + ov*aerr*obs_0(n)%a01
      f_cg(ip,jp,1) = f_cg(ip,jp,1) + ov*aerr*obs_0(n)%a11
      f_cg(ip,j,1) = f_cg(ip,j,1) + ov*aerr*obs_0(n)%a10
    else if (obs_0(n)%code .eq. sss_code .and. obs_0(n)%win) then
      ks1 = kass + 1
      ov = obs_0(n)%a00 * d_cg(i,j,ks1) + &
           obs_0(n)%a01 * d_cg(i,jp,ks1) + &
           obs_0(n)%a11 * d_cg(ip,jp,ks1) + &
           obs_0(n)%a10 * d_cg(ip,j,ks1)
      aerr = obs_0(n)%aerr
      f_cg(i,j,ks1) = f_cg(i,j,ks1) + ov*aerr*obs_0(n)%a00
      f_cg(i,jp,ks1) = f_cg(i,jp,ks1) + ov*aerr*obs_0(n)%a01
      f_cg(ip,jp,ks1) = f_cg(ip,jp,ks1) + ov*aerr*obs_0(n)%a11
      f_cg(ip,j,ks1) = f_cg(ip,j,ks1) + ov*aerr*obs_0(n)%a10
    endif
  enddo
!
!-----------------------------------------------------------------------
!  Altimeter observations
!-----------------------------------------------------------------------
!
  do n=1,num_obsa
    i = obs_A(n)%io
    ip = i + 1
    j = obs_A(n)%jo
    jp = j + 1
    if (obs_A(n)%code .eq. altm_code .and. obs_A(n)%win) then
      ov = 0.0
      do k=1,kass
        ov = ov + cdnz(k) * &
                ( obs_A(n)%a00 * d_cg(i,j,k) + &
                  obs_A(n)%a01 * d_cg(i,jp,k) + &
                  obs_A(n)%a11 * d_cg(ip,jp,k) + &
                  obs_A(n)%a10 * d_cg(ip,j,k) ) + &
                  cdnzs(k) * &
                ( obs_A(n)%a00 * d_cg(i,j,k+kass) + &
                  obs_A(n)%a01 * d_cg(i,jp,k+kass) + &
                  obs_A(n)%a11 * d_cg(ip,jp,k+kass) + &
                  obs_A(n)%a10 * d_cg(ip,j,k+kass) )
      enddo
      aerr = obs_A(n)%aerr
      do k=1,kass
        f_cg(i,j,k) = f_cg(i,j,k) + ov*aerr*obs_A(n)%a00*cdnz(k)
        f_cg(i,jp,k) = f_cg(i,jp,k) + ov*aerr*obs_A(n)%a01*cdnz(k)
        f_cg(ip,jp,k) = f_cg(ip,jp,k) + ov*aerr*obs_A(n)%a11*cdnz(k)
        f_cg(ip,j,k) = f_cg(ip,j,k) + ov*aerr*obs_A(n)%a10*cdnz(k)
        f_cg(i,j,k+kass) = f_cg(i,j,k+kass) + ov*aerr*obs_A(n)%a00*cdnzs(k)
        f_cg(i,jp,k+kass) = f_cg(i,jp,k+kass) + ov*aerr*obs_A(n)%a01*cdnzs(k)
        f_cg(ip,jp,k+kass) = f_cg(ip,jp,k+kass) + ov*aerr*obs_A(n)%a11*cdnzs(k)
        f_cg(ip,j,k+kass) = f_cg(ip,j,k+kass) + ov*aerr*obs_A(n)%a10*cdnzs(k)
      enddo
    endif
  enddo
!
!-----------------------------------------------------------------------
!   Add e to f
!-----------------------------------------------------------------------
!
  f_cg = f_cg + e_cg
!
  call mpp_update_domains (f_cg, Dom%domain2d)
!
  end subroutine comp_f
! </SUBROUTINE> NAME="comp_f"


! </SUBROUTINE> NAME="eg_lpsmthr"
!

!#######################################################################
! <SUBROUTINE NAME="eg_lpsmthr">
!
! <DESCRIPTION>
! This subroutine multiplies g by an approximation to the first guess
! error covariance matrix [e] to get the vector h. The approximation to
! [e] is made by a series of multiplications by 1+laplacian.
! </DESCRIPTION>
!
subroutine eg_lpsmthr

  integer         :: nit, n, i, j, k, kk, ka, kp, kkp
  integer         :: np, npid2
  integer         :: jbg, jfn, jgbg, jgfn
  real            :: con, col
integer :: pe
real :: tmskmn

pe = mpp_pe()

  npid2=npits/2
  jbg = jsc
  if (jbg .eq. 1) jbg = 3
  jfn = jec
  if (jfn .eq. jsg) jfn = jsg - 2

!
!-----------------------------------------------------------------------
!   multiply g by the square root of the local vertical background
!   error covariance
!-----------------------------------------------------------------------
!
  do j=jsc,jec
    do i=isc,iec

      covsr = 0.0
      if (Grd%kmt(i,j) .gt. 0) then
        ka = min(kass,Grd%kmt(i,j))
        do k=1,ka
          ev(k) = sqrt(vtmp(i,j,k))
!         ev(k) = 1.0
        enddo
        if (Grd%kmt(i,j) .ge. kass) then
          do k=1,kass
            do kk=1,kass
              covsr(kk,k) = cvn(kk,k) * ev(kk) * ev(k)
            enddo
          enddo
        else
          do k=1,ka
            covsr(k,k) = ev(k) * ev(k)
          enddo
        endif
      endif

      do k=1,kass
        wrkk(k) = 0.0
        do kk=1,kass
          wrkk(k) = wrkk(k) + g_cg(i,j,kk) * covsr(k,kk)
        enddo
      enddo

      if (num_cor_tracers .eq. 2) then

        covsr = 0.0
        if (Grd%kmt(i,j) .gt. 0) then
          ka = min(kass,Grd%kmt(i,j))
          do k=1,ka
            ev(k) = sqrt(vsal(i,j,k))
          enddo
          if (Grd%kmt(i,j) .ge. kass) then
            do k=1,kass
              do kk=1,kass
                covsr(kk,k) = cvn(kk,k) * ev(kk) * ev(k)
              enddo
            enddo
          else
            do k=1,ka
                covsr(k,k) = ev(k) * ev(k)
            enddo
          endif
        endif
  
        do k=1,kass
          kp = k+kass
          wrkk(kp) = 0.0
          do kk=1,kass
            kkp = kk+kass
            wrkk(kp) = wrkk(kp) + g_cg(i,j,kkp) * covsr(k,kk)
          enddo
        enddo

      endif

      do k=1,kass2
        g_cg(i,j,k) = wrkk(k)
      enddo

    enddo
  enddo

  do k=1,kass2
    do j=jsd,jed
      do i=isd,ied
        wcn(i,j) = 1.0 - wso(i,j) - wno(i,j) - wea(i,j) - wwe(i,j)
      enddo
    enddo

    if (k .le. kass) then
      kk = k                           ! temperature
    else
      kk = k - kass                    ! salinity
    endif

    do j=jsc,jec
      do i=isc,iec
        con = wso(i,j)
        if (Grd%kmt(i,j+1) .lt. kk) con = wno(i,j)
        col = con*con/((con+aeval)*con+dbsq)
        if (Grd%kmt(i,j-1) .lt. kk) wcn(i,j) = wcn(i,j)+con*col
        if (Grd%kmt(i,j+1) .lt. kk) wcn(i,j) = wcn(i,j)+con*col

        con = wwe(i,j)
        col = con*con*con/((con+aeval)*con+dbsq)
        if (Grd%kmt(i-1,j) .lt. kk) wcn(i,j) = wcn(i,j)+col
        if (Grd%kmt(i+1,j) .lt. kk) wcn(i,j) = wcn(i,j)+col
      enddo
    enddo
    call mpp_update_domains (wcn, Dom%domain2d)

    s1(:,:) = g_cg(:,:,k) * wgta(:,:)
    s2(:,:) = 0.0

    do np=1,npid2
      do j=jbg,jfn
        do i=isc,iec
! DEBUG
          tmskmn = min(Grd%tmask(i,j,kk),Grd%tmask(i,j-1,kk),Grd%tmask(i,j+1,kk), &
                       Grd%tmask(i-1,j,kk),Grd%tmask(i+1,j,kk))
          s2(i,j) = ( wcn(i,j) * s1(i,j) + wso(i,j) * s1(i,j-1) + wno(i,j) * s1(i,j+1) &
                        + wwe(i,j) * s1(i-1,j) + wea(i,j) * s1(i+1,j) ) * tmskmn
!         s2(i,j) = ( wcn(i,j) * s1(i,j) + wso(i,j) * s1(i,j-1) + wno(i,j) * s1(i,j+1) &
!                       + wwe(i,j) * s1(i-1,j) + wea(i,j) * s1(i+1,j) ) * Grd%tmask(i,j,kk)
! DEBUG
        enddo
      enddo
      call mpp_update_domains (s1, Dom%domain2d)
      do j=jbg,jfn
        do i=isc,iec
! DEBUG
          tmskmn = min(Grd%tmask(i,j,kk),Grd%tmask(i,j-1,kk),Grd%tmask(i,j+1,kk), &
                       Grd%tmask(i-1,j,kk),Grd%tmask(i+1,j,kk))
          s1(i,j) = ( wcn(i,j) * s2(i,j) + wno(i,j-1) * s2(i,j-1) + wso(i,j+1) * s2(i,j+1) &
                        + wwe(i,j) * s2(i-1,j) + wea(i,j) * s2(i+1,j) ) * tmskmn
!         s1(i,j) = ( wcn(i,j) * s2(i,j) + wno(i,j-1) * s2(i,j-1) + wso(i,j+1) * s2(i,j+1) &
!                       + wwe(i,j) * s2(i-1,j) + wea(i,j) * s2(i+1,j) ) * Grd%tmask(i,j,kk)
! DEBUG
        enddo
      enddo
      call mpp_update_domains (s2, Dom%domain2d)
    enddo

    h_cg(:,:,k) = s1(:,:) * wgta(:,:)

  enddo
!
!-----------------------------------------------------------------------
!   multiply h by the square root of the local vertical background
!   error covariance
!-----------------------------------------------------------------------
!
  do j=jsc,jec
    do i=isc,iec

      covsr = 0.0
      if (Grd%kmt(i,j) .gt. 0) then
        ka = min(kass,Grd%kmt(i,j))
        do k=1,ka
          ev(k) = sqrt(vtmp(i,j,k))
!         ev(k) = 1.0
        enddo
        if (Grd%kmt(i,j) .ge. kass) then
          do k=1,kass
            do kk=1,kass
              covsr(kk,k) = cvn(kk,k) * ev(kk) * ev(k)
            enddo
          enddo
        else
          do k=1,ka
            covsr(k,k) = ev(k) * ev(k)
          enddo
        endif
      endif

      do k=1,kass
        wrkk(k) = 0.0
        do kk=1,kass
          wrkk(k) = wrkk(k) + h_cg(i,j,kk) * covsr(k,kk)
        enddo
      enddo

      if (num_cor_tracers .eq. 2) then

        covsr = 0.0
        if (Grd%kmt(i,j) .gt. 0) then
          ka = min(kass,Grd%kmt(i,j))
          do k=1,ka
            ev(k) = sqrt(vsal(i,j,k))
          enddo
          if (Grd%kmt(i,j) .ge. kass) then
            do k=1,kass
              do kk=1,kass
                covsr(kk,k) = cvn(kk,k) * ev(kk) * ev(k)
              enddo
            enddo
          else
            do k=1,ka
              covsr(k,k) = ev(k) * ev(k)
            enddo
          endif
        endif

        do k=1,kass
          kp = k+kass
          wrkk(kp) = 0.0
          do kk=1,kass
            kkp = kk+kass
            wrkk(kp) = wrkk(kp) + h_cg(i,j,kkp) * covsr(k,kk)
          enddo
        enddo

      endif

      do k=1,kass2
        h_cg(i,j,k) = wrkk(k)
      enddo

    enddo
  enddo

end subroutine eg_lpsmthr
! </SUBROUTINE> NAME="eg_lpsmthr"

end module godas_mod
