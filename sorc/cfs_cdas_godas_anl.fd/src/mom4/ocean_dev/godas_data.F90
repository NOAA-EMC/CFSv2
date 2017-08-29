module godas_data_mod
!
use time_manager_mod, only: time_type
!
public
!
!   Assimilation data for MOM
!
!   Basic paramters set by namelist (see godas_init)
!
!   num_cor_tracers = number of tracers to be corrected (def:2, T&S)
!   kass            = number of vertical levels in assimilation
!   nwkobs          = number of weeks of observations used per run
!   maxits          = maximum iterations in oi analysis scheme
!   npits           = number of iterations used in lsmth to define [e]
!   gds_step        = interval between GODAS analyses (sec)
!   no_asm_rep      = number of repeat assimilations at each triggering 
!   acoef           = global scaling for model error variance
!   hrscl           = horizontal covariance scale (deg)
!   vcvn            = vertical covariance normalization factor
!   vsclf           = vertical covariance scale factor (scale is vsclf*(layer thickness))
!   tz_wndw_fwd     = forward time window for temperature profiles
!   tz_wndw_bwd     = backward time window for temperature profiles
!   sz_wndw_fwd     = forward time window for salinity profiles
!   sz_wndw_bwd     = backward time window for salinity profiles
!   t0_wndw_fwd     = forward time window for SST
!   t0_wndw_bwd     = backward time window for SST
!   s0_wndw_fwd     = forward time window for SSS
!   s0_wndw_bwd     = backward time window for SSS
!   al_wndw_fwd     = forward time window for altimetry
!   al_wndw_bwd     = backward time window for altimetry
!   assrestrt       = if .true. an assimilation is done at restart
!   debug_godas     = if .true. print debug information

integer :: num_cor_tracers, kass, nwkobs, maxits, npits, gds_step, no_asm_rep
real    :: acoef, hrscl, vcvn, vsclf
integer :: tz_wndw_fwd, tz_wndw_bwd, sz_wndw_fwd, sz_wndw_bwd
integer :: t0_wndw_fwd, t0_wndw_bwd, s0_wndw_fwd, s0_wndw_bwd
integer :: al_wndw_fwd, al_wndw_bwd
logical :: assrestrt, debug_godas

!   kass2  = num_cor_tracers*kass 
!   kass3  = 3*kass; used for multivariate analysis of u and v
!   kass4  = 4*kass; used for multivariate analysis of u and v

integer           :: kass2, kass3, kass4, asm_cnt
type(time_type)   :: gds_freq, alrm_dur
logical           :: ovr_alrm

!   rtzw   = inverse of the larger of tz_wndw_fwd and tz_wndw_bwd
!   rszw   = inverse of the larger of sz_wndw_fwd and sz_wndw_bwd
!   rt0w   = inverse of the larger of t0_wndw_fwd and t0_wndw_bwd
!   rs0w   = inverse of the larger of s0_wndw_fwd and s0_wndw_bwd
!   ralw   = inverse of the larger of al_wndw_fwd and al_wndw_bwd

real :: rtzw, rszw, rt0w, rs0w, ralw

! Some constants

integer, parameter   :: spd = 86400

! The observations
!
!   All data sets are organized in weekly periods.
!   Profile data are organized into obs_Z structures and surface data
!   (altimetry data) are organized into obs_0 structures.
!   Data can be separated into various types, depending on the type
!   of obsevation and the type of platform.  These are managed 
!   externally by data_table entries and internally by integer codes.
!
!     nwkobs = number of weekly observation periods on disk.
!     temp_code  =  1, generic T(z) code
!     xbt_code   =  2, xbt T(z) code
!     tao_code   =  3, tao, triton, pirata T(z) code
!     argo_code  =  4, argo T(z) code
!     salt_code  = 11, generic S(z) code
!     stao_code  = 13, tao, triton, pirata S(z) code
!     sargo_code = 14, argo S(z) code
!     sst_code   = 21, generic SST code
!     sss_code   = 23, generic SSS code
!     altm_code  = 24, generic altimetry code
!     tp_code    = 25, topex/poseidon code
!     j1_code    = 26, jason-1 code
!
!     num_obsz   = total number of profiles of all kinds
!     num_obs0   = total number of surface obs of all kinds (excl. alt.)
!     num_obsa   = total number of altimetry obs of all knids
!
integer, parameter     ::     temp_code  =  1
integer, parameter     ::     xbt_code   =  2
integer, parameter     ::     tao_code   =  3
integer, parameter     ::     argo_code  =  4
integer, parameter     ::     salt_code  = 11
integer, parameter     ::     stao_code  = 13
integer, parameter     ::     sargo_code = 14
integer, parameter     ::     sst_code   = 21
integer, parameter     ::     sss_code   = 23
integer, parameter     ::     altm_code  = 24
integer, parameter     ::     tp_code    = 25
integer, parameter     ::     j1_code    = 26
integer                ::     num_obsz, num_obs0, num_obsa
!
! Limiting the size of the innovations
!
!   dtemp_max = maximum size of temperature innovations
!   dtemp_elm = increase error of temperature innovations larger than this limit
!   dsalt_max = maximum size of salinity innovations
!   dsalt_elm = increase error of salinity innovations larger than this limit
!   daltm_max = maximum size of altimetry innovations
!   daltm_elm = increase error of altimetry innovations larger than this limit
!
real(kind=4), save     ::  dtemp_max = 10.0
real(kind=4), save     ::  dtemp_elm = 5.0
real(kind=4), save     ::  dsalt_max = 5.0
real(kind=4), save     ::  dsalt_elm = 3.0
real(kind=4), save     ::  daltm_max = 5.0
real(kind=4), save     ::  daltm_elm = 3.0
!
!  Only the variable part of the altimetry is assimilated. A surface height 
!  climatology must be subtracted from the model surface height. This is typically
!  bootstrapped from a model analysis that assimilates only T(z) and S(z).
!
!  eta_clm = model surface height climatology
!
real, save, dimension(:,:), allocatable :: eta_clm
!
!  The initial surface height innovation is assumed to represent an integral
!  error in the baroclinic field.  Coefficients (based on a linearized
!  dynamic height calculation) are used to distribute the innovation between
!  T and S down through the water column.
!
real, save, dimension(:), allocatable :: cdnz, cdnzs
!
!  Arrays used in specifying the background error covariance
!
real, save, dimension(:,:), allocatable :: covsr           !  covsr(kass,kass)
real, save, dimension(:), allocatable    :: ev, wrkk       !  ev(kass), wrkk(kass2)
!
!   cvn  = local vertical covariance matrix for temperature
!   vtmp = vertical variance for temperature
!
real, save, dimension(:,:), allocatable :: cvn      !  cvn(kass,kass)
real, save, dimension(:,:,:), allocatable :: vtmp
!
!   cvnsalt = local vertical covariance matrix for salinity
!   vsal    = vertical variance for salinity
!
real, save, dimension(:,:), allocatable :: cvnsalt      !  cvnsalt(kass,kass)
real, save, dimension(:,:,:), allocatable :: vsal
!
!   Arrays used in the minimizing the cost function. 
!
!   In general, arrays used by the Laplace smoother will be allocated in
!   the meridional direction with the global dimension, jmt, because that
!   routine is decomposed in the vertical direction.  The exception is s2h,
!   which facilitates the multiplication by the vertical covariance matrix
!   in a horizontal decomposition. Other arrays that are not part of the
!   Laplace smoother will be allocated for a horizontal decomposition.
!
!     elipt controls anisotropy of background error covariance (i,j)
!     wgta is normalization for Laplace smoother (lpwghts and lpsmthr) (i,j)
!     xcb etc. are used to hold compute domain decomposition data
!
real, save, dimension(:,:), allocatable :: wgta, elipt
integer, dimension(:), allocatable :: xcb, xce, xcsz, ycb, yce, ycsz
!
!     wcn, wea, wwe, wno, wso are C, E, W, N and S coefficients, allocated as (i,j)
!     wgns, s1, s2, dpth are arrays allocated as (i,j)
!
real, save, dimension(:,:), allocatable :: wcn, wea, wwe, wno, wso
real, save, dimension(:,:), allocatable :: wgns, s1, s2, dpth
!
!    The following arrays represent T, d, e, f, g, and h in Derber 
!    and Rosati (1989) and will be dimensioned (imt,kass4,jstask:jetask)
!    They are tagged with "_cg" to identify them with the congugate
!    gradient algorithm of the assimilation.
!
real, save, dimension(:,:,:), allocatable :: t_cg, d_cg, e_cg, f_cg, g_cg, h_cg
!
!  For saving innovations
!
!   invdys = save innovations within rsdys of the model time
!   ioexti = file unit for saving extracted temperature innovations
!   ioexsi = file unit for saving extracted salinity innovations
!   ioexei = file unit for saving extracted eta (ALTM) innovations
!   tinvd  = holds filename for temperature innovations
!   sinvd  = holds filename for salinity innovations
!   einvd  = holds filename for eta (ALTM) innovations
!
real, save :: invdys = 0.25
integer, save :: ioexti, ioexsi, ioexei
character(len=10), save :: tinvd, sinvd, einvd
!
! Section for multivariate assimilation
!
!   geofrac   =  accounts for inexactness of geostrophic balance, geofrac < 1
!   yl0, yl1  =  latitudes for averaging geostrophic velocities across equator
!   jl0, jl1  =  indices for averaging geostrophic velocities across equator
!   rl0, rl1  =  interpolation factors for averaging across equator
!   kmva(imt,jstask:jetask) = a "kmt" for u, v multi-variable
!
! application of the geo correction near the equatorial undercurrent is
! controlled by the following variables
!   ylso, ylno = equatorial latitudes to exclude geo correction of undercurrent
!   jlso, jlno = j-indices to exclude geo correction of undercurrent
!   xlwe, xlea = longitudes to exclude geo correction of undercurrent
!   ilwe, ilea = i-indices to exclude geo correction of undercurrent
!  within the bounds set by ylso and ylno, geo correction is controlled by
!  twEq and kwEq (precidence is given to whichever of these 2 conditions 
!  is deeper) and by tcEq.
!   twEq       = sfc currents above this temperature will get geo correction
!   kwEq       = sfc currents k <= kwEq will get geo correction
!   tcEq       = sfc currents below this temperature will get geo correction
!
integer, save :: pneq, jseq, jeeq
integer, save, dimension(:,:), allocatable :: kmva
real, save, dimension(:), allocatable :: rl0, rl1
real, parameter :: geofrac = 0.9
real, parameter :: yl0 = -1.2, yl1 = 1.2
integer, save :: jl0, jl1
real, parameter :: ylso = -2.2, ylno = 2.2, twEq = 25.0, tcEq = 13.0
real, parameter :: xlwe = 39.0, xlea = 103.0
! integer, parameter :: kwEq = 2
integer, parameter :: kwEq = 3
! the following settings should allow geostrophy applied everywhere
! real, parameter :: ylso = -2.2, ylno = 2.2, twEq = 1.0, tcEq = 1.0
! real, parameter :: xlwe = 39.0, xlea = 103.0
! integer, parameter :: kwEq = 35
integer, save :: jlso, jlno, ilwe, ilea
real, save, dimension(:,:), allocatable :: awrk
!
! End of section for multivariate assimilation
!
end module godas_data_mod
