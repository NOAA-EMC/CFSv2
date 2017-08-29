
MODULE constantsMod


   !----------------------------------------------------------------------------
   ! physical constants (all data public)
   !----------------------------------------------------------------------------
   public
   real*8,parameter :: CONST_PI     = 3.14159265358979323846  ! pi
   real*8,parameter :: CONST_CDAY   = 86400.0      ! sec in calendar day ~ sec
   real*8,parameter :: CONST_SDAY   = 86164.0      ! sec in siderial day ~ sec
   real*8,parameter :: CONST_OMEGA  = 2.0*CONST_PI/CONST_SDAY ! earth rot ~ rad/sec
   real*8,parameter :: CONST_REARTH = 6.37122e6    ! radius of earth ~ m
   real*8,parameter :: CONST_G      = 9.80616      ! acceleration of gravity ~ m/s^2
   real*8,parameter :: CONST_PSTD   = 101325.0     ! standard pressure ~ pascals

   real*8,parameter :: CONST_STEBOL = 5.67e-8      ! Stefan-Boltzmann constant ~ W/m^2/K^4
   real*8,parameter :: CONST_BOLTZ  = 1.38065e-23  ! Boltzmann's constant ~ J/K/molecule
   real*8,parameter :: CONST_AVOGAD = 6.02214e26   ! Avogadro's number ~ molecules/kmole
   real*8,parameter :: CONST_RGAS   = CONST_AVOGAD*CONST_BOLTZ ! Universal gas constant ~ J/K/kmole
   real*8,parameter :: CONST_MWDAIR = 28.966       ! molecular weight dry air ~ kg/kmole
   real*8,parameter :: CONST_MWWV   = 18.016       ! molecular weight water vapor
   real*8,parameter :: CONST_RDAIR  = CONST_RGAS/CONST_MWDAIR  ! Dry air gas constant ~ J/K/kg
   real*8,parameter :: CONST_RWV    = CONST_RGAS/CONST_MWWV    ! Water vapor gas constant ~ J/K/kg
   real*8,parameter :: CONST_ZVIR   = (CONST_RWV/CONST_RDAIR)-1.0   ! RWV/RDAIR - 1.0
   real*8,parameter :: CONST_KARMAN = 0.4          ! Von Karman constant
 
   real*8,parameter :: CONST_TKFRZ  = 273.16       ! freezing T of fresh water ~ K (intentionally made == to TKTRIP)
   real*8,parameter :: CONST_TKTRIP = 273.16       ! triple point of fresh water ~ K

   real*8,parameter :: CONST_RHODAIR=CONST_PSTD/ &
     (CONST_RDAIR*CONST_TKFRZ)         ! density of dry air at STP   ~ kg/m^3
   real*8,parameter :: CONST_RHOFW  = 1.000e3      ! density of fresh water ~ kg/m^3
   real*8,parameter :: CONST_RHOSW  = 1.026e3      ! density of sea water ~ kg/m^3
   real*8,parameter :: CONST_RHOICE = 0.917e3      ! density of ice   ~ kg/m^3
   real*8,parameter :: CONST_CPDAIR = 1.00464e3    ! specific heat of dry air ~ J/kg/K
   real*8,parameter :: CONST_CPFW   = 4.188e3      ! specific heat of fresh h2o ~ J/kg/K
   real*8,parameter :: CONST_CPSW   = 3.996e3      ! specific heat of sea h2o ~ J/kg/K
   real*8,parameter :: CONST_CPWV   = 1.810e3      ! specific heat of water vap ~ J/kg/K
   real*8,parameter :: CONST_CPICE  = 2.11727e3    ! specific heat of fresh ice ~ J/kg/K
   real*8,parameter :: CONST_LATICE = 3.337e5      ! latent heat of fusion ~ J/kg
   real*8,parameter :: CONST_LATVAP = 2.501e6      ! latent heat of evaporation ~ J/kg
   real*8,parameter :: CONST_LATSUB = CONST_LATICE + CONST_LATVAP ! latent heat of sublimation ~ J/kg

   real*8,parameter :: CONST_OCN_REF_SAL = 34.7    ! ocn ref salinity (psu)
   real*8,parameter :: CONST_ICE_REF_SAL =  4.0    ! ice ref salinity (psu)


 END MODULE constantsMod
