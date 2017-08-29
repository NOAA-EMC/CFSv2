!-------------------------------------------------------------------------
! NASA Goddard Space Flight Center Land Information System (LIS) V3.0
! Released May 2004
!
! See SOFTWARE DISTRIBUTION POLICY for software distribution policies
!
! The LIS source code and documentation are in the public domain,
! available without fee for educational, research, non-commercial and
! commercial purposes.  Users may distribute the binary or source
! code to third parties provided this statement appears on all copies and
! that no charge is made for such copies.
!
! NASA GSFC MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE
! SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED AS IS WITHOUT EXPRESS OR
! IMPLIED WARRANTY.  NEITHER NASA GSFC NOR THE US GOVERNMENT SHALL BE
! LIABLE FOR ANY DAMAGES SUFFERED BY THE USER OF THIS SOFTWARE.
!
! See COPYRIGHT.TXT for copyright details.
!
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: lis_module.F90
!
! !DESCRIPTION:
!  Module for LDAS variable specification.  This file will contain no
!   tile space or grid space variables.  
!
! !REVISION HISTORY:
!  15 Oct 1999: Paul Houser; Initial code
!  4  Apr 2000: Jeffrey Walker; Added some catchment model variables
!  11 Apr 2000: Brian Cosgrove; Elevation correction and Forcing Mask
!               variables added 
!  6  Jun 2000: Jon Radakovich; Updated for new version of CLM
!  23 Feb 2001: Urszula Jambor; Updated for GEOS & GDAS forcing in GLDAS
!  27 Feb 2001: Brian Cosgrove; Added Catchment forcing data variables
!  15 Mar 2001: Jon Gottschalck; Updated for GDAS initialization of Mosaic 
!  12 Apr 2001: Urszula Jambor; Added domain,lsm,& force namefile paramters     
!  30 Apr 2001: Jon Radakovich; Update for PSAS temperature assimilation
!  17 Jul 2001: Jon Gottschalck; Update for global precipitation variables
!  30 Jul 2001: Matt Rodell; Add new soil parameter variables
!  05 Sep 2001: Brian Cosgrove; Add variables for PAR and BRTTMP, remove 
!               1/4 to 1/8 interp variables
!  15 Oct 2001: Jesse Meng; Replace agrmet flag by agrmetsw and agrmetlw
!  27 Nov 2001: Jon Gottschalck; Added variables for AVHRR LAI data 
!  07 Dec 2001: Urszula Jambor; Added LDAS_KGDS array     
!  03 Feb 2002: Jon Gottschalck; Added Koster tilespace variables
!  05 Feb 2002: Brian Cosgrove; Added NLDAS 11 Layer soil class file from Yun Duan
!               and ftype variable to indicate NLDAS ETA forcing source used
!  08 Feb 2002: Urszula Jambor; Added latmax for AGRMET use.
!  15 Apr 2002: Urszula Jambor; Added ECMWF forcing options.
!  28 Apr 2002: Kristi Arsenault; Added NOAH LSM parameters and code
!  14 Nov 2002; Sujay Kumar; Optimized version for LIS
!  14 Oct 2003; Sujay Kumar; Removed LSM specific variables. 
!  17 Oct 2003: Yudong Tian; Added IC, IR for regional runs 
!
! !INTERFACE:
module lis_module 
! !USES:
  implicit none
! !ARGUMENTS:
  type lisdomain
     integer :: nch            !actual number of tiles
     integer :: lsm            !land surface model (2=clm,4=noah)      
     integer :: soil           !soil parameter scheme (1=original veg-based, 2=reynolds soils)
     integer :: elev           !elevation difference base
     integer :: glbnch          !actual global number of tiles
     integer :: ngrid           !actual number of grids
     integer :: glbngrid        !actual global number of grids
     integer :: domain         !model domain, (1=nldas, 2=gldas)
     integer :: landcover      ! landcover type
     integer :: gnc            !global array (different if subsetting is used)
     integer :: gnr            !global array (different if subsetting is used)
     integer :: lnc            !local number of columns in grid
     integer :: lnr            !local number of rows in grid
     integer :: ic             ! column index of sub-domain block
     integer :: ir             ! row index of sub-domain block
     integer :: maxt           !maximum tiles per grid  
     real :: mina              !min grid area for tile (%)
     real :: udef              !undefined value
     real :: gridDesc(50)      !grid definition array 
     real :: soil_gridDesc(6)   !grid definition for soil dataset
     real :: elev_gridDesc(6)   !grid definition for elev dataset
     real :: lc_gridDesc(6)   !grid definition for landcover dataset
  end type lisdomain
  
  type lisforcing
     integer :: force          !forcing data type (1=gdas,2=geos)
     integer :: ecor           !use elevation correction
     integer :: nforce
     integer :: nf             !number of forcing variables
     integer :: nmif           !number of forcing variables for model initialization option                        
     integer :: rstflag        !0=use only 1 forcing time, 1=find two forcing times upon restart                                !=== hdf output variables
     integer :: gridchange
     integer :: interp
     integer :: latmax         !per hemisphere, for agrmet intepolation
     
     integer :: shortflag      !shortwave radiation source flag
                                !0=no radiation
                                !1=instantaneous sw
                                !2=time averaged sw
     integer :: longflag       !longwave radiation source flag
                                !0=no radiation
                                !1=instantaneous lw
                                !2=time averaged lw
     integer :: findtime1,findtime2
     integer :: findagrtime1,findagrtime2
     integer :: f00_flag, f06_flag
     integer :: gpcpsrc     !global precipitation flags
     integer :: radsrc
  end type lisforcing

  type lisparameters
     integer :: lai            !lai data source (1=original, 
                               !2=avhrr satellite data 
                               !3=modis satellite data)
     integer :: nt             !number of vegetation types       
     integer :: vclass         !vegetation classification (1=umd)
     integer  :: laiflag        !satellite lai time
     integer  :: saiflag        !satellite lai time
     character*50 :: mfile     !land/water mask file for modelling (avhrr)
     character*50 :: vfile     !vegetation classification file (avhrr)!      
     character*40 :: safile    !sand fraction map file
     character*40 :: clfile    !clay fraction map file
     character*40 :: po1file    !porosity map file
     character*40 :: po2file    !porosity map file
     character*40 :: po3file    !porosity map file
     character*40 :: slfile    !slope map file
     character*40 :: sifile    !silt map file
     character*40 :: avhrrdir     !avhrr data directory
     character*40 :: modisdir  !modis data directory
     character*40 :: iscfile   !soil color map file
     character*40 :: elevfile
     real*8  :: laitime        !satellite lai time
!<kluge sai read>
     real*8  :: saitime        !satellite sai time
!</kluge sai read>
     
  end type lisparameters

  type lisoutput
     integer :: wfor           !write forcing (0=no,1=yes)
     integer :: wtil           !write tile space data (0=no, 1=yes)
     integer :: wout           !output format option (1-binary, 2-grib)
     integer :: wsingle        !write one variable per file
     integer :: wparam         !write parameter file output
     integer :: expcode        !3 digit experiment code 
     integer :: startcode      !0=restart date, 1=card date
     integer :: foropen
     integer :: numoutf        !counts number of output times for forcing data
     integer :: fidgm,rcgm,fidtm,rctm
     character*40 :: odir      !output data base directory  
     character*40 :: dfile     !runtime diagnostics file
  end type lisoutput

  type listime
     integer :: sss            !starting second 
     integer :: sdoy           !starting day of year
     integer :: smn            !starting minute
     integer :: shr            !starting hour
     integer :: sda            !starting day
     integer :: smo            !starting month
     integer :: syr            !starting year
     integer :: endcode        !0=realtime, 1=specific date
     integer :: ess            !ending second
     integer :: emn            !ending minute
     integer :: edoy           !ending day of year
     integer :: ehr            !ending hour
     integer :: eda            !ending day
     integer :: emo            !ending month
     integer :: eyr            !ending year
     integer :: ts             !timestep (seconds) 
     integer :: tscount        !timestep count
     integer :: yyyymmdd,hhmmss
     integer :: doy,yr,mo,da,hr,mn,ss !lis current model timing variables   
     integer :: endtime        !lis stop (0=continue time looping)
     integer :: pda            !lis previous timestep day
     real*8 :: time            !lis current model time in years
     real*8 :: etime           !lis end time in years
     real :: gmt,egmt,sgmt     
  end type listime
      
  type lisassimil
     integer :: rpsas, rbias,ribc,rdbc,rsdbc
  end type lisassimil

  type lisdec
     type(lisdomain)     :: d
     type(lisforcing)    :: f
     type(lisparameters) :: p
     type(listime)       :: t
     type(lisoutput)     :: o
     type(lisassimil)    :: a
  end type lisdec
!EOP
end module lis_module
