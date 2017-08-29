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
! !MODULE: noah_module.F90
!
! !DESCRIPTION:
!  Module for 1-D NOAH land model driver variable specification.
!
! !REVISION HISTORY:
!
! 28 Apr 2002: K. Arsenault added NOAH LSM 2.5 code to LDAS. 
! 14 Nov 2002: Sujay Kumar Optimized version for LIS  
! 12 Apr 2006: Jesse Meng Added Z0, FORCING(11:16)
!              When changing number of variables, MUST
!              modify noah_blkcnts=(/I,J/) in noahpardef_module.F
!              I = number of integer; J = number of real
! 
! !INTERFACE:
module noah_module
!EOP
  implicit none
  public noahdec
!BOC
  type noahdec
     
     INTEGER :: ts                 !Timestep (seconds)
     INTEGER :: maxt               !Maximum tiles per grid
     INTEGER :: SIBVEG             !UMD to SIB Vegetation Class Index value
     INTEGER :: NSLAY              !Number of NOAH soil layers (4)
     INTEGER :: COUNT
     INTEGER :: ZOBSOIL(1)  !Zobler Soil Classes (LIS%NCH)
     
     REAL:: VEGP(7)      !Static vegetation parameter values, dim(NOAH_NVEGP)
     REAL:: VEGIP        !Interpolated Green Fraction from monthly parameters 
     REAL:: VEGMP1       !Month 1 Greenness Fraction Value 
     REAL:: VEGMP2       !Month 2 Greenness Fraction Value
     REAL:: ALBSF1       !Date 1 Snow-Free Albedo Value
     REAL:: ALBSF2       !Date 2 Snow-Free Albedo Value
     REAL:: SOILP(10)    !Static soil parameter values, dim(NOAH_NSOILP)
     REAL:: ALBSF        !Quarterly Snow-Free Albedo dataset
     REAL:: MXSNALB      !Maximum snow albedo dataset
     REAL:: TEMPBOT      !Bottom boundary temperature
     REAL:: Z0           !Surface roughness [m]
!-------------------------------------------------------------------------
! NOAH-State Variables
!-------------------------------------------------------------------------
     REAL :: T1          !NOAH Skin Temperature (K)
     REAL :: CMC         !NOAH Canopy Water Content 
     REAL :: SNOWH       !NOAH Actual Snow depth (m) 
     REAL :: SNEQV       !NOAH Water Equivalent Snow Depth (m)
     REAL :: STC(4)      !NOAH Soil Temperaure (4 layers)
     REAL :: SMC(4)      !NOAH Soil (4 layers)
     REAL :: SH2O(4)     !NOAH Liquid-only soil moisture (4 layers)
     REAL :: CH          !NOAH Heat/moisture exchange coef.
     REAL :: CM          !NOAH Momentum exchange coef.
     REAL :: FORCING(16) !TILE FORCING..
     REAL :: VEGT        !Vegetation type of tile
!-----------------------------------------------------------------------
!  NOAH-Output variables
!-----------------------------------------------------------------------
     REAL :: swnet 
     REAL :: lwnet
     REAL :: qle
     REAL :: qh
     REAL :: qg
     REAL :: snowf
     REAL :: rainf
     REAL :: evap
     REAL :: qs
     REAL :: qsb
     REAL :: qsm
     REAL :: avgsurft
     REAL :: albedo
     REAL :: swe
     REAL :: soilmoist1
     REAL :: soilmoist2
     REAL :: soilmoist3
     REAL :: soilmoist4
     REAL :: soilwet
     REAL :: ecanop
     REAL :: canopint
     REAL :: tveg
     REAL :: esoil
     REAL :: rootmoist
     REAL :: soilm_prev
     REAL :: swe_prev
  end type noahdec
!EOC
 end module noah_module


