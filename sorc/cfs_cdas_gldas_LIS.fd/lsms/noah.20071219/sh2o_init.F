!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: sh2o_init.F90
!
! !DESCRIPTION:
!  To do a 'cold start' initialization of soil liquid water SH2O for 
!   NOAH LSM, though also adaptable for other land-surface models,
!   using either GDAS or Eta forcing data. 
!
! !REVISION HISTORY:
!  NCEP;  Original code developed by NCEP for Eta model
!           subroutines to initialize soil liquid water, SH2O   
!  04 Nov 2002: Kristi Arsenault; Modified code to be used with noahrst.f
!                                 to initialize NOAH with NCEP forcing data  
!
!
! !INTERFACE:
subroutine sh2oinit(smc,stc,smcmax,psis,beta,sh2o)

  implicit none      
! !ARGUMENTS:
  REAL STC      ! NOAH Soil Layer Temperature (K)
  REAL SMC      ! NOAH Soil Layer Total Moisture (liq+frzn) 
  REAL SH2O     ! NOAH Soil Layer Liquid Moisture
  
  REAL  PSIS                    ! Saturated soil potential
  REAL  BETA                    ! B-parameter 
  REAL  SMCMAX                  ! Max soil moisture content (porosity)
  REAL  BX
!EOP
  REAL  FK
  REAL  FRH2O 

  REAL :: HLICE=3.335E5         ! Ice parameter
  REAL :: GRAV=9.81             ! Gravity (m s-1) 
  REAL :: T0=273.15             ! Freezing point of water
  REAL :: BLIM=5.5              ! B-parameter upper limit

!=== End Variable Definition =============================================
!BOC
! ----------------------------------------------------------------------
! COLD START:  determine liquid soil water content (SH2O)
! NSOIL number of soil layers
! ----------------------------------------------------------------------
!  SH2O <= SMC for T < 273.149K (-0.001C)
  IF (STC .LT. 273.149) THEN
! ----------------------------------------------------------------------
! first guess following explicit solution for Flerchinger Eqn from Koren 
! et al, JGR, 1999, Eqn 17 (KCOUNT=0 in FUNCTION FRH2O). 
! ----------------------------------------------------------------------
     BX = BETA
     IF ( BETA .GT. BLIM )  BX = BLIM
     FK=(((HLICE/(GRAV*(-PSIS)))* & 
          ((STC-T0)/STC))**(-1/BX))*SMCMAX
     IF (FK .LT. 0.02) FK = 0.02
     SH2O = MIN ( FK, SMC )
! ----------------------------------------------------------------------
! now use iterative solution for liquid soil water content using 
! FUNCTION FRH2O with the initial guess for SH2O from above explicit
! first guess.
! ----------------------------------------------------------------------
     SH2O = FRH2O(STC,SMC,SH2O,SMCMAX,BETA,PSIS)

  ELSE
! ----------------------------------------------------------------------
!  SH2O = SMC for T => 273.149K (-0.001C)
     SH2O=SMC
! ----------------------------------------------------------------------
  ENDIF
  
  RETURN 
!EOC
END subroutine sh2oinit

!===========================================================================

FUNCTION FRH2O(TKELV,SMC,SH2O,SMCMAX,BEXP,PSIS)
  
  IMPLICIT NONE
  
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  PURPOSE:  CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT 
!  IF TEMPERATURE IS BELOW 273.15K (T0).  REQUIRES NEWTON-TYPE ITERATION 
!  TO SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF 
!  KOREN ET AL. (1999, JGR, VOL 104(D16), 19569-19585).
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! New version (JUNE 2001): much faster and more accurate newton iteration 
! achieved by first taking log of eqn cited above -- less than 4
! (typically 1 or 2) iterations achieves convergence.  Also, explicit  
! 1-step solution option for special case of parameter Ck=0, which reduces 
! the original implicit equation to a simpler explicit form, known as the 
! ""Flerchinger Eqn". Improved handling of solution in the limit of  
! freezing point temperature T0. 
! 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! INPUT:
!   TKELV.........Temperature (Kelvin)
!   SMC...........Total soil moisture content (volumetric)
!   SH2O..........Liquid soil moisture content (volumetric)
!   SMCMAX........Saturation soil moisture content (from REDPRM)
!   B.............Soil type "B" parameter (from REDPRM)
!   PSIS..........Saturated soil matric potential (from REDPRM)
!
! OUTPUT:
!   FRH2O.........supercooled liquid water content.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

       REAL BEXP
       REAL BLIM
       REAL BX
       REAL CK
       REAL DENOM
       REAL DF
!       REAL DH2O
!       REAL DICE
       REAL DSWL
       REAL ERROR
       REAL FK
       REAL FRH2O
       REAL GS
       REAL HLICE
       REAL PSIS
       REAL SH2O
       REAL SMC
       REAL SMCMAX
       REAL SWL
       REAL SWLK
       REAL TKELV
       REAL T0

       INTEGER NLOG
       INTEGER KCOUNT

       PARAMETER (CK=8.0)
!      PARAMETER (CK=0.0)
       PARAMETER (BLIM=5.5)
!      PARAMETER (BLIM=7.0)
       PARAMETER (ERROR=0.005)

       PARAMETER (HLICE=3.335E5)
       PARAMETER (GS = 9.81)
!       PARAMETER (DICE=920.0)
!       PARAMETER (DH2O=1000.0)
       PARAMETER (T0=273.15)

!  ###   LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)  ####
!  ###   SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT  ####
!  ###   IS NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES    ####
!##################################################################

      BX = BEXP
      IF ( BEXP .GT. BLIM ) BX = BLIM
!------------------------------------------------------------------

! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
      NLOG=0
      KCOUNT=0

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (T0), SH2O = SMC 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (TKELV .GT. (T0 - 1.E-3)) THEN
         FRH2O=SMC

      ELSE

        IF (CK .NE. 0.0) THEN

! -------------------------------------------------------------
! OPTION 1: ITERATED SOLUTION FOR NONZERO CK
! IN KOREN ET AL, JGR, 1999, EQN 17
! -------------------------------------------------------------
! INITIAL GUESS FOR SWL (frozen content)
         SWL = SMC-SH2O

! KEEP WITHIN BOUNDS.
          IF (SWL .GT. (SMC-0.02)) SWL=SMC-0.02
          IF(SWL .LT. 0.) SWL=0. 
!--------------------------------------------------------------
!  START OF ITERATIONS 
!--------------------------------------------------------------
         DO WHILE (NLOG .LT. 10 .AND. KCOUNT .EQ. 0)
          NLOG = NLOG+1
          DF = ALOG(( PSIS*GS/HLICE ) * ( ( 1.+CK*SWL )**2. ) * & 
          ( SMCMAX/(SMC-SWL) )**BX) - ALOG(-(TKELV-T0)/TKELV)
          DENOM = 2. * CK / ( 1.+CK*SWL ) + BX / ( SMC - SWL )
          SWLK = SWL - DF/DENOM
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
          IF (SWLK .GT. (SMC-0.02)) SWLK = SMC - 0.02
          IF(SWLK .LT. 0.) SWLK = 0.
! MATHEMATICAL SOLUTION BOUNDS APPLIED.
          DSWL=ABS(SWLK-SWL)
          SWL=SWLK 

!---------------------------------------------------------------
! IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)  
! WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED. 
!---------------------------------------------------------------
          IF ( DSWL .LE. ERROR )  THEN
            KCOUNT=KCOUNT+1
          END IF
         END DO 
!---------------------------------------------------------------
!  END OF ITERATIONS 
!---------------------------------------------------------------
! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
         FRH2O = SMC - SWL

!CCCCCCCCCCCCCCCCCCCCCCC END OPTION 1 CCCCCCCCCCCCCCCCCCCCCCCCCCC

        ENDIF

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0
! IN KOREN ET AL., JGR, 1999, EQN 17
!----------------------------------------------------------------
        IF (KCOUNT .EQ. 0) THEN
!      Print*,'Flerchinger used in NEW version. Iterations=',NLOG
          FK=(((HLICE/(GS*(-PSIS)))*((TKELV-T0)/TKELV))** & 
           (-1/BX))*SMCMAX
!  APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
          IF (FK .LT. 0.02) FK = 0.02
          FRH2O = MIN ( FK, SMC )

!CCCCCCCCCCCCCCCCCCCCCCCCC END OPTION 2 CCCCCCCCCCCCCCCCCCCCCCCCCC

        ENDIF

      ENDIF

      RETURN
      END

