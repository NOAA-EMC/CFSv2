 SUBROUTINE LAMBCONF(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET)
!                .      .    .                                       .
! subprogram:    lambconf
!   prgmmr: gayno          org: w/np2     date: 2015-jan-30
!
! $Revision$
!
! abstract: this subroutine transforms from i/j to lat/lon and
!   vice versa for a lambert conformal grid with an elliptical
!   earth.
!
! program history log:
! 2015-jan-30  gayno    - initial version
!
! input argument list:
!     KGDS     - INTEGER (200) GDS PARAMETERS AS DECODED BY W3FI63
!     IOPT     - INTEGER OPTION FLAG
!                (+1 TO COMPUTE EARTH COORDS OF SELECTED GRID COORDS)
!                (-1 TO COMPUTE GRID COORDS OF SELECTED EARTH COORDS)
!     NPTS     - INTEGER MAXIMUM NUMBER OF COORDINATES
!     FILL     - REAL FILL VALUE TO SET INVALID OUTPUT DATA
!                (MUST BE IMPOSSIBLE VALUE; SUGGESTED VALUE: -9999.)
!     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT>0
!     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT>0
!     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT<0
!                (ACCEPTABLE RANGE: -360. TO 360.)
!     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT<0
!                (ACCEPTABLE RANGE: -90. TO 90.)
!
! output argument list:
!     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT<0
!     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT<0
!     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT>0
!     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT>0
!     NRET     - INTEGER NUMBER OF VALID POINTS COMPUTED
!
! files: none
!
! condition codes: none
!
! remarks: based on ncep ipolates library routine gdswzd03.
!
!$$$
!
 IMPLICIT NONE

 INTEGER, INTENT(IN)  :: KGDS(22), IOPT, NPTS
 INTEGER, INTENT(OUT) :: NRET

 REAL,    INTENT(IN)  :: FILL

 REAL, PARAMETER :: PI=3.14159265358979
 REAL, PARAMETER :: DPR=180./PI
 REAL, PARAMETER :: A=6378137.0
 REAL, PARAMETER :: B=6356752.314

 INTEGER :: IM, JM, IPROJ, ISCAN, JSCAN
 INTEGER :: ITER, N

 REAL :: DENOM
 real :: bigf, e, m, m1, m2, t, t1, t2
 real :: AN, latguess, rho, de
 REAL :: RLAT1, RLON1, ORIENT, DX, DY, H, HI, HJ
 REAL :: RLATI1, RLATI2, DXS, DYS, XP, YP
 REAL :: DLON, DLON1, XMAX, YMAX, XMIN, YMIN
 REAL :: DI, DJ, DR, DR2
 REAL :: XPTS(NPTS),YPTS(NPTS),RLON(NPTS),RLAT(NPTS)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 E = SQRT(1. - B**2/A**2)
 IM=KGDS(8)
 JM=KGDS(9)
 RLAT1=KGDS(10)*1.E-6
 RLON1=KGDS(11)*1.E-6
 ORIENT=KGDS(14)*1.E-6
 DX=KGDS(15)*1.E-3
 DY=KGDS(16)*1.E-3
 IPROJ=MOD(KGDS(17)/128,2)
 ISCAN=MOD(KGDS(18)/128,2)
 JSCAN=MOD(KGDS(18)/64,2)
 RLATI1=KGDS(19)*1.E-6
 RLATI2=KGDS(20)*1.E-6
 H=(-1.)**IPROJ
 HI=(-1.)**ISCAN
 HJ=(-1.)**(1-JSCAN)
 DXS=DX*HI
 DYS=DY*HJ
 IF(RLATI1.EQ.RLATI2) THEN
   AN=SIN(H*RLATI1/DPR)
 ELSE
   M1 = COS(RLATI1/DPR) / SQRT(1.0 - E**2*SIN(RLATI1/DPR)**2)
   M2 = COS(RLATI2/DPR) / SQRT(1.0 - E**2*SIN(RLATI2/DPR)**2)
   DENOM = ((1.0-E*SIN(RLATI1/DPR)) /    &
            (1.0+E*SIN(RLATI1/DPR)))**(E/2.)
   T1 = TAN((H*90-RLATI1)/2/DPR) / DENOM
   DENOM = ((1.0-E*SIN(RLATI2/DPR)) /    &
            (1.0+E*SIN(RLATI2/DPR)))**(E/2.)
   T2 = TAN((H*90-RLATI2)/2/DPR) / DENOM
   AN = (LOG(M1)-LOG(M2)) / (LOG(T1)-LOG(T2))
   BIGF = M1 / (AN*(T1**AN))
 ENDIF
 DENOM = ((1.0-E*SIN(RLAT1/DPR)) /    &
          (1.0+E*SIN(RLAT1/DPR)))**(E/2.)
 T = TAN((H*90-RLAT1)/2/DPR) / DENOM
 RHO = A * BIGF * T**AN
 DR = RHO
 DLON1=MOD(RLON1-ORIENT+180.+3600.,360.)-180.0
 XP=1.0-H*SIN(AN*DLON1/DPR)*DR/DXS
 YP=1.0+COS(AN*DLON1/DPR)*DR/DYS
 XMIN=0
 XMAX=IM+1
 YMIN=0
 YMAX=JM+1
 NRET=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  TRANSLATE GRID COORDINATES TO EARTH COORDINATES
 IF(IOPT.EQ.0.OR.IOPT.EQ.1) THEN
   DO N=1,NPTS
     IF(XPTS(N).GE.XMIN.AND.XPTS(N).LE.XMAX.AND.   &
        YPTS(N).GE.YMIN.AND.YPTS(N).LE.YMAX) THEN
       DI=(XPTS(N)-XP)*DXS
       DJ=(YPTS(N)-YP)*DYS
       DR2=DI**2+DJ**2
       RLON(N)=MOD(ORIENT+H/AN*DPR*ATAN2(DI,-DJ)+3600.,360.)
       RHO = H*SQRT(DR2)
       T = (RHO/(A*BIGF))**(1./AN)
       LATGUESS = (PI/2.) - 2.*ATAN(T)
       DO ITER = 1, 10
         DENOM = ((1. - E*SIN(LATGUESS)) /    &
                  (1. + E*SIN(LATGUESS))) ** (E/2.)
         RLAT(N) = (PI/2.) - 2*ATAN(T*DENOM)
         IF (ABS(RLAT(N)-LATGUESS) < .000001) EXIT
         LATGUESS = RLAT(N)
       ENDDO
       RLAT(N) = RLAT(N)*DPR
       NRET=NRET+1
     ELSE
       RLON(N)=FILL
       RLAT(N)=FILL
     ENDIF
   ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  TRANSLATE EARTH COORDINATES TO GRID COORDINATES
 ELSEIF(IOPT.EQ.-1) THEN
   DO N=1,NPTS
     IF(ABS(RLON(N)).LE.360.AND.ABS(RLAT(N)).LE.90.AND.   &
                                    H*RLAT(N).NE.-90) THEN
       DENOM = ((1.0-E*SIN(RLAT(N)/DPR))/   &
                (1.0+E*SIN(RLAT(N)/DPR)))**(E/2.)
       T = TAN((H*90-RLAT(N))/2/DPR) / DENOM
       RHO = A * BIGF * T**AN
       DR = RHO
       DLON=MOD(RLON(N)-ORIENT+180.+3600.,360.)-180.
       XPTS(N)=XP+H*SIN(AN*DLON/DPR)*DR/DXS
       YPTS(N)=YP-COS(AN*DLON/DPR)*DR/DYS
       NRET=NRET+1
     ELSE
       XPTS(N)=FILL
       YPTS(N)=FILL
     ENDIF
   ENDDO
 ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 END SUBROUTINE LAMBCONF
