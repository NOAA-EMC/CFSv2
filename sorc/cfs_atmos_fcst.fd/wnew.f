       SUBROUTINE W3FB11(ALAT,ELON,ALAT1,ELON1,DX,ELONV,ALATAN,XI,XJ)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM:  W3FB11        LAT/LON TO LAMBERT(I,J) FOR GRIB
C   PRGMMR: STACKPOLE        ORG: NMC42       DATE:88-11-28
C
C ABSTRACT: CONVERTS THE COORDINATES OF A LOCATION ON EARTH GIVEN IN
C   THE NATURAL COORDINATE SYSTEM OF LATITUDE/LONGITUDE TO A GRID
C   COORDINATE SYSTEM OVERLAID ON A LAMBERT CONFORMAL TANGENT CONE
C   PROJECTION TRUE AT A GIVEN N OR S LATITUDE. W3FB11 IS THE REVERSE
C   OF W3FB12. USES GRIB SPECIFICATION OF THE LOCATION OF THE GRID
C
C PROGRAM HISTORY LOG:
C   88-11-25  ORIGINAL AUTHOR:  STACKPOLE, W/NMC42
C   90-04-12  R.E.JONES   CONVERT TO CFT77 FORTRAN
C   94-04-28  R.E.JONES   ADD SAVE STATEMENT
C
C USAGE:  CALL W3FB11 (ALAT,ELON,ALAT1,ELON1,DX,ELONV,ALATAN,XI,XJ)
C   INPUT ARGUMENT LIST:
C     ALAT     - LATITUDE IN DEGREES (NEGATIVE IN SOUTHERN HEMIS)
C     ELON     - EAST LONGITUDE IN DEGREES, REAL*4
C     ALAT1    - LATITUDE  OF LOWER LEFT POINT OF GRID (POINT (1,1))
C     ELON1    - LONGITUDE OF LOWER LEFT POINT OF GRID (POINT (1,1))
C                ALL REAL*4
C     DX       - MESH LENGTH OF GRID IN METERS AT TANGENT LATITUDE
C     ELONV    - THE ORIENTATION OF THE GRID.  I.E.,
C                THE EAST LONGITUDE VALUE OF THE VERTICAL MERIDIAN
C                WHICH IS PARALLEL TO THE Y-AXIS (OR COLUMNS OF
C                OF THE GRID) ALONG WHICH LATITUDE INCREASES AS
C                THE Y-COORDINATE INCREASES.  REAL*4
C                THIS IS ALSO THE MERIDIAN (ON THE BACK SIDE OF THE
C                TANGENT CONE) ALONG WHICH THE CUT IS MADE TO LAY
C                THE CONE FLAT.
C     ALATAN   - THE LATITUDE AT WHICH THE LAMBERT CONE IS TANGENT TO
C                (TOUCHING) THE SPHERICAL EARTH.
C                 SET NEGATIVE TO INDICATE A
C                 SOUTHERN HEMISPHERE PROJECTION.
C
C   OUTPUT ARGUMENT LIST:
C     XI       - I COORDINATE OF THE POINT SPECIFIED BY ALAT, ELON
C     XJ       - J COORDINATE OF THE POINT; BOTH REAL*4
C
C   REMARKS: FORMULAE AND NOTATION LOOSELY BASED ON HOKE, HAYES,
C     AND RENNINGER'S "MAP PROJECTIONS AND GRID SYSTEMS...", MARCH 1981
C     AFGWC/TN-79/003
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY C916-128, CRAY Y-MP8/864, CRAY Y-MP EL2/256
C
C$$$
C
         SAVE
C
         DATA  RERTH /6.3712E+6/, PI/3.14159/
C
C        PRELIMINARY VARIABLES AND REDIFINITIONS
C
C        H = 1 FOR NORTHERN HEMISPHERE; = -1 FOR SOUTHERN
C
         IF (ALATAN.GT.0) THEN
           H = 1.
         ELSE
           H = -1.
         ENDIF
C
         RADPD  = PI    / 180.0
         REBYDX = RERTH / DX
         ALATN1 = ALATAN * RADPD
         AN     = H * SIN(ALATN1)
         COSLTN = COS(ALATN1)
C
C        MAKE SURE THAT INPUT LONGITUDES DO NOT PASS THROUGH
C        THE CUT ZONE (FORBIDDEN TERRITORY) OF THE FLAT MAP
C        AS MEASURED FROM THE VERTICAL (REFERENCE) LONGITUDE.
C
         ELON1L = ELON1
         IF ((ELON1 - ELONV).GT.180.)
     &     ELON1L = ELON1 - 360.
         IF ((ELON1 - ELONV).LT.(-180.))
     &     ELON1L = ELON1 + 360.
C
         ELONL = ELON
         IF ((ELON  - ELONV).GT.180.)
     &     ELONL  = ELON  - 360.
         IF ((ELON - ELONV).LT.(-180.))
     &     ELONL = ELON + 360.
C
         ELONVR = ELONV * RADPD
C
C        RADIUS TO LOWER LEFT HAND (LL) CORNER
C
         ALA1 =  ALAT1 * RADPD
         RMLL = REBYDX * (((COSLTN)**(1.-AN))*(1.+AN)**AN) *
     &           (((COS(ALA1))/(1.+H*SIN(ALA1)))**AN)/AN
C
C        USE LL POINT INFO TO LOCATE POLE POINT
C
         ELO1 = ELON1L * RADPD
         ARG = AN * (ELO1-ELONVR)
         POLEI = 1. - H * RMLL * SIN(ARG)
         POLEJ = 1. + RMLL * COS(ARG)
C
C        RADIUS TO DESIRED POINT AND THE I J TOO
C
         ALA =  ALAT * RADPD
         RM = REBYDX * ((COSLTN**(1.-AN))*(1.+AN)**AN) *
     &           (((COS(ALA))/(1.+H*SIN(ALA)))**AN)/AN
C
         ELO = ELONL * RADPD
         ARG = AN*(ELO-ELONVR)
         XI = POLEI + H * RM * SIN(ARG)
         XJ = POLEJ - RM * COS(ARG)
C
C        IF COORDINATE LESS THAN 1
C        COMPENSATE FOR ORIGIN AT (1,1)
C
         IF (XI.LT.1.)  XI = XI - 1.
         IF (XJ.LT.1.)  XJ = XJ - 1.
C
      RETURN
      END
       SUBROUTINE W3FB12(XI,XJ,ALAT1,ELON1,DX,ELONV,ALATAN,ALAT,ELON,
     &                               IERR)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM:  W3FB12        LAMBERT(I,J) TO LAT/LON FOR GRIB
C   PRGMMR: STACKPOLE        ORG: NMC42       DATE:88-11-28
C
C ABSTRACT: CONVERTS THE COORDINATES OF A LOCATION ON EARTH GIVEN IN A
C   GRID COORDINATE SYSTEM OVERLAID ON A LAMBERT CONFORMAL TANGENT
C   CONE PROJECTION TRUE AT A GIVEN N OR S LATITUDE TO THE
C   NATURAL COORDINATE SYSTEM OF LATITUDE/LONGITUDE
C   W3FB12 IS THE REVERSE OF W3FB11.
C   USES GRIB SPECIFICATION OF THE LOCATION OF THE GRID
C
C PROGRAM HISTORY LOG:
C   88-11-25  ORIGINAL AUTHOR:  STACKPOLE, W/NMC42
C   90-04-12  R.E.JONES   CONVERT TO CFT77 FORTRAN
C   94-04-28  R.E.JONES   ADD SAVE STATEMENT
C
C USAGE:  CALL W3FB12(XI,XJ,ALAT1,ELON1,DX,ELONV,ALATAN,ALAT,ELON,IERR,
C                                   IERR)
C   INPUT ARGUMENT LIST:
C     XI       - I COORDINATE OF THE POINT  REAL*4
C     XJ       - J COORDINATE OF THE POINT  REAL*4
C     ALAT1    - LATITUDE  OF LOWER LEFT POINT OF GRID (POINT 1,1)
C                LATITUDE <0 FOR SOUTHERN HEMISPHERE; REAL*4
C     ELON1    - LONGITUDE OF LOWER LEFT POINT OF GRID (POINT 1,1)
C                  EAST LONGITUDE USED THROUGHOUT; REAL*4
C     DX       - MESH LENGTH OF GRID IN METERS AT TANGENT LATITUDE
C     ELONV    - THE ORIENTATION OF THE GRID.  I.E.,
C                THE EAST LONGITUDE VALUE OF THE VERTICAL MERIDIAN
C                WHICH IS PARALLEL TO THE Y-AXIS (OR COLUMNS OF
C                THE GRID) ALONG WHICH LATITUDE INCREASES AS
C                THE Y-COORDINATE INCREASES.  REAL*4
C                THIS IS ALSO THE MERIDIAN (ON THE OTHER SIDE OF THE
C                TANGENT CONE) ALONG WHICH THE CUT IS MADE TO LAY
C                THE CONE FLAT.
C     ALATAN   - THE LATITUDE AT WHICH THE LAMBERT CONE IS TANGENT TO
C                (TOUCHES OR OSCULATES) THE SPHERICAL EARTH.
C                 SET NEGATIVE TO INDICATE A
C                 SOUTHERN HEMISPHERE PROJECTION; REAL*4
C
C   OUTPUT ARGUMENT LIST:
C     ALAT     - LATITUDE IN DEGREES (NEGATIVE IN SOUTHERN HEMI.)
C     ELON     - EAST LONGITUDE IN DEGREES, REAL*4
C     IERR     - .EQ. 0   IF NO PROBLEM
C                .GE. 1   IF THE REQUESTED XI,XJ POINT IS IN THE
C                         FORBIDDEN ZONE, I.E. OFF THE LAMBERT MAP
C                         IN THE OPEN SPACE WHERE THE CONE IS CUT.
C                  IF IERR.GE.1 THEN ALAT=999. AND ELON=999.
C
C   REMARKS: FORMULAE AND NOTATION LOOSELY BASED ON HOKE, HAYES,
C     AND RENNINGER'S "MAP PROJECTIONS AND GRID SYSTEMS...", MARCH 1981
C     AFGWC/TN-79/003
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY C916-128, CRAY Y-MP8/864, CRAY Y-MP EL2/256
C
C$$$
C
         LOGICAL NEWMAP
C
         SAVE
C
         DATA  RERTH /6.3712E+6/, PI/3.14159/, OLDRML/99999./
C
C        PRELIMINARY VARIABLES AND REDIFINITIONS
C
C        H = 1 FOR NORTHERN HEMISPHERE; = -1 FOR SOUTHERN
C
         IF (ALATAN.GT.0) THEN
           H = 1.
         ELSE
           H = -1.
         ENDIF
C
         PIBY2  = PI     / 2.0
         RADPD  = PI     / 180.0
         DEGPRD = 1.0    / RADPD
         REBYDX = RERTH  / DX
         ALATN1 = ALATAN * RADPD
         AN     = H * SIN(ALATN1)
         COSLTN = COS(ALATN1)
C
C        MAKE SURE THAT INPUT LONGITUDE DOES NOT PASS THROUGH
C        THE CUT ZONE (FORBIDDEN TERRITORY) OF THE FLAT MAP
C        AS MEASURED FROM THE VERTICAL (REFERENCE) LONGITUDE
C
         ELON1L = ELON1
         IF ((ELON1-ELONV).GT.180.)
     &     ELON1L = ELON1 - 360.
         IF ((ELON1-ELONV).LT.(-180.))
     &     ELON1L = ELON1 + 360.
C
         ELONVR = ELONV * RADPD
C
C        RADIUS TO LOWER LEFT HAND (LL) CORNER
C
         ALA1 =  ALAT1 * RADPD
         RMLL = REBYDX * ((COSLTN**(1.-AN))*(1.+AN)**AN) *
     &           (((COS(ALA1))/(1.+H*SIN(ALA1)))**AN)/AN
C
C        USE RMLL TO TEST IF MAP AND GRID UNCHANGED FROM PREVIOUS
C        CALL TO THIS CODE.  THUS AVOID UNNEEDED RECOMPUTATIONS.
C
         IF (RMLL.EQ.OLDRML) THEN
           NEWMAP = .FALSE.
         ELSE
           NEWMAP = .TRUE.
           OLDRML = RMLL
C
C          USE LL POINT INFO TO LOCATE POLE POINT
C
           ELO1 = ELON1L * RADPD
           ARG = AN * (ELO1-ELONVR)
           POLEI = 1. - H * RMLL * SIN(ARG)
           POLEJ = 1. + RMLL * COS(ARG)
         ENDIF
C
C        RADIUS TO THE I,J POINT (IN GRID UNITS)
C              YY REVERSED SO POSITIVE IS DOWN
C
         XX = XI - POLEI
         YY = POLEJ - XJ
         R2 = XX**2 + YY**2
C
C        CHECK THAT THE REQUESTED I,J IS NOT IN THE FORBIDDEN ZONE
C           YY MUST BE POSITIVE UP FOR THIS TEST
C
         THETA = PI*(1.-AN)
         BETA = ABS(ATAN2(XX,-YY))
         IERR = 0
         IF (BETA.LE.THETA) THEN
           IERR = 1
           ALAT = 999.
           ELON = 999.
           IF (.NOT.NEWMAP)  RETURN
         ENDIF
C
C        NOW THE MAGIC FORMULAE
C
         IF (R2.EQ.0) THEN
           ALAT = H * 90.0
           ELON = ELONV
         ELSE
C
C          FIRST THE LONGITUDE
C
           ELON = ELONV + DEGPRD * ATAN2(H*XX,YY)/AN
           ELON = AMOD(ELON+360., 360.)
C
C          NOW THE LATITUDE
C          RECALCULATE THE THING ONLY IF MAP IS NEW SINCE LAST TIME
C
           IF (NEWMAP) THEN
             ANINV = 1./AN
             ANINV2 = ANINV/2.
             THING = ((AN/REBYDX) ** ANINV)/
     &         ((COSLTN**((1.-AN)*ANINV))*(1.+ AN))
           ENDIF
           ALAT = H*(PIBY2 - 2.*ATAN(THING*(R2**ANINV2)))*DEGPRD
         ENDIF
C
C        FOLLOWING TO ASSURE ERROR VALUES IF FIRST TIME THRU
C         IS OFF THE MAP
C
         IF (IERR.NE.0) THEN
           ALAT = 999.
           ELON = 999.
           IERR = 2
         ENDIF
         RETURN
         END
       SUBROUTINE W3FB08(ALAT,ALON,ALAT1,ALON1,ALATIN,DX,XI,XJ)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM:  W3FB08        LAT/LON TO MERC (I,J) FOR GRIB
C   PRGMMR: STACKPOLE        ORG: NMC42       DATE:88-04-05
C
C ABSTRACT: CONVERTS A LOCATION ON EARTH GIVEN IN
C   THE COORDINATE SYSTEM OF LATITUDE/LONGITUDE TO AN (I,J)
C   COORDINATE SYSTEM OVERLAID ON A MERCATOR MAP PROJECTION
C   W3FB08 IS THE REVERSE OF W3FB09
C   USES GRIB SPECIFICATION OF THE LOCATION OF THE GRID
C
C PROGRAM HISTORY LOG:
C   88-03-01  ORIGINAL AUTHOR:  STACKPOLE, W/NMC42
C   90-04-12  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C
C USAGE:  CALL W3FB08 (ALAT,ALON,ALAT1,ALON1,ALATIN,DX,XI,XJ)
C   INPUT ARGUMENT LIST:
C     ALAT     - LATITUDE IN DEGREES (NEGATIVE IN SOUTHERN HEMIS)
C     ALON     - EAST LONGITUDE IN DEGREES, REAL*4
C     ALAT1    - LATITUDE  OF LOWER LEFT CORNER OF GRID (POINT (1,1))
C     ALON1    - LONGITUDE OF LOWER LEFT CORNER OF GRID (POINT (1,1))
C                ALL REAL*4
C     ALATIN   - THE LATITUDE AT WHICH THE MERCATOR CYLINDER
C                INTERSECTS THE EARTH
C     DX       - MESH LENGTH OF GRID IN METERS AT ALATIN
C
C   OUTPUT ARGUMENT LIST:
C     XI       - I COORDINATE OF THE POINT SPECIFIED BY ALAT, ALON
C     XJ       - J COORDINATE OF THE POINT; BOTH REAL*4
C
C   REMARKS: FORMULAE AND NOTATION LOOSELY BASED ON HOKE, HAYES,
C     AND RENNINGER'S "MAP PROJECTIONS AND GRID SYSTEMS...", MARCH 1981
C     AFGWC/TN-79/003
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/832
C
C$$$
C
         DATA  RERTH /6.3712E+6/, PI/3.1416/
C
C        PRELIMINARY VARIABLES AND REDIFINITIONS
C
         RADPD  = PI    / 180.0
         DEGPR  = 180.0 / PI
         CLAIN  = COS(RADPD*ALATIN)
         DELLON = DX   / (RERTH*CLAIN)
C
C        GET DISTANCE FROM EQUATOR TO ORIGIN ALAT1
C
         DJEO = 0.
         IF (ALAT1.NE.0.)
     &     DJEO = (ALOG(TAN(0.5*((ALAT1+90.0)*RADPD))))/DELLON
C
C        NOW THE I AND J COORDINATES
C
         XI = 1. + ((ALON - ALON1)/(DELLON*DEGPR))
         XJ = 1. + (ALOG(TAN(0.5*((ALAT + 90.) * RADPD))))/
     &               DELLON
     &             - DJEO
C
      RETURN
      END
       SUBROUTINE W3FB06(ALAT,ALON,ALAT1,ALON1,DX,ALONV,XI,XJ)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C
C SUBPROGRAM:  W3FB06        LAT/LON TO POLA (I,J) FOR GRIB
C   PRGMMR: STACKPOLE        ORG: NMC42       DATE:88-04-05
C
C ABSTRACT: CONVERTS THE COORDINATES OF A LOCATION ON EARTH GIVEN IN
C   THE NATURAL COORDINATE SYSTEM OF LATITUDE/LONGITUDE TO A GRID
C   COORDINATE SYSTEM OVERLAID ON A POLAR STEREOGRAPHIC MAP PRO-
C   JECTION TRUE AT 60 DEGREES N OR S LATITUDE. W3FB06 IS THE REVERSE
C   OF W3FB07. USES GRIB SPECIFICATION OF THE LOCATION OF THE GRID
C
C PROGRAM HISTORY LOG:
C   88-01-01  ORIGINAL AUTHOR:  STACKPOLE, W/NMC42
C   90-04-12  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C
C USAGE:  CALL W3FB06 (ALAT,ALON,ALAT1,ALON1,DX,ALONV,XI,XJ)
C   INPUT ARGUMENT LIST:
C     ALAT     - LATITUDE IN DEGREES (NEGATIVE IN SOUTHERN HEMIS)
C     ALON     - EAST LONGITUDE IN DEGREES, REAL*4
C     ALAT1    - LATITUDE  OF LOWER LEFT POINT OF GRID (POINT (1,1))
C     ALON1    - LONGITUDE OF LOWER LEFT POINT OF GRID (POINT (1,1))
C                ALL REAL*4
C     DX       - MESH LENGTH OF GRID IN METERS AT 60 DEG LAT
C                 MUST BE SET NEGATIVE IF USING
C                 SOUTHERN HEMISPHERE PROJECTION.
C                   190500.0 LFM GRID,
C                   381000.0 NH PE GRID, -381000.0 SH PE GRID, ETC.
C     ALONV    - THE ORIENTATION OF THE GRID.  I.E.,
C                THE EAST LONGITUDE VALUE OF THE VERTICAL MERIDIAN
C                WHICH IS PARALLEL TO THE Y-AXIS (OR COLUMNS OF
C                OF THE GRID)ALONG WHICH LATITUDE INCREASES AS
C                THE Y-COORDINATE INCREASES.  REAL*4
C                   FOR EXAMPLE:
C                   255.0 FOR LFM GRID,
C                   280.0 NH PE GRID, 100.0 SH PE GRID, ETC.
C
C   OUTPUT ARGUMENT LIST:
C     XI       - I COORDINATE OF THE POINT SPECIFIED BY ALAT, ALON
C     XJ       - J COORDINATE OF THE POINT; BOTH REAL*4
C
C   REMARKS: FORMULAE AND NOTATION LOOSELY BASED ON HOKE, HAYES,
C     AND RENNINGER'S "MAP PROJECTIONS AND GRID SYSTEMS...", MARCH 1981
C     AFGWC/TN-79/003
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/832
C
C$$$
C
         DATA  RERTH /6.3712E+6/, PI/3.1416/
         DATA  SS60  /1.86603/
C
C        PRELIMINARY VARIABLES AND REDIFINITIONS
C
C        H = 1 FOR NORTHERN HEMISPHERE; = -1 FOR SOUTHERN
C
C        REFLON IS LONGITUDE UPON WHICH THE POSITIVE X-COORDINATE
C        DRAWN THROUGH THE POLE AND TO THE RIGHT LIES
C        ROTATED AROUND FROM ORIENTATION (Y-COORDINATE) LONGITUDE
C        DIFFERENTLY IN EACH HEMISPHERE
C
         IF (DX.LT.0) THEN
           H      = -1.0
           DXL    = -DX
           REFLON = ALONV - 90.0
         ELSE
           H      = 1.0
           DXL    = DX
           REFLON = ALONV - 270.0
         ENDIF
C
         RADPD  = PI / 180.0
         REBYDX = RERTH/DXL
C
C        RADIUS TO LOWER LEFT HAND (LL) CORNER
C
         ALA1 =  ALAT1 * RADPD
         RMLL = REBYDX * COS(ALA1) * SS60/(1. + H * SIN(ALA1))
C
C        USE LL POINT INFO TO LOCATE POLE POINT
C
         ALO1  = (ALON1 - REFLON) * RADPD
         POLEI = 1. - RMLL * COS(ALO1)
         POLEJ = 1. - H * RMLL * SIN(ALO1)
C
C        RADIUS TO DESIRED POINT AND THE I J TOO
C
         ALA = ALAT   * RADPD
         RM  = REBYDX * COS(ALA) * SS60/(1. + H * SIN(ALA))
C
         ALO = (ALON - REFLON) * RADPD
         XI  = POLEI + RM * COS(ALO)
         XJ  = POLEJ + H * RM * SIN(ALO)
C
      RETURN
      END
        subroutine baciolgwv()
          print *, ' GWVX REPLACE WITH A REAL BACIOL ROUTINE'
          stop 99
            end
        subroutine dgeff()
          print *, ' GWVX REPLACE WITH A REAL DGEF   ROUTINE'
          stop 99
            end
        subroutine dgesf()
          print *, ' GWVX REPLACE WITH A REAL DGES   ROUTINE'
          stop 99
            end
!       subroutine ludcmp()
!         print *, ' GWVX REPLACE WITH A REAL LUDCMP  ROUTINE'
!         stop 99
!           end
!       subroutine lubksb()
!         print *, ' GWVX REPLACE WITH A REAL LUbksb  ROUTINE'
!         stop 99
!           end
           subroutine dgef()
            print *,' GWVX REPLACE WITH A REAL DGEF'
           end
           subroutine dges()
            print *,' GWVX REPLACE WITH A REAL DGEs'
           end
