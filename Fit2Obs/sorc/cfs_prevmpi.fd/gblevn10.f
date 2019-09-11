C#######################
C#######################
C#######################
C#######################
C#######################
C#######################
C***********************************************************************
C***********************************************************************
      SUBROUTINE GBLEVN10(IUNITF,IDATEP,IM,JM,KBAK) ! FORMERLY
                                                    ! SUBROUTINE GESRES
      USE GBLEVN_MODULE
      USE SIGIO_MODULE
      USE SIGIO_R_MODULE

      IMPLICIT NONE
      INTEGER IUNITF(2), IDATEP, IM, JM, KBAK 
      REAL, PARAMETER :: PI180=.0174532
      INTEGER*4, PARAMETER :: ONE=1, TEN=10

      TYPE(SIGIO_HEAD) :: HEAD(2)
      TYPE(SIGIO_DATS) :: DATS
      TYPE(SIGIO_DATM) :: DATM

      INTEGER*4 IDVC,IDSL,NVCOORD,SFCPRESS_ID,THERMODYN_ID
      INTEGER*4 IRET,IRET1,IRETS,IMJM4,KM4,IDVM,NTRAC,IUNIT4(2)

      INTEGER KFILES,IFILE,JFILE,IDATGS_COR,JCAP,JCAP1,JCAP2,JCAP1X2,
     $ MDIMA,MDIMB,MDIMC,IROMB,MAXWV,IDIR,NS,I,J,K,L,II,JJ,IB,IE
     $ ,latb,lonb  

      INTEGER IDATE(8,2),JDATE(8,2),KDATE(8,2),KINDX(2)

      CHARACTER*6  COORD(3)
      CHARACTER*20 CFILE

      REAL FHOUR

      DATA COORD /'SIGMA ','HYBRID','GENHYB'/

      REAL,         ALLOCATABLE :: cofs(:,:),     cofv(:,:,:)
      REAL,         ALLOCATABLE :: cofs_f(:,:,:), cofv_f(:,:,:,:)
      REAL (KIND=4),ALLOCATABLE :: grds(:,:,:),   grdv(:,:,:,:),
     $                             wrk1(:,:),     wrk2(:,:)

      IMAX  = IM
      JMAX  = JM
      IMJM4 = IM*JM
      IUNIT4(:) = IUNITF(:)

      IF(NBAK/=2)  THEN
         KFILES = 1
         KINDX = 0
         PRINT 331,kbak,nbak          
  331    FORMAT(/' --> GBLEVENTS: background time ',i2,' of ',i2)  
      ELSE
         KFILES = 2
         KINDX(1) = MOD(MOD(IDATEP,100),3)
         KINDX(2) = KINDX(1) - 3
         PRINT 332, MOD(IDATEP,100)
  332    FORMAT(/' --> GBLEVENTS: THE PREPBUFR CENTER HOUR (',I2.2,
     $    ') IS NOT A MULTIPLE OF 3 - TWO SPANNING GLOBAL SIGMA OR ',
     $    'HYBRID FILES'/16X,'ARE READ AND THE SPECTRAL COEFFICIENTS ',
     $    'ARE INTERPOLATED TO THE PREPBUFR CENTER TIME'/)
      ENDIF

C  GET VALID-TIME DATE OF SIGMA OR HYBRID FILE(S), ALSO READ HEADERS
C  -----------------------------------------------------------------

      JFILE = 0
      DO IFILE=1,KFILES
         JFILE = IFILE; idate(:,ifile)=0

         WRITE(CFILE,'("fort.",I2.2)') IUNITF(IFILE)

         print *,' cfile=',cfile

         CALL SIGIO_RROPEN(IUNIT4(IFILE),CFILE,IRET)
         CALL SIGIO_RRHEAD(IUNIT4(IFILE),HEAD(IFILE),IRET1)

         IF(IRET.NE.0)  GO TO 903
         IF(IRET1.NE.0) GO TO 904

         IDATE(1,IFILE)   = HEAD(IFILE)%IDATE(4)
         IDATE(2:3,IFILE) = HEAD(IFILE)%IDATE(2:3)
         IDATE(5,IFILE)   = HEAD(IFILE)%IDATE(1)

         FHOUR = HEAD(IFILE)%FHOUR
         print *,' idate=',idate(:,ifile),' fhour=',head(ifile)%fhour

         IF(IDATE(1,IFILE).LT.100)  THEN

C IF 2-DIGIT YEAR FOUND IN GLOBAL SIMGA FILE INITIAL DATE
C  (IDATE(1,IFILE)), MUST USE "WINDOWING" TECHNIQUE TO CREATE A 4-DIGIT
C  YEAR (NOTE: THE T170 IMPLEMENTATION IN JUNE 1998 WAS TO INCLUDE THE
C  WRITING OF A 4-DIGIT YEAR HERE.  PRIOR TO THIS, THE YEAR HERE WAS
C  2-DIGIT.)

            PRINT *, '##GBLEVENTS/GBLEVN10 - 2-DIGIT YEAR FOUND IN ',
     $       'GLOBAL SIGMA OR HYBRID FILE ',IFILE,'; INITIAL DATE ',
     $       '(YEAR IS: ',idate(1,IFILE),')'
            PRINT *, '     - USE WINDOWING TECHNIQUE TO OBTAIN 4-DIGIT',
     $       ' YEAR'
            IF(IDATE(1,IFILE).GT.20)  THEN
               IDATE(1,IFILE) = 1900 + IDATE(1,IFILE)
            ELSE
               IDATE(1,IFILE) = 2000 + IDATE(1,IFILE)
            ENDIF
            PRINT *,'##GBLEVENTS/GBLEVN10 - CORRECTED 4-DIGIT YEAR IS ',
     $       'NOW: ',IDATE(1,IFILE)
         ENDIF

         CALL W3MOVDAT((/0.,FHOUR,0.,0.,0./),IDATE(:,IFILE),
     $                 JDATE(:,IFILE))

         PRINT 1, kbak,HEAD(IFILE)%FHOUR,
     $    (IDATE(II,IFILE),II=1,3),IDATE(5,IFILE),(JDATE(II,IFILE),
     $    II=1,3),JDATE(5,IFILE)
    1    FORMAT(' --> GBLEVENTS: GLOBAL SIGMA OR HYBRID FILE',I2,
     $    ' HERE IS A ',F5.1,' HOUR FORECAST FROM ',I5.4,3I3.2,' VALID',
     $    ' AT ',I5.4,3I3.2)

         KDATE(:,IFILE) = JDATE(:,IFILE)

         IF(KFILES.EQ.2) CALL W3MOVDAT((/0.,REAL(KINDX(IFILE)),0.,0.,
     $    0./),JDATE(:,IFILE),KDATE(:,IFILE))

         IDATGS_COR = (KDATE(1,IFILE) * 1000000) + (KDATE(2,IFILE) *
     $    10000) + (KDATE(3,IFILE) * 100) + KDATE(5,IFILE)

C  VALID DATES MUST MATCH
C  ----------------------

         IF(IDATEP.NE.IDATGS_COR)  GO TO 901

      ENDDO


C  EXTRACT HEADER INFO
C  -------------------
 
      JCAP    = HEAD(1)%JCAP
      KMAX    = HEAD(1)%LEVS
      latb    = HEAD(1)%latb
      lonb    = HEAD(1)%lonb
      KM4     = KMAX
      IDVC    = HEAD(1)%IDVC
      IDVM    = HEAD(1)%IDVM
      NTRAC   = HEAD(1)%NTRAC
      NVCOORD = HEAD(1)%NVCOORD
      ALLOCATE (VCOORD(KMAX+1,NVCOORD))
      VCOORD  = HEAD(1)%VCOORD

      if(imax<=0) then
         IMAX  = min(2304,lonb)
         JMAX  = min(1152,latb)
         IMJM4 = IMax*JMax
         print*,'latb,lonb=',latb,lonb
      endif

      SFCPRESS_ID  = MOD(HEAD(1)%IDVM,TEN)
      THERMODYN_ID = MOD(HEAD(1)%IDVM/TEN,TEN)
      IF(IDVC == 3 .AND. THERMODYN_ID == 3) THEN
         KMAXS = (NTRAC+1)*KMAX + 2
      ELSE
         KMAXS = 2*KMAX + 2
         NTRAC = 1
      ENDIF

      print*,'allocate',imax,jmax,kmax
      ALLOCATE (iar12z(imax,jmax), iar13p(imax,jmax))
      ALLOCATE (iar14t(imax,jmax,kmax),  iar15u(imax,jmax,kmax),
     $          iar16v(imax,jmax,kmax),  iar17q(imax,jmax,kmax),
     $          iarpsl(imax,jmax,kmax),  iarpsi(imax,jmax,kmax+1))


      if(idvc.eq.0)  idvc = 1  ! Reset IDVC=0 to 1 (sigma coord.)
      IF(IDVC < 0 .or. IDVC > 3) THEN
         PRINT *, '##GBLEVENTS/GBLEVN10: INVALID VERT COORD ID (=',IDVC
      ENDIF


C  DEFINE THE OTHER RESOLUTION PARAMETERS
C  --------------------------------------
 
      JCAP1   = JCAP+1
      JCAP2   = JCAP+2
      JCAP1X2 = JCAP1*2
      MDIMA   = JCAP1*JCAP2
      MDIMB   = MDIMA/2+JCAP1
      MDIMC   = MDIMB*2
      !IMAX    = 384
      !JMAX    = IMAX/2+1
 
      DLAT  = 180./(JMAX-1)
      DLON  = 360./IMAX
 
      PRINT 2, JCAP,KMAX,kmaxs,DLAT,DLON,COORD(IDVC)
    2 FORMAT(/' --> GBLEVENTS: GLOBAL MODEL SPECS: T',I5,' ',I3,
     $ ' LEVELS ',I3,' SCALARS -------> ',F5.2,' X ',F5.2,' VERT. ',
     $ 'COORD: ',A)
 
      GO TO 902

  901 CONTINUE
      PRINT 9901, JFILE,(JDATE(II,JFILE),II=1,3),JDATE(5,JFILE),IDATEP
 9901 FORMAT(/' ##GBLEVENTS/GBLEVN10 - SIGMA OR HYBRID FILE',I2,' DATE',
     $ ' (',I4.4,3(I2.2),'), DOES NOT MATCH -OR SPAN- PREPBUFR FILE ',
     $ 'CENTER DATE (',I10,') -STOP 68'/)
      CALL ERREXIT(68)
  903 CONTINUE
      PRINT 9903, JFILE,IRET
 9903 FORMAT(/' ##GBLEVENTS/GBLEVN10 - SIGMA OR HYBRID FILE',I2,
     $ ' RETURNED FROM SIGIO_RROPEN WITH R.C.',I3,' -STOP 70'/)
      CALL ERREXIT(70)
  904 CONTINUE
      PRINT 9904, JFILE,IRET1
 9904 FORMAT(/' ##GBLEVENTS/GBLEVN10 - SIGMA OR HYBRID FILE',I2,
     $ ' RETURNED FROM SIGIO_RRHEAD WITH R.C.',I3,' -STOP 71'/)
      CALL ERREXIT(71)
  902 CONTINUE
      IF(KMAX.GT.500) then
         PRINT *,'##GBLEVENTS/GBLEVN10 - KMAX TOO BIG = ',KMAX,
     $    ' - UNABLE',' TO TRANSFORM GLOBAL SIGMA FILE(S) - STOP 69'
         CALL ERREXIT(69)
      ENDIF

C***********************************************************************
C***********************************************************************

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C USAGE:    CALL SPTEZM(IROMB,MAXWV,IDRT,IMAX,JMAX,KMAX,WAVE,GRID,IDIR)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRT     - INTEGER GRID IDENTIFIER
C                (IDRT=4 FOR GAUSSIAN GRID,
C                 IDRT=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRT=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAX     - INTEGER EVEN NUMBER OF LONGITUDES
C     JMAX     - INTEGER NUMBER OF LATITUDES
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM
C     WAVE     - REAL (2*MX,KMAX) WAVE FIELD IF IDIR>0
C                WHERE MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
C     GRID     - REAL (IMAX,JMAX,KMAX) GRID FIELD (E->W,N->S) IF IDIR<0
C     IDIR     - INTEGER TRANSFORM FLAG
C                (IDIR>0 FOR WAVE TO GRID, IDIR<0 FOR GRID TO WAVE)
C   OUTPUT ARGUMENTS:
C     WAVE     - REAL (2*MX,KMAX) WAVE FIELD IF IDIR<0
C                WHERE MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
C     GRID     - REAL (IMAX,JMAX,KMAX) GRID FIELD (E->W,N->S) IF IDIR>0


C USAGE:    CALL SPTEZMV(IROMB,MAXWV,IDRT,IMAX,JMAX,KMAX,
C    &                   WAVED,WAVEZ,GRIDU,GRIDV,IDIR)
C   INPUT ARGUMENTS:
C     WAVED    - REAL (2*MX,KMAX) WAVE DIVERGENCE FIELD IF IDIR>0
C                WHERE MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
C     WAVEZ    - REAL (2*MX,KMAX) WAVE VORTICITY FIELD IF IDIR>0
C                WHERE MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
C     GRIDU    - REAL (IMAX,JMAX,KMAX) GRID U-WIND (E->W,N->S) IF IDIR<0
C     GRIDV    - REAL (IMAX,JMAX,KMAX) GRID V-WIND (E->W,N->S) IF IDIR<0
C   OUTPUT ARGUMENTS:
C     WAVED    - REAL (2*MX,KMAX) WAVE DIVERGENCE FIELD IF IDIR<0
C                WHERE MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
C     WAVEZ    - REAL (2*MX,KMAX) WAVE VORTICITY FIELD IF IDIR>0
C                WHERE MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
C     GRIDU    - REAL (IMAX,JMAX,KMAX) GRID U-WIND (E->W,N->S) IF IDIR>0
C     GRIDV    - REAL (IMAX,JMAX,KMAX) GRID V-WIND (E->W,N->S) IF IDIR>0

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      IROMB = 0
      MAXWV = JCAP
      !!if (idrt < 0 .or. idrt > 256) IDRT  = 0
      IDIR  = 1

      IF(KINDX(1).EQ.0)  THEN
         KFILES = 1
         ifile=1
      ELSE
         KFILES = 2
      ENDIF

C  Allocate for sigio read
C  -----------------------

      SFCPRESS_ID  = MOD(HEAD(1)%IDVM,TEN)
      THERMODYN_ID = MOD(HEAD(1)%IDVM/TEN,TEN)

!     print *,' sfcpress_id=',sfcpress_id,' thermodyn_id=',
!    $ thermodyn_id,' cpi(0)=',head(1)%cpi(1)

!  copy the spectral coefficients into memory

         CALL SIGIO_ALDATS(HEAD(IFILE),DATS,IRETS)
         CALL SIGIO_ALDATM(HEAD(IFILE),ONE,KM4,DATM,IRETS)

         CALL SIGIO_RRDATS(IUNIT4(IFILE),HEAD(IFILE),DATS,IRETS)
         IF(IRETS.NE.0) THEN
            print *,' irets from sigio_rrdats = ', irets
            RETURN
         ENDIF

         CALL SIGIO_RRDATM(IUNIT4(IFILE),HEAD(IFILE),DATM,IRETS)
         IF(IRETS.NE.0) THEN
            print *,' irets from sigio_rrdatm = ', irets
            RETURN
         ENDIF

         if(kbak==1) call prttime('sigio')

!  store coefficients in working memory

      ALLOCATE (COFS(MDIMA,KMAXS),     COFV(MDIMA,KMAX,2))
      ALLOCATE (GRDS(imax,jmax,KMAXS), GRDV(imax,jmax,KMAX,2))
      ALLOCATE (WRK1(imax*jmax,KMAX),  WRK2(imax*jmax,KMAX+1))

      COFS(:,1) = DATS%HS(:)
      COFS(:,2) = DATS%PS(:)
!$OMP PARALLEL DO PRIVATE(k)                  
      do k=1,kmax
      cofs(:,2+k)      = datm%t(:,k)
      cofs(:,kmax+2+k) = datm%q(:,k,1)
      cofv(:,k,1)      = datm%d(:,k)
      cofv(:,k,2)      = datm%z(:,k)
      enddo
         
!  run the transform

      CALL SPTEZM(IROMB,MAXWV,IDRT,IMAX,JMAX,KMAXS,COFS,GRDS,IDIR)
      if(kbak==1) call prttime('sptezm') 

      CALL SPTEZMV(IROMB,MAXWV,IDRT,IMAX,JMAX,KMAX,
     &         COFV(1,1,1),COFV(1,1,2),GRDV(1,1,1,1),GRDV(1,1,1,2),IDIR)
      if(kbak==1) call prttime('sptezmv') 

!  copy the grids to working memory

      IF( SFCPRESS_ID == 2 ) THEN   ! for enthalpy version
         GRDS(:,:,2) = 1000.0*GRDS(:,:,2)         ! Now in Pa
      ELSE
         GRDS(:,:,2) = 1000.0*EXP(GRDS(:,:,2))    ! Now in Pa
      ENDIF

      DO NS=1, KMAXS
         CALL GBLEVN11(IMAX,JMAX,GRDS(1,1,NS))
      ENDDO
      DO J=1,JMAX
         DO I=1,IMAX
            IAR12Z(I,J) = GRDS(I,J,1)         ! Orography
            IAR13P(I,J) = GRDS(I,J,2) * 0.01  ! Surface Pressure in hPa
         ENDDO
      ENDDO

      IF(THERMODYN_ID == 3 .AND. IDVC == 3) THEN
         GRDS(:,:,3:2+KMAX) = GRDS(:,:,3:2+KMAX) / HEAD(1)%CPI(1)
      ENDIF
      CALL SIGIO_MODPR(IMJM4,IMJM4,KM4,NVCOORD,IDVC,IDSL,VCOORD,IRET,
     $                 GRDS(1,1,2),GRDS(1,1,3),PM=WRK1,PD=WRK2(1,2))

      DO J=1,JMAX
         JJ = (J-1)*IMAX
         DO I=1,IMAX
            WRK2(I+JJ,1) = GRDS(I,J,2)                     ! in Pa
         ENDDO
      ENDDO
      DO L=1,KMAX
         WRK2(:,L+1) = WRK2(:,L) - WRK2(:,L+1)             ! in Pa
      ENDDO

      IF(THERMODYN_ID == 3 .AND. IDVC == 3) THEN
                                 ! Convert from enthalpy to temperature
         GRDS(:,:,3:2+KMAX) = GRDS(:,:,3:2+KMAX) * HEAD(1)%CPI(1)
         CALL SIGIO_CNVTDV(IMJM4,IMJM4,KM4,IDVC,IDVM,NTRAC,IRET,
     $                     GRDS(1,1,3),GRDS(1,1,3+KMAX),HEAD(1)%CPI,1_4)
                                 ! Convert back to virtual temperature
         GRDS(:,:,3:KMAX+2) = GRDS(:,:,3:KMAX+2) *
     $            (1.+(461.50/287.05-1)*GRDS(:,:,3+KMAX:2+KMAX*2))
      ENDIF

      DO L=1,KMAX
      CALL GBLEVN11(IMAX,JMAX,GRDV(1,1,L,1))
      CALL GBLEVN11(IMAX,JMAX,GRDV(1,1,L,2))
      ENDDO

      DO L=1,KMAX; J=1
!$OMP PARALLEL DO PRIVATE(i)                  
      DO I=1,IMAX*jmax
      IAR14T(I,J,L) = GRDS(I,J,2+L)                     ! Temp (virtual)
      IAR15U(I,J,L) = GRDV(I,J,L,1)                     ! U component
      IAR16V(I,J,L) = GRDV(I,J,L,2)                     ! V component
      IAR17Q(I,J,L) = MAX(0.0,GRDS(I,J,2+KMAX+L)*1.0E6) ! specific humidity
      IARPSL(I,J,L) = WRK1(I,L)*0.01                    ! 3D layer pres(hPa)
      ENDDO
      ENDDO


      DEALLOCATE (COFS, COFV)
      DEALLOCATE (GRDS, GRDV, WRK1, WRK2)

      print *,' RETURNING from GBLENV10'

      RETURN

      END
