C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: RELOCATE_MV_NVORTEX
C   PROGMMR: QINGFU LIU     ORG: NP23       DATE: 2013-03-15
C
C ABSTRACT: RELOCATES HURRICANE VORTEX IN GLOBAL MODEL.
C   THIS PROGRAM CONTAINS THE FOLLOWING STEPS:
C   1) CONVERTS THE GLOBAL SPECTRAL COEFS TO GAUSSIAN GRID
C      AND DEFINES A 40x40 DEG AREAS AROUND THE REPORTED HURRICANES.
C   2) USING GFDL PROCEDURE SEPARATES THE HURRICANE DISTURBANCE FROM
C      THE ENVIRONMENTAL FIELD AND MOVE THE HURRICANE DISTURBANCE TO
C      THE OBSERVATIONAL LOCATION.
C   3) CONVERTS THE GAUSSIAN GRID TO GLOBAL SPECTRAL COEFS.
C
C PROGRAM HISTORY LOG:
C 2000-04-25  QINGFU LIU
C 2000-06-14  DENNIS KEYSER -- ADDED CALLS TO W3TAGB AND W3TAGE
C             AND CALLS TO ERREXIT FOR NON-ZERO STOP CONDITIONS.
C 2005        Mark Iredell -- Generailzed to sigma-P coordinate
C 2007-10-01  S. Moorthi -- include enthalpy and more prognostic variables
C 2010-09-01  Fanglin Yang -- add threading 
C 2012        J. Woollen -- converted to MPI in order to process multiple
C             backgrouds in the same run
C 2012-12-01  J. Woollen -- transitioned to WCOSS 
C 2013-03-15  D. Stokes -- added/modified some informational print or write
C             statements to aid tracing of the output stream.
C
C USAGE:
C   INPUT FILES:
C
C     UNIT 11    THE CURRENT TC VITAL FILE
C     UNIT 20    THE SIGMA FILE AT TIME t-3
C     UNIT 21    THE SIGMA FILE AT (CURRENT) TIME t
C     UNIT 22    THE SIGMA FILE AT TIME t+3
C     UNIT 30    MODEL VORTEX CENTER LOCATION AT TIME t-3,t,t+3
C
C   SUBPROGRAMS CALLED:
C     UNIQUE     - modules     BOUND_QLIU  fft99      sig_p_convt1
C                  SEPAR_QLIU  WNLIT       FDUMP      H12
C                  I1MACH      J4SAVE      XGETUA     WNLSM
C                  WNNLS       XERABT      XERCTL     XERPRT
C                  XERROR      XERRWV      XERSAV     srotm
C                  srotmg      rodist_qliu amatrix_qliu
C     LIBRARY:
C       W3LIB    - W3TAGB      W3TAGE      ERREXIT
C
C
C   EXIT STATES:
C     COND =  0 - SUCCESSFUL RUN
C     COND = 56 - NO TC VITAL DATA (OR TC VITAL IS EMPTY)
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM-SP
C
C$$$
C
C234567890123456789012345678901234567890123456789012345678901234567890
C

      PROGRAM RELOCATE_MV_NVORTEX
      use sigio_module
!     use sigio_r_module

      integer, parameter :: IRX=41, JRX=41, NST=10

      COMMON/SMTH/ CLAT,CLON
      COMMON/CNT/ SLON,SLAT
      COMMON /NHC/ KSTM,IC_N(NST),JC_N(NST)
      COMMON /NHC1/SLON_N(NST),SLAT_N(NST),CLON_N(NST),CLAT_N(NST)

      COMMON /HDAT1/NWRT1,NRED1,NWT1
      COMMON /HDAT3/NWRT2,NRED2
C                                 ! NST is the max storm num
      CHARACTER ST_NAME(NST)*3,TCVT(NST)*95
      COMMON /STNAME/ST_NAME
      COMMON /TCVIT/TCVT
      COMMON /CHEN/KUNIT,ITIM

      REAL,   ALLOCATABLE :: GLAT(:),GLON(:)
      REAL,   ALLOCATABLE :: COLRAD(:),WGT(:),WGTCS(:),RCS2(:)

      REAL,   ALLOCATABLE :: ZG(:,:),PSFC(:,:),PSLB(:,:)

      REAL,   ALLOCATABLE :: HDAT(:,:,:,:),HDATA(:,:,:),PDAT(:,:,:)

      REAL(4),ALLOCATABLE :: WRK1_4(:,:,:), WRK2_4(:,:,:,:)
      REAL,   ALLOCATABLE :: WORK_8(:)
      REAL,   ALLOCATABLE :: WK_S1(:,:),WK_S2(:,:),WK_G(:,:,:),
     1                       WK_G2(:,:,:),WK_G3(:,:,:),WK_G4(:,:,:)
     &,                      WK_G5(:,:,:)
      REAL(4),   ALLOCATABLE :: SLREF(:),VCRD(:,:), tref(:)
!     INTEGER IDVC,IDSL, IDVM, NTRAC, IKMAX
      integer * 4 iret, iret1, irets, imjm4, km4, idvm, ntrac
      integer * 4 idvc, idsl, nvcd
      integer ikmax

      REAL(4) PSREF(1)

      REAL STRPSF(NST)

      REAL(4) FHOUR
!     REAL(4) FHOUR,DUMMY(245)
!     REAL DUMMY8(245)
      
!     CHARACTER*8 LAB(4)
      DIMENSION IDATE(4)

      CHARACTER cfile*7,kfile*7,ciunit*2,ckunit*2
      character(80) parm,outp

!     COMMON /COEF1/LAB
!     COMMON /COEF2/IDATE
!     COMMON /COEF3/FHOUR,DUMMY
!     COMMON /COEF3/nvcd,idvc,idsl,vcrd
!     COMMON /COEF5/NCNT,NCNT2


      type(sigio_head) head
      type(sigio_data):: data1

      include "mpif.h"
 
!--------------------------------------------------------------------------
      call mpi_init(ierr)
      call mpi_comm_rank(MPI_COMM_WORLD,myid,ierr)
      call mpi_comm_size(MPI_COMM_WORLD,nprc,ierr)
      write(parm,'("parm.",i1)')myid; open(5,file=parm)       
      write(outp,'("stdo.",i1)')myid; open(6,file=outp,recl=132)       
      read(5,*) itim,imax,jmax
!--------------------------------------------------------------------------

      CALL W3TAGB('RELOCATE_MV_NVORTEX',2013,0074,0000,'NP23')

! the following are prints rather than write statements to send the info to stdout.
!  writes to unit 6 will be diverted to individual files for each process.
      if(myid.eq.0)then
        print '(/,a,a)','*** A single call to this MPI version of '
     $   ,'relocate_mv_nvortex spawns multiple processes.'
        print '(a,a,/)','*** W3TAGB/E are called for each '
     $   ,'background field to be processed.'
      endif
      print '(a,i0,a,i0,a,i0/a,a,i0,a/)',
     $  'In task ',myid,' of relocate_mv_nvortex for fhr ',itim
     $   ,', writes to unit 6 are being diverted to file stdo.',myid
     $   ,'Select output from w3tagb/e (start/end times and resource '
     $   ,'stats) may be diverted to file stdo.',myid
     $   ,' or stdout, depending on the version of w3tagb/e used.'
! end block of informational print statements

      WRITE(6,*) '===> WELCOME TO RELOCATE_MV_NVORTEX - MPI VERSION ',
     $ '02-22-2013'
      write(6,*)' ' 
      write(6,*)' FORECAST HOUR= ', ITIM
      write(6,*)' IMAX, JMAX= ', IMAX,JMAX
      write(6,*)' ' 
      write(6,'(a,a,/)') 'Informational output from multiple storms '
     $    ,'relocated for one background field might be interspersed'

      IUNIT = 19+ITIM/3
      KUNIT = 50+ITIM

      NRED1 = 0
      NWRT1 = 0
      NRED2 = 0
      NWRT2 = 0

      write(ciunit,'(I2)')iunit
      cfile='fort.'//ciunit

      write(ckunit,'(I2)')KUNIT
      kfile='fort.'//ckunit

!     write(6,*)cfile,kfile

      call sigio_sropen(IUNIT,cfile,iret)
      call sigio_srhead(IUNIT,head,iret)

      call sigio_swopen(KUNIT,kfile,iret)
      call sigio_swhead(KUNIT,head,iret)
      
      call sigio_aldata(head,data1,iret)
      call sigio_srdata(IUNIT,head,data1,iret)
      if (iret.ne.0) write(6,*)'sigio_srdata failed,iret=',iret
!
      idvc  = head%idvc  !idvc=2 for hybrid, idvc=1 for sigma files
      idsl  = head%idsl
      idvm  = head%idvm
      ntrac = head%ntrac

      MWAVE = head%jcap
      KMAX  = head%levs
      latb  = head%latb
      lonb  = head%lonb
      write(6,*)' LONB, LATB, KMAX, MWAVE= ', LONB,LATB,KMAX,MWAVE
      if(imax<=0.or.jmax<=0) then
         imax=lonb;jmax=latb
         write(6,*),'imax,jmax reset to sigma header values=',imax,jmax
      endif

      IKMAX=(NTRAC-2)*KMAX

      IDATE=head%idate
      FHOUR=head%fhour
   
      WRITE(6,210) (IDATE(I),I=1,4),FHOUR
c     1    ,(DUMMY(K),K=1,2*KMAX+1)
210   FORMAT(5X,' INPUT DATE AND FCST HOUR ',4I5,F7.1/(2X,G13.6))

      MAXWV=(MWAVE+1)*(MWAVE+2)/2
      MAXWV2=2*MAXWV
      MAXWV22=MAXWV2+1
   
      JHF=JMAX/2

!     MTV  = KMAX*(3+ntrac) + 2
      MTV  = KMAX*4 + 2
      MTV1 = KMAX*5 + 2
      MTV2 = KMAX*6 + 3
      MTV3 = (2*KMAX+1)*6 + 3     

      ALLOCATE ( GLAT(JMAX),GLON(IMAX) ) 
      ALLOCATE ( COLRAD(JHF), WGT(JHF),WGTCS(JHF),RCS2(JHF) ) 
      ALLOCATE ( ZG(IMAX,JMAX),PSFC(IMAX,JMAX),PSLB(IMAX,JMAX) )
!     ALLOCATE ( WORK_4(MAXWV2,MTV1+IKMAX) )
      ALLOCATE ( WORK_8(MAXWV22) )
      ALLOCATE ( WK_S1(MAXWV2,KMAX),WK_S2(MAXWV2,KMAX) )
      ALLOCATE ( WK_G(IMAX,JMAX,KMAX),WK_G2(IMAX,JMAX,KMAX) )
      ALLOCATE ( WK_G3(IMAX,JMAX,KMAX),WK_G4(IMAX,JMAX,KMAX) )
      ALLOCATE ( WK_G5(IMAX,JMAX,KMAX))
      ALLOCATE ( SLREF(KMAX), TREF(KMAX) )
      if (mod(idvm/10,10) == 3 .and. idvc == 3) then
        ALLOCATE ( WRK1_4(IMAX,JMAX,KMAX),WRK2_4(IMAX,JMAX,KMAX,ntrac) )
      endif


      ALLOCATE ( HDAT(IRX,JRX,MTV2,NST) )
      ALLOCATE ( HDATA(IMAX,JMAX,MTV) )
      ALLOCATE ( PDAT(IRX,JRX,MTV3) )

      CALL GLATS(JHF,COLRAD,WGT,WGTCS,RCS2)

      PI  = ASIN(1.)*2
      RDR = 180./PI
!
      DO LL = 1,JHF
        LLS = JMAX+1 - LL
        GLAT(LL)  = 90. - COLRAD(LL)*RDR
        GLAT(LLS) = -GLAT(LL)
      ENDDO

      DO LL=1,JMAX
        !write(6,*)'GLAT(LL)=',LL,GLAT(LL)
      END DO
!
      DLN = 360.0/FLOAT(IMAX)
      DO LN = 1,IMAX
        GLON(LN) = (LN-1) * DLN
      ENDDO

      nvcd=head%nvcoord
      allocate ( vcrd(KMAX+1,nvcd) )
      vcrd=head%vcoord

      PSREF = 1.E5
      tref  = 270.0

      CALL sigio_modpr(1,1,KMAX,nvcd,idvc,idsl,vcrd,iret,
     &                 ps=PSREF,t=tref,pm=SLREF)

      SLREF = SLREF/1.E5

      do k=1,kmax
        write(6,*)'k,SLREF=',k,SLREF(k)
      end do

      CALL HURR_MESS(IMAX,JMAX,GLON,GLAT,STRPSF)

      DO NW=1,MAXWV2
        WORK_8(NW) = data1%hs(NW)
      END DO
      

      call SPTEZ(0,MWAVE,4,IMAX,JMAX,WORK_8,WK_G(1,1,1),+1)

      CALL SPC2G(IMAX,JMAX,GLON,GLAT,ZG,PSFC,PSLB,
     1           MTV,MTV2,HDAT,HDATA,WK_G(1,1,1),1,idvm)


      DO NW=1,MAXWV2
        WORK_8(NW) = data1%ps(NW)
      END DO

      call SPTEZ(0,MWAVE,4,IMAX,JMAX,WORK_8,WK_G(1,1,1),+1)
      CALL SPC2G(IMAX,JMAX,GLON,GLAT,ZG,PSFC,PSLB,
     1           MTV,MTV2,HDAT,HDATA,WK_G(1,1,1),2,idvm)
!
!     write(6,*)' after SPC2G for PSFC'

      DO K=1,KMAX
        DO NW=1,MAXWV2
          WK_S1(NW,K) = data1%t(NW,k)
          WK_S2(NW,K) = data1%q(NW,K,1)
        END DO
      END DO

      CALL SPTEZM(0,MWAVE,4,IMAX,JMAX,KMAX,WK_S1,WK_G,+1)
      CALL SPTEZM(0,MWAVE,4,IMAX,JMAX,KMAX,WK_S2,WK_G5,+1)

!     write(6,*)' wk_g5=',wk_g5(1,jmax/2,:)*1000

      if (mod(idvm/10,10) == 3 .and. idvc == 3) then
        do nt=2,ntrac
          DO K=1,KMAX
            DO NW=1,MAXWV2
              WK_S1(NW,K) = data1%q(NW,K,nt)
            END DO
          ENDDO
          CALL SPTEZM(0,MWAVE,4,IMAX,JMAX,KMAX,WK_S1,WK_G3,+1)
          wrk2_4(:,:,:,nt) = wk_g3
        ENDDO
        imjm4 = imax*jmax ; km4 = kmax
!     write(6,*)' imjm4=',imjm4,' km4=',km4,' ntrac=',ntrac
!    &,' cpi=',head%cpi
        wrk1_4 = wk_g
        wrk2_4(:,:,:,1) = wk_g5

!     write(6,*)' bef cnvtdv wrk1_4=',wrk1_4(1,90,:)
        call sigio_cnvtdv(imjm4, imjm4, km4, idvc
     &,                   idvm, ntrac, iret, wrk1_4
     &,                   wrk2_4, head%cpi,1)
!     write(6,*)' iret=',iret,' after cnvtdv','wrk2_4=',wrk2_4(1,90,:)
        if (iret /= 0) then
          write(6,*)' return code from cnvtdv = ',iret,' job stopping'
          stop 444
        endif

        wk_g = wrk1_4 * (1.+(461.50/287.05-1)*wrk2_4(:,:,:,1))
      endif
        
      DO K=1,KMAX
        IDX=10
        IF(K.EQ.1)IDX=3
!                                 Virtual Temperature
        CALL SPC2G(IMAX,JMAX,GLON,GLAT,ZG,PSFC,PSLB,
     1             MTV,MTV2,HDAT,HDATA,WK_G(1,1,K),IDX,idvm)
      ENDDO
      IDX = 10
!
      DO K=1,KMAX
        DO NW=1,MAXWV2
          WK_S1(NW,K) = data1%d(NW,K)
          WK_S2(NW,K) = data1%z(NW,K)
        END DO
      ENDDO
      CALL SPTEZM(0,MWAVE,4,IMAX,JMAX,KMAX,WK_S1,WK_G,+1) 
      CALL SPTEZM(0,MWAVE,4,IMAX,JMAX,KMAX,WK_S2,WK_G2,+1) 
      CALL SPTEZMV(0,MWAVE,4,IMAX,JMAX,KMAX,WK_S1,WK_S2,
     &             WK_G3,WK_G4,+1)      

      IDX=10
      DO K=1,KMAX
        CALL SPC2G(IMAX,JMAX,GLON,GLAT,ZG,PSFC,PSLB,
     1             MTV,MTV2,HDAT,HDATA,WK_G(1,1,K),IDX,idvm)
        CALL SPC2G(IMAX,JMAX,GLON,GLAT,ZG,PSFC,PSLB,
     1             MTV,MTV2,HDAT,HDATA,WK_G2(1,1,K),IDX,idvm)
!
!.. CONVERT DI, ZE TO U,V and U,V TO DI ZE again for confirm
!
        CALL SPC2G(IMAX,JMAX,GLON,GLAT,ZG,PSFC,PSLB,
     1             MTV,MTV2,HDAT,HDATA,WK_G3(1,1,K),100,idvm)
        CALL SPC2G(IMAX,JMAX,GLON,GLAT,ZG,PSFC,PSLB,
     1             MTV,MTV2,HDAT,HDATA,WK_G4(1,1,K),101,idvm)
      ENDDO
!                                 Specific Humidity
      DO K=1,KMAX
        CALL SPC2G(IMAX,JMAX,GLON,GLAT,ZG,PSFC,PSLB,
     1             MTV,MTV2,HDAT,HDATA,WK_G5(1,1,K),IDX,idvm)
      ENDDO
!
!
!      DO 270 K=1,KMAX
!      NCNT = NCNT + 1
!      READ(IUNIT) (WORK_4(NW,NCNT),NW=1,MAXWV2)
!      WRITE(10) (WORK_4(NW,NCNT),NW=1,MAXWV2)
!270   CONTINUE
!
!      DO 280 K=1,IKMAX
!      NCNT = NCNT + 1
!      READ(IUNIT) (WORK_4(NW,NCNT),NW=1,MAXWV2)
!      WRITE(10) (WORK_4(NW,NCNT),NW=1,MAXWV2)
!280   CONTINUE

!      write(6,*)'NCNT=',NCNT
!      write(6,*)'NCNT should equal to 170 or'212+IKMAX
!      if(NCNT.ne.(212+IKMAX))write(6,*)'Wrong Data Read In'

      DEALLOCATE ( COLRAD, WGT, WGTCS, RCS2 )
      DEALLOCATE ( ZG, PSFC )
      DEALLOCATE ( WORK_8, WK_S1, WK_S2, WK_G )
      DEALLOCATE ( WK_G2, WK_G3, WK_G4, WK_G5 )

      CALL HURR_REL(MWAVE,IMAX,JMAX,KMAX,IKMAX,MAXWV2,
     1              JHF,MTV,MTV1,MTV2,MTV3,
     2              HDAT,HDATA,PDAT,data1,head,PSLB,SLREF,
     3              nvcd,idvc,idsl,vcrd,idvm,ntrac,wrk1_4,
     4              wrk2_4,STRPSF)

      if(allocated(wrk1_4)) deallocate(wrk1_4)
      if(allocated(wrk2_4)) deallocate(wrk2_4)
!
      DO K=1,KMAX
        data1%d(1,K) = 0.0
        data1%z(1,K) = 0.0
      END DO

      CALL sigio_swdata(KUNIT,head,data1,iret)


      CALL W3TAGE('RELOCATE_MV_NVORTEX')
      call mpi_finalize(ierr)
C
      STOP
      END
C
      SUBROUTINE GLATS(LGGHAF,COLRAD,WGT,WGTCS,RCS2)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GLATS       COMPUTES LOCATION OF GAUSSIAN LATITUDES.
C   PRGMMR: JOSEPH SELA      ORG: W/NMC23    DATE: 88-04-05
C
C ABSTRACT: COMPUTES THE LOCATION OF THE GAUSSIAN LATITUDES FOR THE
C   INPUT LGGHAF.  THE LATITUDES ARE DETERMINED BY FINDING
C   THE ZEROS OF THE LEGENDRE POLYNOMIALS.
C
C PROGRAM HISTORY LOG:
C   88-04-05  JOSEPH SELA
C
C USAGE:    CALL GLATS (LGGHAF, COLRAD, WGT, WGTCS, RCS2)
C   INPUT ARGUMENT LIST:
C     LGGHAF   - NUMBER OF GAUSSIAN LATITUDES IN A HEMISPHERE.
C
C   OUTPUT ARGUMENT LIST:
C     COLRAD   - ARRAY OF COLATITUDE OF GAUSSIAN LATITUDES
C                IN NORTHERN HEMISPHERE.
C     WGT      - ARRAY OF WEIGHTS AT EACH GAUSSIAN LATITUDE
C                REQUIRED FOR GAUSSIAN QUADRATURE.
C     WGTCS    - ARRAY OF GAUSSIAN WEIGHT/SIN OF COLATITUDE SQUARED.
C     RCS2     - ARRAY OF RECIPROCAL  OF  SIN OF COLATITUDE SQUARED.
C
C   OUTPUT FILES:
C     OUTPUT   - write(6,*)OUT FILE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 200.
C   MACHINE:  CYBER 205.
C
C$$$
      implicit none
      REAL(8) EPS,SI,RL2,PI,DRADZ,RAD,DRAD,P1,P2,PHI,X,W,SCALE,SN,RC
      REAL    COLRAD(LGGHAF),WGT(LGGHAF),WGTCS(LGGHAF),RCS2(LGGHAF)
      INTEGER L2,LGGHAF,K1,K,ITER
      EPS=1.E-12
C     write(6,*) 101
C101  FORMAT ('0 I   COLAT   COLRAD     WGT', 12X, 'WGTCS',
CCCC 1 10X, 'ITER  RES')
      SI = 1.0
      L2=2*LGGHAF
      RL2=L2
      SCALE = 2.0/(RL2*RL2)
      K1=L2-1
      PI = ATAN(SI)*4.E+00
      DRADZ = PI / 360./10.
      RAD = 0.0
      DO 1000 K=1,LGGHAF
      ITER=0
      DRAD=DRADZ
1     CALL POLY(L2,RAD,P2)
2     P1 =P2
      ITER=ITER+1
      RAD=RAD+DRAD
      CALL POLY(L2,RAD,P2)
      IF(SIGN(SI,P1).EQ.SIGN(SI,P2)) GO TO 2
      IF(DRAD.LT.EPS)GO TO 3
      RAD=RAD-DRAD
      DRAD = DRAD * 0.25
      GO TO 1
3     CONTINUE
      COLRAD(K)=RAD
      PHI = RAD * 180 / PI
      CALL POLY(K1,RAD,P1)
      X = COS(RAD)
      W = SCALE * (1.0 - X*X)/ (P1*P1)
      WGT(K) = W
      SN = SIN(RAD)
      W=W/(SN*SN)
      WGTCS(K) = W
      RC=1./(SN*SN)
      RCS2(K) = RC
      CALL POLY(L2,RAD,P1)
C     write(6,*) 102,K,PHI,COLRAD(K),WGT(K),WGTCS(K),ITER,P1
C102  FORMAT(1H ,I2,2X,F6.2,2X,F10.7,2X,E13.7,2X,E13.7,2X,I4,2X,D13.7)
1000  CONTINUE
      RETURN
      END
      SUBROUTINE POLY(N,RAD,P)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    POLY        EVALUATES LEGENDRE POLYNOMIAL.
C   PRGMMR: JOSEPH SELA      ORG: W/NMC23    DATE: 88-04-01
C
C ABSTRACT: EVALUATES THE UNNORMALIZED LEGENDRE POLYNOMIAL
C   OF SPECIFIED DEGREE AT A GIVEN COLATITUDE USING A STANDARD
C   RECURSION FORMULA.  REAL ARITHMETIC IS USED.
C
C PROGRAM HISTORY LOG:
C   88-04-01  JOSEPH SELA
C
C USAGE:    CALL POLY (N, RAD, P)
C   INPUT ARGUMENT LIST:
C     N        - DEGREE OF LEGENDRE POLYNOMIAL.
C     RAD      - REAL COLATITUDE IN RADIANS.
C
C   OUTPUT ARGUMENT LIST:
C     P        - REAL VALUE OF LEGENDRE POLYNOMIAL.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 200.
C   MACHINE:  CYBER 205.
C
C$$$
      implicit none              
      REAL(8) X,RAD,Y1,Y2,Y3,G,P
      INTEGER I,N
      X = COS(RAD)
      Y1 = 1.0
      Y2=X
      DO 1 I=2,N
      G=X*Y2
      Y3=G-Y1+G-(G-Y1)/FLOAT(I)
      Y1=Y2
      Y2=Y3
1     CONTINUE
      P=Y3
      RETURN
      END

      subroutine maxmin(a,len,k,k1,k2,ch)
      dimension a(len,k)
      character ch*(*)
c
      do 100 j=k1,k2
      aamax = a(1,j)
      aamin = a(1,j)
      do 10 m=1,len
      aamax = max( aamax, a(m,j) )
      aamin = min( aamin, a(m,j) )
10    continue
C      write(6,*)   *,ch,' has max=',aamax,' min=',aamin
100   continue
      return
      end
C
      SUBROUTINE PMSL2PS(IMAX,JMAX,GLON,GLAT,
     1               IUT,MTV2,DUMM,HDAT,ZN,TN)

      real, parameter :: G=9.8, R=287.05, GAMMA=6.7*0.001
      PARAMETER (IRX=41,JRX=41,NST=10)
      REAL GLON(IMAX),GLAT(JMAX),DUMM(IMAX,JMAX)
      REAL TN(IRX,JRX)
      REAL ZN(IRX,JRX),PSN(IRX,JRX),PSFCN(IRX,JRX)


      COMMON /HDAT1/NWRT1,NRED1,NWT1
      REAL HDAT(IRX,JRX,MTV2,NST)

      CALL CUT_DM(IMAX,JMAX,GLON,GLAT,PSN,DUMM,1)

C.. Using interpolated MSLP, Make surface pressure

!!$OMP PARALLEL DO DEFAULT(SHARED)
!!$OMP+ PRIVATE(I,J,A,B,C,DD,D1)
      DO I=1,IRX 
      DO J=1,JRX
      PSN(I,J) = LOG(PSN(I,J))
      A = (GAMMA * ZN(I,J)) / TN(I,J)
      B = LOG(1+A)
      C = (G*B)/(R*GAMMA)
      DD = PSN(I,J) - C
      D1 = EXP(DD)/1000.
      PSFCN(I,J) = LOG(D1)
      PSN(I,J) = EXP(PSN(I,J))
      ENDDO
      ENDDO

C      write(6,*)'MSLP at Hurricane center ',psn(31,21)/100.
c      call maxmin(psn,41*41,1,1,1,'sea-level pressure in reg')
c      call maxmin(psfcn,41*41,1,1,1,'sfc pressure in reg (hPa)')
c      call maxmin(dum1,41*41,1,1,1,'sfc pressure in reg(ln(cb))')
c      call maxmin(zn,41*41,1,1,1,'terraine in reg')
c      call maxmin(tn,41*41,1,1,1,'temperature at k=1 in reg')
c      write(6,*)'============================'
C
c      write(6,*)'write sfc press'
c      WRITE(IUT) ((PSFCN(I,J),I=1,IRX),J=JRX,1,-1)
      CALL WRIT1(IUT,NWT1,NWRT1,MTV2,PSFCN,HDAT)
c      write(6,*)'write MSLP'
c      WRITE(IUT) ((PSN(I,J),I=1,IRX),J=JRX,1,-1)
      CALL WRIT1(IUT,NWT1,NWRT1,MTV2,PSN,HDAT)
c      write(6,*)'write t1'
c      WRITE(IUT) ((TN(I,J),I=1,IRX),J=JRX,1,-1)
      CALL WRIT1(IUT,NWT1,NWRT1,MTV2,TN,HDAT)
      RETURN
      END
C
      SUBROUTINE SPC2G(IMAX,JMAX,GLON,GLAT,ZG,PSFC,PSLB,
     1               MTV,MTV2,HDAT,HDATA,DUM,IDX,idvm)
CCCCC MEMBER HALF
c      SAVE
C
      PARAMETER ( IRX= 41,JRX= 41, NST=10 )
      real, parameter :: G=9.8, R=287.05, GAMMA=6.7*0.001

      integer * 4 idvm
      COMMON/SMTH/ CLAT,CLON
      REAL GLON(IMAX),GLAT(JMAX)
      COMMON /NHC/ KSTM,IC_N(NST),JC_N(NST)
      COMMON /NHC1/ SLON_N(NST),SLAT_N(NST),CLON_N(NST),CLAT_N(NST)
      COMMON/CNT/ SLON,SLAT
      COMMON /CHEN/KUNIT,ITIM
!
      REAL HDAT(IRX,JRX,MTV2,NST),HDATA(IMAX,JMAX,MTV)
      REAL DUM(IMAX,JMAX)

      COMMON /HDAT1/NWRT1,NRED1,NWT1
      COMMON /HDAT3/NWRT2,NRED2

       REAL ZG(IMAX,JMAX),PSFC(IMAX,JMAX),PSLB(IMAX,JMAX)
       REAL T1(IMAX,JMAX)
       REAL PS(IMAX,JMAX),DUMM(IMAX,JMAX),PSL(IMAX,JMAX)

      REAL ZN(IRX,JRX),TN(IRX,JRX),NEW(IRX,JRX)
!
!.. Global coefficent to Gaussian grid
!
!      call SPTEZ(0,MWAVE,4,IMAX,JMAX,DO,DUM,+1)

      IF(IDX.NE.100.AND.IDX.NE.101) THEN
!       WRITE(66) DUM

        NWRT2 = NWRT2+1
        DO J=1,JMAX
          DO I=1,IMAX
            HDATA(I,J,NWRT2) = DUM(I,J)
          END DO
        END DO
!       write(6,*)'WRIT2 COUNT = ',NWRT2,' hdata=',hdata(1,jmax/2,nwrt2)
!    &,' idx=',idx

!       CALL WRIT2(DUM)
!       write(6,*)'=====IDX ',IDX
!       call maxmin(DUM,IMAX*JMAX,1,1,1,'DUM in gbl')

! test qliu
!      READ(66) DUM
!      CALL G2SPC(DUM)
! end qliu

!       IF(IDX.EQ.1) write(6,*)'TERRAIN AT 289, 80 ',DUM(289,80)
      ENDIF
!
      IF(IDX.EQ.1) THEN
        DO I=1,IMAX
          DO J=1,JMAX
            ZG(I,J) = DUM(I,J)
          ENDDO
        ENDDO
      ELSEIF(IDX.EQ.2) THEN
        DO I=1,IMAX
          DO J=1,JMAX
            PSFC(I,J) = DUM(I,J)
          ENDDO
        ENDDO
      ELSEIF(IDX.EQ.3) THEN
        DO I=1,IMAX
          DO J=1,JMAX
            T1(I,J) = DUM(I,J)
          ENDDO
        ENDDO
      ENDIF
!
       IF(IDX.EQ.2)call maxmin(PSFC,IMAX*JMAX,1,1,1,'psfc in gbl')
       IF(IDX.EQ.3)call maxmin(T1,IMAX*JMAX,1,1,1,'T1 in gbl')

      IF(IDX.EQ.3)THEN
!!$OMP    PARALLEL DO DEFAULT(SHARED)
!!$OMP+   PRIVATE(I,J)
         DO I=1,IMAX
         DO J=1,JMAX
         if (mod(idvm,10)==2) PS(i,j)=PSFC(I,J)*1000.
         if (mod(idvm,10)/=2) PS(i,j)=EXP(PSFC(I,J))*1000.
         PSFC(I,J) = LOG(PS(i,j))
         ENDDO
         ENDDO
!!$OMP END PARALLEL DO
         call maxmin(ps,IMAX*JMAX,1,1,1,'sfc press in gbl')
!
!.. Calculate MSLP from SFC Pressure
!
!!$OMP    PARALLEL DO DEFAULT(SHARED)
!!$OMP+   PRIVATE(I,J,A,B,C)
         DO I=1,IMAX
         DO J=1,JMAX
         A        = (GAMMA * ZG(I,J)) / T1(I,J)
         B        = LOG(1+A)
         C        = (G*B)/(R*GAMMA)
         PSL(I,J) = PSFC(I,J) + C
         DUMM(I,J) = EXP(PSL(I,J))
         ENDDO
         ENDDO
!!$OMP END PARALLEL DO

         call maxmin(DUMM,IMAX*JMAX,1,1,1,'MSLP in gbl')
!        write(70)DUMM
         PSLB = DUMM
      ENDIF 
     
      DO K=1,KSTM

        IUT=K

!       NWRT1 = 0
        NWT1=0

        SLON = SLON_N(K)
        SLAT = SLAT_N(K) 
        CLON = CLON_N(K)
        CLAT = CLAT_N(K)
        IC   = IC_N(K)
        JC   = JC_N(K) 
!
        IF (IDX.EQ.1) THEN
          CALL CUT_DM(IMAX,JMAX,GLON,GLAT,ZN,DUM,2)
!         WRITE(IUT) ((ZN(I,J),I=1,IRX),J=JRX,1,-1)
          CALL WRIT1(IUT,NWT1,NWRT1,MTV2,ZN,HDAT)
          write(6,222)K,ITIM,SLON,SLAT,CLON,CLAT,IC,JC
 222  FORMAT(/' STORM ',I2,', FORECAST HOUR ',I4/,
     1       ' SLON,SLAT,CLON,CLAT,IC,JC=',4F10.3,2x,2I5/)
!         write(6,*)'write zn'
        ENDIF
        IF (IDX.EQ.3) THEN
          DO I=1,41
            DO J=1,41
              ZN(I,J)=HDAT(I,J,1,IUT)
            END DO
          END DO
        END IF
        IF (IDX.EQ.3) CALL CUT_DM(IMAX,JMAX,GLON,GLAT,TN,DUM,3)
        IF(IDX.NE.1.AND.IDX.NE.3.AND.IDX.LT.100) THEN
          CALL CUT_DM(IMAX,JMAX,GLON,GLAT,NEW,DUM,3)
        ELSEIF(IDX.GE.100) THEN
          CALL CUT_DM(IMAX,JMAX,GLON,GLAT,NEW,DUM,IDX)
        ENDIF

!        IF(IDX.LE.3)write(6,*)'===at sub SPC2G just bfr call PMSL2PS ==='
!        IF(IDX.EQ.1)call maxmin(zg,IMAX*JMAX,1,1,1,'terrain in gbl')
!        IF(IDX.EQ.1)call maxmin(zn,41*41,1,1,1,'terrain in reg')
!        IF(IDX.EQ.2)call maxmin(psfc,IMAX*JMAX,1,1,1,'sfc pres in gbl')
!        IF(IDX.EQ.2)call maxmin(new,41*41,1,1,1,'sfc pres in reg')
!        IF(IDX.EQ.3)call maxmin(t1,IMAX*JMAX,1,1,1,'temp at k=1 in gbl')
!        IF(IDX.EQ.3)call maxmin(tn,41*41,1,1,1,'temp at k=1 in reg')
!        IF(IDX.LE.3)write(6,*)'======================================='
!
        IF(IDX.EQ.3) CALL PMSL2PS(IMAX,JMAX,GLON,GLAT,
     1                        IUT,MTV2,DUMM,HDAT,ZN,TN)
131   FORMAT(1x,'TERRAIN')
121   FORMAT(1x,20F5.0)

!
        IF(IDX.GT.3) THEN
!         WRITE(IUT)((NEW(I,J),I=1,IRX),J=JRX,1,-1)
          CALL WRIT1(IUT,NWT1,NWRT1,MTV2,NEW,HDAT)
        ENDIF

        IF(K.LT.KSTM)NWRT1=NWRT1-NWT1

      ENDDO
!
      RETURN
      END
C

      SUBROUTINE DECVAR(ISTART,IEND,IVALUE,IERDEC,FMT,BUFF)
C
      PARAMETER (NCHLIN=130)
C
      CHARACTER FMT*(*),BUFF*(*),OUTLIN*1
C
c      SAVE
C
      DIMENSION OUTLIN(NCHLIN)
C
c && 2 comments
CC    WRITE(6,1) FMT,BUFF
CC  1 FORMAT(/'...FMT=',A10,/,' ...BUFF=',A100)
C
      READ(BUFF(ISTART:IEND),FMT,ERR=10)  IVALUE
      IERDEC=0
      RETURN
C
   10 CONTINUE
C
      OUTLIN=' '
C
      IERDEC=10
      OUTLIN(ISTART:IEND)='*'
C
      WRITE(6,31) (OUTLIN(ICH1),ICH1=1,NCHLIN)
      WRITE(6,32) BUFF
   31 FORMAT(/'******ERROR DECODING, BUFF=',/,130A1)
   32 FORMAT(A130)
C
      RETURN
      END


      SUBROUTINE HURR_MESS(IMAX,JMAX,GLON,GLAT,STRPSF)
    
      PARAMETER (IRX=41,JRX=41,NST=10)
      PARAMETER (MAXVIT=15)
 
      COMMON/SMTH/ CLAT,CLON
      REAL GLAT(JMAX),GLON(IMAX)
      REAL STRPSF(NST)
      COMMON /NHC/ KSTM,IC_N(NST),JC_N(NST)
      COMMON /NHC1/ SLON_N(NST),SLAT_N(NST),CLON_N(NST),CLAT_N(NST)
      DIMENSION STMDIR(NST),STMSPD(NST)
c      CHARACTER ST_NAME(NST)*3,TCVT(NST)*95
      CHARACTER ST_NAME(10)*3,STMNAME(10)*3,TCVT(10)*95
      COMMON /STNAME/ST_NAME
      COMMON /TCVIT/TCVT
      COMMON /CHEN/KUNIT,ITIM

      CHARACTER BUFIN(95)*1,BUFY2K(95)*1,STMNAM(NST)*12,STMNMZ*9
      CHARACTER FMTVIT(MAXVIT)*6,BUFINZ*100,LATNS*1,LONEW*1

      DIMENSION IVTVAR(MAXVIT),VITVAR(MAXVIT),VITFAC(MAXVIT),
     1          ISTVAR(MAXVIT),IENVAR(MAXVIT)

      DIMENSION ISTMCX1(3,NST),ISTMCY1(3,NST),STMCX(NST),STMCY(NST)
 
      DATA ISTVAR/20,29,34,39,45,49,53,58,63,68,71,75,80,85,90/
      DATA IENVAR/27,32,36,42,47,51,56,61,66,69,73,78,83,88,93/ 
      DATA VITFAC/2*1.0,2*0.1,1.0,0.1,9*1.0/
      DATA FMTVIT/'(I8.8)','(I4.4)','(I3.3)','(I4.4)',2*'(I3.3)',
     1            3*'(I4.4)','(I2.2)','(I3.3)',4*'(I4.4)'/
   
      EQUIVALENCE (BUFIN(37),LATNS),(BUFIN(43),LONEW),
     1            (BUFIN(10),STMNMZ),(BUFIN(1),BUFINZ)
C      EQUIVALENCE (IVTVAR(1),IDATEZ),(IVTVAR(2),IUTCZ)
C
      EQUIVALENCE (VITVAR( 3),STMLTZ),(VITVAR( 4),STMLNZ),
     1            (VITVAR( 5),STMDRZ),(VITVAR( 6),STMSPZ),
     1            (VITVAR( 9),RMPSFZ)
C
       ONEDEG=360./(2.*3.1415926*6.37E6)
C
      DO I=1,10
        SLON_N(I)=0.
        SLAT_N(I)=0.
        CLON_N(I)=0.
        CLAT_N(I)=0.
        IC_N(I)=0
        JC_N(I)=0
      END DO

  90  REWIND 11
      KREC=0
      KSTORM=0
      NERROR=0
C
C  Get the hurricane center from the hurricane message made by NHC
C
C     READ A RECORD INTO BUFFER
C
  100 CONTINUE
      READ(11,101,ERR=990,END=200) (BUFIN(NCH),NCH=1,95)
  101 FORMAT(95A1)

      if(BUFIN(35).eq.'N' .or. BUFIN(35).eq.'S')  then

         write(6,*) ' '
         write(6,*) '==> RECORD from tcvitals file contains a',
     $    ' 2-digit year "'
         write(6,*) ' '

         BUFY2K(1:19) = BUFIN(1:19)
         IF(BUFIN(20)//BUFIN(21).GT.'20')  THEN
            BUFY2K(20) = '1'
            BUFY2K(21) = '9'
         ELSE
            BUFY2K(20) = '2'
            BUFY2K(21) = '0'
         ENDIF
         BUFY2K(22:95) = BUFIN(20:93)
         BUFIN = BUFY2K

         write(6,*) ' '
         write(6,*) '==> 2-digit year converted to 4-digit year "'
         write(6,*) ' '

      else  if(BUFIN(37).eq.'N' .or. BUFIN(37).eq.'S')  then

         write(6,*) ' '
         write(6,*) '==> RECORD from tcvitals file -- contains a',
     $    ' 4-digit year "'
         write(6,*) ' '

      else

         write(6,*) ' '
         write(6,*) '***** Cannot determine if this record contains ',
     $    'a 2-digit year or a 4-digit year - skip it and try reading ',
     $    'the next record'
         write(6,*) ' '
         go to 100

      end if

C
C     DECODE DATE AND TIME
C
      DO 110 IV=1,2
      CALL DECVAR(ISTVAR(IV),IENVAR(IV),IVTVAR(IV),IERDEC,FMTVIT(IV),
     1            BUFINZ)
 
  110 CONTINUE

      DO 140 IV=3,MAXVIT
      CALL DECVAR(ISTVAR(IV),IENVAR(IV),IVTVAR(IV),IERDEC,FMTVIT(IV),
     1            BUFINZ)
      VITVAR(IV)=REAL(IVTVAR(IV))*VITFAC(IV)
  140 CONTINUE

C          *****************************************************
C          *****************************************************
C          ****            IMPORTANT NOTES:                 ****
C          ****                                             ****
C          ****    ALL STORM LONGITUDES CONVERTED TO        ****
C          ****    0-360 DEGREES, POSITIVE EASTWARD  !!!    ****
C          ****                                             ****
C          ****    ALL STORM SPEEDS ARE IN M/SEC            ****
C          ****                                             ****
C          ****    ALL DISTANCE DATA ARE IN KM              ****
C          ****                                             ****
C          ****    ALL PRESSURE DATA ARE IN HPA (MB)        ****
C          *****************************************************
C          *****************************************************
C
C     SIGN OF LATITUDE AND CONVERT LONGITUDE
C
      IF(LATNS .EQ. 'S')  THEN
      STMLTZ=-STMLTZ
      ELSE IF(LATNS .NE. 'N')  THEN
      WRITE(6,153) STMLTZ,STMLNZ,LATNS
  153 FORMAT('******ERROR DECODING LATNS, ERROR RECOVERY NEEDED.',
     1       '  STMLTZ,STMLNZ,LATNS=',2F12.2,2X,A1)
      GO TO 100
      ENDIF
C
      IF(LONEW .EQ. 'W')  THEN
      STMLNZ=360.-STMLNZ
      ELSE IF(LONEW .NE. 'E')  THEN
      WRITE(6,157) STMLTZ,STMLNZ,LATNS
  157 FORMAT('******ERROR DECODING LONEW, ERROR RECOVERY NEEDED.',
     1       '  STMLTZ,STMLNZ,LATNS=',2F12.2,2X,A1)
      ENDIF

      IF(STMLNZ.gt.345..or.STMLNZ.lt.15.)go to 100

      KREC=KREC+1

      DO I=1,3
        ST_NAME(KREC)(I:I)=BUFIN(I+5)
      END DO
      DO I=1,95
        TCVT(KREC)(I:I)=BUFIN(I)
      END DO
C
      IF(KSTORM .LT. 10)  THEN
      KSTORM=KSTORM+1
      CLAT_N(KSTORM)=STMLTZ
      CLON_N(KSTORM)=STMLNZ
      STMDIR(KSTORM)=STMDRZ
      STMSPD(KSTORM)=STMSPZ
      STMNAM(KSTORM)=STMNMZ
      STRPSF(KSTORM)=RMPSFZ
      GO TO 100
C
      ELSE

  300 WRITE(6,301) KSTORM
  301 FORMAT(/'******KSTORM EXCEEDS AVAILABLE SPACE, KSTORM=',I5
     1       ,/,' Results may have serious problem')
      GO TO 200

      ENDIF

  200 IF(KSTORM .GT. 0)  THEN
      WRITE(6,201)KSTORM,KREC
  201 FORMAT(/'...FOUND STORM IN VITALS FILE.',/,4X,I5,
     2       ' TOTAL NUMBER OF RECORDS READ=',I7)
      ELSE
      WRITE(6,202)
  202 FORMAT(/'NO STORM FOUND IN VITALS FILE.')
      CALL W3TAGE('RELOCATE_MV_NVORTEX')
      CALL ERREXIT(56)
      END IF
C
c  Correct to the storm center position

      PI=ATAN(1.0)*4.E+00
      PI180 = PI/180.
      DT=(float(ITIM)-6.)*3600.                     !  Second
      ONEDEG=360./(2.*PI*6.37E6)                    !  Degree/Meter
      FACT=DT*ONEDEG

      KSTM=KSTORM

c      WRITE(12, 233) KSTM
c 233  FORMAT(2x,I5)

      DO I=1,KSTM

      write(6,430)STMNAM(I),CLAT_N(I),CLON_N(I),STMDIR(I),STMSPD(I)
  430 FORMAT(/' STORM NAME: ',A12,/, ' READIN STORM CENTER=',2F12.4,
     1       /,' STORM DIR and SPEED: ',2F12.4)

       write(6,*)'RAD OUTMOST CLOSED ISOBAR= ',STRPSF(I),' km'

       STRPSF(I)=STRPSF(I)*1000.*ONEDEG

       write(6,*)'RAD OUTMOST CLOSED ISOBAR= ',STRPSF(I),' degree'

cnew        USTM=STMSPD(I)*SIN(PI180*STMDIR(I))
cnew        VSTM=STMSPD(I)*COS(PI180*STMDIR(I))
cnew        CLON_N(I)=CLON_N(I)+USTM*FACT/COS(PI180*CLAT_N(I))
cnew        CLAT_N(I)=CLAT_N(I)+VSTM*FACT

cnew        write(6,*) 'CORRECTED STORM CENTER AT TIME HOUR ',ITIM,' =',
cnew     1           CLON_N(I),CLAT_N(I)

      END DO

      INDX1=ITIM/3

      K1STM=0
      DO I=1,10
        STMCX(I)=0.
        STMCY(I)=0.
        STMNAME(I)='NUL'
        READ(30,442,end=436)
     &     (ISTMCY1(J,I),ISTMCX1(J,I),J=1,3),STMNAME(I)
        STMCX(I)=360.-ISTMCX1(INDX1,I)*0.1
        STMCY(I)=ISTMCY1(INDX1,I)*0.1
        K1STM=K1STM+1
        write(6,*)' CT STORM Model CENTER at ',ITIM,'h = ',
     &          STMNAME(I),STMCX(I),STMCY(I)
      END DO
 442  FORMAT(22x,6i4,25x,A3)
 436  CONTINUE

      REWIND 30

      DO I=1,KSTM
        DO K=1,K1STM
          IF(STMNAME(K).EQ.ST_NAME(I))THEN
            IFWRT=0
            DO J=1,3
        IF(ISTMCY1(J,K).EQ.0.and.ISTMCX1(J,K).EQ.0)THEN
              IFWRT=1
            END IF
            END DO
            IF(IFWRT.EQ.0)THEN
              XDIST6H=CLON_N(I)-(360.-ISTMCX1(2,K)*0.1)
              YDIST6H=CLAT_N(I)-ISTMCY1(2,K)*0.1
              CLON_N(I)=STMCX(K)+XDIST6H
              CLAT_N(I)=STMCY(K)+YDIST6H
            ELSE
              USTM=STMSPD(I)*SIN(PI180*STMDIR(I))
              VSTM=STMSPD(I)*COS(PI180*STMDIR(I))
              CLON_N(I)=CLON_N(I)+USTM*FACT/COS(PI180*CLAT_N(I))
              CLAT_N(I)=CLAT_N(I)+VSTM*FACT
            END IF 
            write(6,*) ' CT STORM OBS. CENTER at ',ITIM,'h = ',
     &               STMNAME(K),CLON_N(I),CLAT_N(I)
          END IF
        END DO
      END DO


      DO 900 I=1,KSTM

      CLON=CLON_N(I)
      CLAT=CLAT_N(I)
 
      AMN = 500.
      DO 10 ILA = 1,JMAX
      DMN = ABS (GLAT(ILA) - CLAT)
      IF (DMN.LE.AMN) THEN
      AMN = DMN
      JC  = ILA
      ENDIF
10    CONTINUE
C
      BMN = 500.
      DO 20 ILO = 1,IMAX
      OMN = ABS (GLON(ILO) - CLON)
      IF (OMN.LE.BMN) THEN
      BMN = OMN
      IC  = ILO
      ENDIF
20    CONTINUE

      IC_N(I)=IC
      JC_N(I)=JC
C    
      write(6,*)'  '
c      write(6,*)'==========AT SUB HURR_MESS============='
c      write(6,*)'... 1st guess ... center of hurricane'
c      write(6,*)'===IC,JC=== ',IC,JC,GLON(IC),GLAT(JC)
c      write(6,*)'==DIST OF CLON AND IC===',BMN
c      write(6,*)'==DIST OF CLAT AND JC===',AMN

      SLON_N(I) = IFIX(GLON(IC)+0.5 - IRX/2)
      SLAT_N(I) = IFIX(GLAT(JC)+0.5 - JRX/2)
      write(6,*)' '
c      write(6,*)'=========================================='
c      write(6,*)'SLAT, SLON = ', SLAT_N(I),SLON_N(I)
c      WRITE(12,123)SLON_N(I),SLAT_N(I),CLON_N(I),CLAT_N(I)
c123   FORMAT(1x,4F10.3)
      write(6,*)'=========================================='

  900 CONTINUE

      RETURN

  990 WRITE(6,991) BUFIN
  991 FORMAT('******ERROR READING STORM RECORD.  BUFIN IS:',/,
     1       ' ******',A95,'******')
      call mpi_finalize(iret) !!!GO TO 100
      RETURN

      END
C

      SUBROUTINE CUT_DM(IMAX,JMAX,GLON,GLAT,NEW,OLD,IV)
C
      PARAMETER (IRX=41,JRX=41)

      COMMON/SMTH/ CLAT,CLON
      REAL GLAT(JMAX),GLON(IMAX),OLD(IMAX,JMAX)
      COMMON/CNT/ SLON,SLAT
      REAL NEW(IRX,JRX)
C
      X=360./FLOAT(IMAX)

!!$OMP PARALLEL DO DEFAULT(PRIVATE)
!!$OMP+ SHARED(IMAX,JMAX,GLON,GLAT,NEW,OLD,IV)
!!$OMP+ SHARED(CLAT,CLON,SLON,SLAT,X)                           
      DO 10 J=1,JRX
      BLA = 90. - SLAT - (J-1)
      DO 10 I=1,IRX
      BLO = SLON + (I-1)
      IF(BLO.GT.360.)BLO=BLO-360.
C
      DO 20 IG=IMAX,1,-1
      DON = BLO - GLON(IG)
      IF (DON.GE.0) THEN
      DX = DON
      IX = IG
      GO TO 1
      ENDIF
20    CONTINUE
C
1     DO 30 JG=JMAX,1,-1
      GLA = 90 - GLAT(JG)
      DAT = BLA - GLA
      IF (DAT.GE.0) THEN
      DY = DAT
      IY = JG
      Y = GLAT(JG)-GLAT(JG+1)
      GO TO 2
      ENDIF
30    CONTINUE
C
2     IF (IV.EQ.2) THEN
        DD1 = SQRT(DX**2.+DY**2.)
        DD2 = SQRT(DX**2.+(Y-DY)**2.)
        DD3 = SQRT((X-DX)**2.+DY**2.)
        DD4 = SQRT((X-DX)**2.+(Y-DY)**2.)
        IF(DD1.LE.0.2) THEN
          NEW(I,J) = OLD(IX,IY)
          GO TO 10
        ENDIF
        IF(DD2.LE.0.2) THEN
          NEW(I,J) = OLD(IX,IY+1)
          GO TO 10
        ENDIF
        IF(DD3.LE.0.2) THEN
          NEW(I,J) = OLD(IX+1,IY)
          GO TO 10
        ENDIF
        IF(DD4.LE.0.2) THEN
          NEW(I,J) = OLD(IX+1,IY+1)
          GO TO 10
        ENDIF
      ENDIF 
C
      X1 = ( DY*OLD(IX  ,IY+1) + (Y-DY)*OLD(IX  ,IY) ) / Y
      X2 = ( DY*OLD(IX+1,IY+1) + (Y-DY)*OLD(IX+1,IY) ) / Y
      Y1 = ( DX*OLD(IX+1,IY  ) + (X-DX)*OLD(IX,IY  ) ) / X
      Y2 = ( DX*OLD(IX+1,IY+1) + (X-DX)*OLD(IX,IY+1) ) / X
      XX = (DX*X2 + (X-DX)*X1)/X 
      YY = (DY*Y2 + (Y-DY)*Y1)/Y
      NEW(I,J) = (XX+YY)/2.
c      xxxxx=0.25*(OLD(IX,IY)+OLD(IX+1,IY)+
c     &            OLD(IX,IY+1)+OLD(IX+1,IY+1))
C
c      IF(IV.GE.100) THEN
C
c      IF(I.LE.30.AND.J.EQ.20)THEN
c      write(6,*)'OLD 1,2,3,4 ',
c     1     OLD(IX,IY),OLD(IX+1,IY),OLD(IX,IY+1),OLD(IX+1,IY+1)
c      write(6,*)'X,Y,DX,DY ',X,Y,DX,DY
c      write(6,*)'X1,X2,Y1,Y2 ',x1,x2,y1,y2
c      write(6,*)'XX, YY  ',XX,YY
c      write(6,*)'NEW  ',NEW(I,J)
c      write(6,*)'LAT, LON at SM Domain  ',SLAT+(J-1),SLON+(I-1)
c      write(6,*)'LAT, LON at Gauss grid ',GLAT(IY),GLON(IX)
c      write(6,*)'IX,IY  ',IX,IY
c      write(6,*)'BLA, BLO, GLA, GLO ',BLA, BLO, GLA, GLON(IX)
c      ENDIF
c      ENDIF
C
10    CONTINUE
!!$OMP END PARALLEL DO
C
      RETURN
      END

 
      SUBROUTINE WRIT1(IUT,NWT1,NWRT1,MTV2,DIN,HDAT)
      PARAMETER (IRX=41,JRX=41,NST=10)
      REAL       DIN(IRX,JRX),HDAT(IRX,JRX,MTV2,NST) 
      NWRT1 = NWRT1 + 1
      NWT1  = NWT1  + 1
!      write(6,*)'WRIT1 COUNT = ',NWRT1,NWT1,IUT
      DO J=1,JRX
        DO I=1,IRX
          HDAT(I,J,NWRT1,IUT) = DIN(I,J)
        END DO
        END DO 
!     write(6,*)' inwrit1 hdat=',hdat(41,41,nwrt1,iut),' nwrt1=',nwrt1
!    &,' iut=',iut
      return
      END

      SUBROUTINE READ1(IUT,NRED1,MTV3,DOUT,PDAT)
      PARAMETER (IRX=41,JRX=41)
      REAL DOUT(IRX,JRX),PDAT(IRX,JRX,MTV3)
      NRED1=NRED1+1
!      write(6,*)'READ1 COUNT = ',NRED1
      DO J=1,JRX
      DO I=1,IRX
        DOUT(I,J)=PDAT(I,J,NRED1)
      END DO
      END DO 
      END

      SUBROUTINE WRIT2(IMAX,JMAX,NWRT2,MTV,DIN,HDATA)
      REAL DIN(IMAX,JMAX),HDATA(IMAX,JMAX,MTV)
      NWRT2=NWRT2+1
c      write(6,*)'WRIT2 COUNT = ',NWRT2
c      call maxmin(DIN,IMAX*JMAX,1,1,1,'DIN in gbl')
      DO J=1,JMAX
      DO I=1,IMAX
        HDATA(I,J,NWRT2)=DIN(I,J)
      END DO
      END DO
      END

      SUBROUTINE READ2(IMAX,JMAX,NRED2,MTV,DOUT,HDATA)
      REAL DOUT(IMAX,JMAX),HDATA(IMAX,JMAX,MTV)
      NRED2=NRED2+1
c      write(6,*)'READ2 COUNT = ',NRED2
      DO J=1,JMAX
      DO I=1,IMAX
        DOUT(I,J)=HDATA(I,J,NRED2)
      END DO
      END DO
      END


      SUBROUTINE HURR_REL(MWAVE,IMAX,JMAX,KMAX,IKMAX,MAXWV2,
     1                    JHF,MTV,MTV1,MTV2,MTV3,
     2                    HDAT,HDATA,PDAT,data1,head,PSLB,SL,
     3                    nvcd,idvc,idsl,vcrd,idvm,ntrac,wrk1_4,
     4                    wrk2_4,STRPSF)

c      SUBROUTINE HURR_REL(MWAVE,KMAX,MAXWV2,
c     1                       MTV,MTV1,MTV2,MTV3,
c     2                       HDAT,HDATA,PDAT,PSLB)

C
C SEPARATE HURRICANE VORTEX FROM ENVIRONMENTAL FIELD, THEN
C RELOCATE THE HURRICANCE VORTEX TO NEW LOCATION 
C      REF, Kurihara et al 1992, 1995. MWR
C

      use sigio_module
!     use sigio_r_module


      PARAMETER (IX=41,JX=41,NF=11,IT=24,IR=120,IJ=IX*JX)
      PARAMETER (NSG=24000)
      PARAMETER (NST=10)
c      PARAMETER (IMAX=384,JMAX=190,NSG=24000)
c      PARAMETER (JHF=JMAX/2,NST=10)
C
      integer     MWAVE,IMAX,JMAX,KMAX,IKMAX,MAXWV2,
     &                    JHF,MTV,MTV1,MTV2,MTV3
      integer * 4 nvcd,idvc,idsl,idvm,ntrac
     &,           imjm4, km4, iret
      real (4)    wrk1_4(imax,jmax,kmax)
     &,           wrk2_4(imax,jmax,kmax,ntrac)
!     real (4)    wrk1_4(:,:,:), wrk2_4(:,:,:,:)
      DIMENSION U(IX,JX),V(IX,JX),UD(IX,JX),US(IX,JX),VS(IX,JX)
      DIMENSION SKIP(IX,JX),M(11),FK(NF),TW(IT,IR)
      DIMENSION VD(IX,JX),XTU(IX,NF),XTV(IX,NF),DKY(IX,JX)
      DIMENSION YTU(IX,JX),YTV(IX,JX),RS(IT),R0(IT),RF(IT)
!      DIMENSION INP(IJ),JNP(IJ),CM(IJ),DIST(IJ)
      DIMENSION ALAT(JX),ALON(IX),ZG(IX,JX),DATG(IMAX,JMAX)
      DIMENSION GLON(IMAX,JMAX),GLAT(IMAX,JMAX),ZDATG(IMAX,JMAX)
      DIMENSION ING(NSG),JNG(NSG)
      REAL COLRAD(JHF), WGT(JHF),WGTCS(JHF),RCS2(JHF)
      COMMON /ST/ALON,ALAT

      COMMON /NHC/ KSTM1,IC_N(NST),JC_N(NST)
      COMMON /NHC1/SLON_N(NST),SLAT_N(NST),CLON_N(NST),CLAT_N(NST)

      COMMON /NHC2/MDX,MDY
      COMMON /NHC3/AMDX,AMDY
      COMMON /POSIT/CLON_NEW,CLAT_NEW,SLON,SLAT,CLON,CLAT,RAD                   
      COMMON /vect/R0,XVECT(IT),YVECT(IT)      
      COMMON /TR/ING,JNG,IB
c      COMMON /TR/ZDATG,GLON,GLAT,ING,JNG,IB
      COMMON /CHNL/IUT,KSTM

      COMMON /HDAT1/NWRT1,NRED1,NWT1
      COMMON /HDAT3/NWRT2,NRED2
      REAL PSLB(IMAX,JMAX)
      REAL(4) SL(KMAX)
      REAL HDAT(IX,JX,MTV2,NST),HDATA(IMAX,JMAX,MTV)
      REAL PDAT(IX,JX,MTV3)
      REAL HSIG(IX,JX,KMAX),HP(IX,JX,2*KMAX+1)

      REAL STRPSF(NST)

      INTEGER ISTMCX1(3,NST),ISTMCY1(3,NST)
      REAL    STMCX(NST),STMCY(NST)
      CHARACTER ST_NAME(NST)*3,STMNAME(NST)*3,TCVT(NST)*95
      COMMON /STNAME/ST_NAME
      COMMON /TCVIT/TCVT
      COMMON /CHEN/KUNIT,ITIM

!      REAL(4) FHOUR,DUMMY(245)
!      REAL(4) SKIP2(MAXWV2,MTV1+IKMAX)
!      CHARACTER*8 LAB(4)
!      DIMENSION IDATE(4)
      DIMENSION DKM(IX,JX)
      DIMENSION ENV(IX,JX,MTV2),ENV1(IX,JX,MTV3)

      real (4) vcrd(KMAX+1,nvcd)

      type(sigio_head):: head
      type(sigio_data):: data1
cc
      REAL(4),ALLOCATABLE :: WORK_3(:),PS2(:)
      REAL,   ALLOCATABLE :: WK_S1(:,:),WK_S2(:,:),WK_G(:,:,:)
      
!      COMMON /COEF1/LAB
!      COMMON /COEF2/IDATE
!      COMMON /COEF3/FHOUR,DUMMY
!      COMMON /COEF5/NCNT,NCNT2
C
      DATA M/2,3,4,2,5,6,7,2,8,9,2/
C
      ALLOCATE ( WORK_3(MAXWV2),PS2(MAXWV2) )
      ALLOCATE ( WK_S1(MAXWV2,KMAX),WK_S2(MAXWV2,KMAX) )
      ALLOCATE ( WK_G(IMAX,JMAX,KMAX) )

!      NCNT2 = 0

      DO LO = 1,NSG
      ING(LO) = 0
      JNG(LO) = 0
      ENDDO
C
      CALL GLATS(JHF,COLRAD,WGT,WGTCS,RCS2)
C
      PI=ASIN(1.)*2
      RAD=PI/180.
C
      DO I = 1,IMAX
      DO LL = 1,JHF 
      LLS = JMAX+1 - LL
      GLAT(I,LL)  = 90. - COLRAD(LL)/RAD
      GLAT(I,LLS) = -GLAT(I,LL)
      ENDDO
      ENDDO
C
      DLN = 360.0/FLOAT(IMAX)
      DO J = 1,JMAX
      DO LN = 1,IMAX
      GLON(LN,J) = (LN-1) * DLN
      ENDDO
      ENDDO
C
c      REWIND 12
c      REWIND 20

cql      READ(20)LAB
c      WRITE(6,124) LAB
124   FORMAT(4A8)
!1111      WRITE(KUNIT) LAB
!      WRITE(6,210) (IDATE(I),I=1,4)
c     1           ,FHOUR,(DUMMY(K),K=1,2*KMAX+1)
!210   FORMAT(5X,' INPUT DATE AND FCST HOUR ',4I5,F7.1/(2X,G13.6))
!1111      WRITE(KUNIT)FHOUR,(IDATE(I),I=1,4),DUMMY
      
cql      READ(20)(GZ(NW),NW=1,MAXWV2)
!      NCNT2 = NCNT2 +1
!      DO NW=1,MAXWV2
!       WORK_3(NW)=SKIP2(NW,NCNT2)
!      END DO
!      WRITE(KUNIT)(WORK_3(NW),NW=1,MAXWV2)
cql      READ(20)SKIP2
!      NCNT2 = NCNT2 + 1

c      WRITE(KUNIT)(SKIP2(NW),NW=1,MAXWV2)
C
      DO I=1,NF
      FK(I)=0.5/(1-COS(2.*PI/M(I)))
      ENDDO
C
c      READ(12, 233) KSTM
c 233  FORMAT(2x,I5)
      KSTM = KSTM1

      write(6,244) KSTM
 244  FORMAT('NUMBER OF STORMS: ',I5)

      INDX1=ITIM/3

      K1STM=0
      DO I=1,NST
        STMCX(I)=0.
        STMCY(I)=0.
        STMNAME(I)='NUL'
        READ(30,442,end=436)
     &     (ISTMCY1(J,I),ISTMCX1(J,I),J=1,3),STMNAME(I)
        STMCX(I)=360.-ISTMCX1(INDX1,I)*0.1
        STMCY(I)=ISTMCY1(INDX1,I)*0.1
        K1STM=K1STM+1
c        write(6,*)'QLIU test=',STMNAME(I),STMCX(I),STMCY(I)
      END DO
 442  FORMAT(22x,6i4,25x,A3)
 436  CONTINUE

      DO 788 KST=1,KSTM
      
c      IUT=89+KST
      IUT=KST
C
        DO K=1,MTV2
        DO J=1,JX
        DO I=1,IX
          ENV(I,J,K)=HDAT(I,J,K,KST)
        END DO
        END DO
        END DO

!     write(6,*)' ENV=',ENV(41,41,1:67)

        PSC_MX=0.
        DO J=1,JX
        DO I=1,IX
          IF(PSC_MX.LT.ENV(I,J,2))PSC_MX=ENV(I,J,2)
        END DO
        END DO
        PSC_MX1=EXP(PSC_MX)*1000.+500.0       
        write(6,*)'MAX SFC PRESS=',PSC_MX1

        CALL SIG2P(KMAX,MTV2,MTV3,ENV(1,1,1),PDAT(1,1,1),
     &          PSC_MX1,HSIG,HP,KST,nvcd,idvc,idsl,vcrd)

      NWRT1 = 0
      NWRT2 = 0
      NRED1 = 0
      NRED2 = 0

      CALL READ2(IMAX,JMAX,NRED2,MTV,ZDATG,HDATA)

c     WRIT2(NWRT2,MTV,ZDATG,HDATA)
      NWRT2 = 1

      CALL READ1(IUT,NRED1,MTV3,ZG,PDAT)

      call maxmin(zg,ix*jx,1,1,1,'regional terrain')
      IFLAG=0
cnew      DO J=1,JX
cnew      DO I=1,IX
cnew        IF(ZG(I,J).GT.200.)THEN
cnew          IFLAG=1
c          write(6,*)'Max Terrain Height > 200 m'
cnew          GO TO 443
cnew        END IF
cnew      END DO
cnew      END DO
cnew 443  CONTINUE
C

C.. READ U, V at ~850 mb

!      K850=3+KMAX+(KMAX/4)*4+1
      K8501=1
      DIST2=ABS(SL(1)-0.85)
      DO K=1,KMAX
        DIST1=ABS(SL(K)-0.85)
        IF(DIST1.LT.DIST2)THEN
          K8501=K
          DIST2=DIST1
        END IF
      END DO

! Be consistent with 2001 operational model for KMAX=42
! set K8501=K8501+1
      IF(KMAX.EQ.42) K8501=K8501+1  
 
      K850=3+KMAX+4*(K8501-1)+1

      IF(K8501.LT.1.OR.K8501.GT.KMAX)THEN
        write(6,*)'K8501 is out of bound'
        STOP
      END IF 

      write(6,*)'QLIUQLIU test',K850
      
      NRED1 = NRED1 + K850
      DO J=1,JX
      DO I=1,IX
        U(I,J)=HDAT(I,J,K850+2,KST)
        V(I,J)=HDAT(I,J,K850+3,KST)
      END DO
      END DO
        
C
c qliu
c get Hurricane Center
c      READ(12,123)SLON,SLAT,CLON_NHC,CLAT_NHC
c123   FORMAT(1X,4F10.2)
      SLON = SLON_N(KST)
      SLAT = SLAT_N(KST)
      CLON_NHC = CLON_N(KST)
      CLAT_NHC = CLAT_N(KST)

      CLON = SLON+20.
      CLAT = SLAT+20.
c      write(6,*)'CLON, CLAT, SLON, SLAT=',CLON, CLAT, SLON, SLAT
c      write(6,*)'CLON_NHC,CLAT_NHC=',CLON_NHC,CLAT_NHC
c      fact=cos(CLAT*rad)
      fact=1.0
      do j=1,jx
      do i=1,ix
! East-West wind in new coordinate (phi,theta)
! this conversion only affects Hurrican Center determination and R0
        U(I,J)=U(I,J)/fact      
      end do
      end do
C.. DO ZONAL FILTER
C
!!$OMP PARALLEL DO DEFAULT(SHARED)
!!$OMP+ PRIVATE(J,N,I,XTU,XTV)
      DO 100 J=1,JX
      DO N=1,NF
      XTU(1,N)  = U(1,J)
      XTU(IX,N) = U(IX,J)
      XTV(1,N)  = V(1,J)
      XTV(IX,N) = V(IX,J)
      ENDDO
C
      DO I=2,IX-1
      XTU(I,1) = U(I,J)+FK(1)*(U(I-1,J)+U(I+1,J)-2.*U(I,J))
      XTV(I,1) = V(I,J)+FK(1)*(V(I-1,J)+V(I+1,J)-2.*V(I,J))
      ENDDO
C
      DO N=2,NF
      DO I=2,IX-1
      XTU(I,N)=XTU(I,N-1)+FK(N)*(XTU(I-1,N-1)+XTU(I+1,N-1)-2.
     1         *XTU(I,N-1))
      XTV(I,N)=XTV(I,N-1)+FK(N)*(XTV(I-1,N-1)+XTV(I+1,N-1)-2.
     1         *XTV(I,N-1))
      ENDDO
      ENDDO
C
      DO I=1,IX
      US(I,J) = XTU(I,NF)
      VS(I,J) = XTV(I,NF)
      ENDDO
C
100   CONTINUE
!!$OMP END PARALLEL DO
C
C.. DO MERIDIONAL FILTER 
C
!!$OMP PARALLEL DO DEFAULT(SHARED)
!!$OMP+ PRIVATE(I,N,J,YTU,YTV)
      DO 200 I=1,IX
C
      DO N=1,NF
      YTU(1,N)  = US(I,1)
      YTU(JX,N) = US(I,JX)
      YTV(1,N)  = VS(I,1)
      YTV(JX,N) = VS(I,JX)
      ENDDO
C
      DO J = 2 , JX-1
      YTU(J,1) = US(I,J) + FK(1)*(US(I,J-1) + US(I,J+1)
     *                          -2.*US(I,J))
      YTV(J,1) = VS(I,J) + FK(1)*(VS(I,J-1) + VS(I,J+1)
     *                          -2.*VS(I,J))
      ENDDO     
CC
      DO N = 2 , NF
      DO J = 2 , JX-1
      YTU(J,N) = YTU(J,N-1) + FK(N)*(YTU(J-1,N-1)  +
     *              YTU(J+1,N-1) - 2.*YTU(J,N-1))
      YTV(J,N) = YTV(J,N-1) + FK(N)*(YTV(J-1,N-1)  +
     *              YTV(J+1,N-1) - 2.*YTV(J,N-1))
      ENDDO
      ENDDO
C
      DO J = 1 , JX
      US(I,J)   =  YTU(J,NF)
      VS(I,J)   =  YTV(J,NF)
      ENDDO   
200   CONTINUE
!!$OMP END PARALLEL DO
C
C.. GET THE DISTURBANCE FIELD
C
      DO I=1,IX
      DO J=1,JX
      UD(I,J) = U(I,J) - US(I,J)
      VD(I,J) = V(I,J) - VS(I,J)
      ENDDO
      ENDDO
c      WRITE(39) ((U(I,J),I=1,IX),J=Jx,1,-1)
c      WRITE(39) ((V(I,J),I=1,IX),J=Jx,1,-1)
c      WRITE(39) ((US(I,J),I=1,IX),J=Jx,1,-1)
c      WRITE(39) ((VS(I,J),I=1,IX),J=Jx,1,-1)
c      WRITE(39) ((UD(I,J),I=1,IX),J=Jx,1,-1)
c      WRITE(39) ((VD(I,J),I=1,IX),J=Jx,1,-1)
C
C.. FIND NEW VORTEX CENTER
C
      DO I=1,IX
        ALON(I)=SLON+(I-1)
      END DO
      DO J=1,JX
        ALAT(J)=SLAT+(J-1)
      END DO   

c      CALL FIND_NEWCT1(UD,VD)
      CALL FIND_NEWCT(UD,VD)

      ICHEK=0
      CLON_TIM=0.
      CLAT_TIM=0.
      DO I=1,K1STM
        IF(STMNAME(I).EQ.ST_NAME(KST))THEN
          CLON_TIM=STMCX(I)
          CLAT_TIM=STMCY(I)
          ICHEK=1
          GO TO 446
        END IF
      END DO
 446  CONTINUE
      IF((ICHEK.EQ.1).AND.(ABS(CLON_TIM).LT.359.5))THEN
        CLON_NEW=CLON_TIM
        CLAT_NEW=CLAT_TIM
      ELSE
        write(6,*)'GFDL CENTER= ',ITIM,'h ',CLON_NEW,CLAT_NEW
        CLON_NEW=CLON_NHC
        CLAT_NEW=CLAT_NHC
      ENDIF

C
C.. CALCULATE TANGENTIAL WIND AROUND CIRCLE 
C             24 DIRECTION, RADIALLY 0.1DEG INTERVAL 
C
      CALL TWIND(UD,VD,TW)
C
C.. CALCULATE STARTING POINT AT EACH DIRECTION
C
      CALL STRT_PT(RS,TW,RFAVG)
C
C.. DETERMINE FILTER DOMAIN D0 (=1.25*Rf)
C
      CALL FILTER(RS,TW,RF,RFAVG,STRPSF,KST)

      AMDX=CLON_NHC-CLON_NEW
      AMDY=CLAT_NHC-CLAT_NEW
      MDX=IFIX((CLON_NHC-CLON_NEW)/DLN)

      IF(ITIM.EQ.6)THEN
        WRITE(52,65)TCVT(KST)(1:32),
     &             CLON_NHC,CLAT_NHC,CLON_NEW,
     &             CLAT_NEW,CLON_TIM,CLAT_TIM,AMDX,AMDY,
     &             SQRT(AMDX*AMDX+AMDY*AMDY)
 65   FORMAT(/'STORM NAME: ',A32,
     &       /'  OBSERVED CENTER POSITION:     ',2F10.2,
     &       /'  MODEL CENTER POSITION :       ',2F10.2,
     &       /'  MODEL CENTER POSITION (TIM):  ',2F10.2,
     &       /'  RELOCATION DISTANCE (DEGREE): ',3F10.2)
        DO I=1,K1STM
          IF(STMNAME(I).EQ.ST_NAME(KST))THEN
            IFWRT=0
            DO J=1,3
              IF(ISTMCY1(J,I).EQ.0.and.ISTMCX1(J,I).EQ.0)THEN
                IFWRT=1
              END IF
            END DO
            IF(IFWRT.EQ.1)THEN
              WRITE(52,76)TCVT(KST)(1:32)
            ELSE
              WRITE(52,77)TCVT(KST)(1:32)
            END IF
 76         FORMAT(/'  STORM: ',A32,10x,' is bogused')
 77         FORMAT(/'  STORM: ',A32,10x)
            WRITE(52,79)
     &      (ISTMCY1(J,I),ISTMCX1(J,I),J=1,3),STMNAME(I)
 79         FORMAT(/'  TRACKER OUTPUT: ',6i4,5x,A3)
          END IF
        END DO
      END IF

c test by qliu
c      MDX=MDX+50
c      AMDX=AMDX+50*DLN

      DO J=1,JMAX-1
        IF(CLAT_NHC.LT.GLAT(1,J).and.
     &         CLAT_NHC.GE.GLAT(1,J+1))THEN
          MNHC=J
          IF(CLAT_NHC.LT.0.5*(GLAT(1,J)+GLAT(1,J+1)))MNHC=J+1
          GO TO 566
        END IF
      END DO
 566  CONTINUE 
      DO J=1,JMAX-1
        IF(CLAT_NEW.LT.GLAT(1,J).and.
     &         CLAT_NEW.GE.GLAT(1,J+1))THEN
          MNEW=J
          IF(CLAT_NEW.LT.0.5*(GLAT(1,J)+GLAT(1,J+1)))MNEW=J+1
          GO TO 577
        END IF
      END DO  
 577  CONTINUE
      MDY=MNHC-MNEW
      write(6,*)'MDX,MDY,MNHC,MNEW=',MDX,MDY,MNHC,MNEW
      write(6,*)'AMDX,AMDY=',AMDX,AMDY
      write(6,*)'CLON_NHC,CLAT_NHC=',CLON_NHC,CLAT_NHC
      write(6,*)'CLON_NEW,CLAT_NEW=',CLON_NEW,CLAT_NEW
      RDIST2=AMDX*AMDX+AMDY*AMDY
      IF(RDIST2.LE.0.02)THEN
        write(6,*)'   '
        write(6,*)' FORECAST TIME= ',ITIM,'h ',
     1         ', STORM NAME= ',ST_NAME(KST)
        write(6,*)' CENTER DISTANCE is less than 15 km',
     1  ', storm is not relocated'
        AMDX=0.
        AMDY=0.
        MDX=0
        MDY=0
c        IF(KST.NE.KSTM)THEN
c          DO I=1,113
c           READ(IUT1) DATG
c           WRITE(IUT2) DATG
c          END DO
c          GO TO 788
c        END IF
      ELSE
        write(6,*)'    '
        write(6,*)' FORECAST TIME= ',ITIM,'h',
     1         ' STORM NAME= ',ST_NAME(KST)
        write(6,*)' Center Distance = ',SQRT(RDIST2),' Deg.',
     3  ', relocation starts ...... '

      END IF
c
      IB=0
C
!!$OMP PARALLEL DO DEFAULT(SHARED)
!!$OMP+ PRIVATE(J,I,A,B,R,TH,IC,M2)
      DO J=1,JMAX
      DO I=1,IMAX
      A = GLON(I,J) - CLON_NEW
      B = GLAT(I,J) - CLAT_NEW
      R = SQRT(A**2. + B**2.)
      IF(R.EQ.0.) GO TO 444
      TH = ACOS(A/R) / RAD
      IF(B.LT.0.) TH = 360-TH
C
      IF(TH.LE.7.5 .OR. TH.GT.352.5 ) IC = 1
      DO M2=2,24
        IF((TH.GT.(15.*(M2-1)-7.5)).and.
     &     (TH.LE.(15.*M2-7.5)))IC=M2
      END DO
C
      IF(R.LT.R0(IC)) THEN
      IB = IB+1
      ING(IB) = I
      JNG(IB) = J
      ENDIF
C
      GO TO 22
C
444   IB = IB+1
      ING(IB) = I
      JNG(IB) = J
22    CONTINUE
      ENDDO
      ENDDO
!!$OMP END PARALLEL DO
C
      CALL landcmsk(IMAX,JMAX,GLON,GLAT,ZDATG,IFLAG,lsflag,kst)

c temp relocation turned on
c      IFLAG = 0 

c Check if the syndata need to be called

      IF(ITIM.EQ.3)THEN
        DO I=1,K1STM
          IF(STMNAME(I).EQ.ST_NAME(KST))THEN
            IFWRT=0
            DO J=1,3
        IF(ISTMCY1(J,I).EQ.0.and.ISTMCX1(J,I).EQ.0)THEN
              IFWRT=1
            END IF
            END DO
            IF(IFWRT.EQ.1)THEN
              WRITE(55,101) TCVT(KST)
            END IF
  101       FORMAT(A95)
          END IF
        END DO
      END IF


      write(6,*)'GAUSSIAN GRID # WITHIN R0 ',IB
!      DO I = 1,IB
!      write(6,*)'GAUSSIAN GRID WITHIN R0, LAT,LON ',ING(I),
!     1      JNG(I),GLAT(ING(I),JNG(I)),GLON(ING(I),JNG(I))
!      write(6,*)'GAUSSIAN GRID WITHIN R0 ',ING(I),JNG(I)
!      ENDDO

C.. SETTING VALUE for xvect, yvect, a(,), capd2

      call rodist

      call amatrix
c
c      REWIND IUT

      KMP=2*KMAX+1
      KDIV1=3+KMP
      KQ1=KDIV1+4*KMP

      NRED1 = 0

      IG = 0
      DO 777 IV = 1,MTV3

      IREM = -22
C
      CALL READ1(IUT,NRED1,MTV3,SKIP,PDAT)
C
      DO J=1,JX
      DO I=1,IX
        ENV1(I,J,IV) = SKIP(I,J)
      ENDDO
      ENDDO

      IF(IV.GT.KDIV1.AND.IV.LE.KQ1)IREM=MOD(IV-KDIV1,4)
      IF((IV.GE.3.AND.IV.LE.KDIV1).OR.(IV.GT.KQ1).OR.
     1     (IREM.EQ.1.OR.IREM.EQ.2)) THEN
      IG = IG+1
!      write(6,*)'ORIGINAL VARIABLE # IS ',IV
!      write(6,*)'VARIABLE # IS ',IG

c added by Qingfu Liu
c obtain the disturbance field

      DO J=1,JX
      DO I=1,IX
        U(I,J)=SKIP(I,J)
      END DO
      END DO
c
c First smooth in east-west direction
c
!!$OMP PARALLEL DO DEFAULT(SHARED)
!!$OMP+ PRIVATE(J,N,I,XTU)
      DO 107 J=1,JX
      DO N=1,NF
      XTU(1,N)  = U(1,J)
      XTU(IX,N) = U(IX,J)
      ENDDO
C
      DO I=2,IX-1
      XTU(I,1) = U(I,J)+FK(1)*(U(I-1,J)+U(I+1,J)-2.*U(I,J))
      ENDDO
C
      DO N=2,NF
      DO I=2,IX-1
      XTU(I,N)=XTU(I,N-1)+FK(N)*(XTU(I-1,N-1)+XTU(I+1,N-1)-2.
     1         *XTU(I,N-1))
      ENDDO
      ENDDO
C
      DO I=1,IX
      US(I,J) = XTU(I,NF)
      ENDDO
C
 107  CONTINUE
!!$OMP END PARALLEL DO
C
C.. DO MERIDIONAL FILTER
C
!!$OMP PARALLEL DO DEFAULT(SHARED)
!!$OMP+ PRIVATE(I,N,J,YTU)
      DO 207 I=1,IX
C
      DO N=1,NF
      YTU(1,N)  = US(I,1)
      YTU(JX,N) = US(I,JX)
      ENDDO
C
      DO J = 2 , JX-1
      YTU(J,1) = US(I,J) + FK(1)*(US(I,J-1) + US(I,J+1)
     *                          -2.*US(I,J))
      ENDDO
CC
      DO N = 2 , NF
      DO J = 2 , JX-1
      YTU(J,N) = YTU(J,N-1) + FK(N)*(YTU(J-1,N-1)  +
     *              YTU(J+1,N-1) - 2.*YTU(J,N-1))
      ENDDO
      ENDDO
C
      DO J = 1 , JX
      US(I,J)   =  YTU(J,NF)
      ENDDO
 207  CONTINUE
!!$OMP END PARALLEL DO
C
C.. GET THE DISTURBANCE FIELD
C
      DO I=1,IX
      DO J=1,JX
      DKY(I,J) = U(I,J) - US(I,J)
      ENDDO
      ENDDO

      DKM=DKY
      CALL SEPAR(DKY,DKM)
      
      DO J=1,JX                                                                 
      DO I=1,IX
        SKIP(I,J)=DKM(I,J)
c        SKIP(I,J)=U(I,J)
        DKY(I,J) = DKM(I,J) + US(I,J)
c        DKY(I,J) = U(I,J)
      ENDDO
      ENDDO

      
      DO J=1,JX
      DO I=1,IX
cnew        ENV1(I,J,IV) = DKY(I,J)
        ENV1(I,J,IV) = DKY(I,J)-PDAT(I,J,IV)
      ENDDO
      ENDDO

      ENDIF

 777  CONTINUE

      ENV=0.

      CALL P2SIG(KMAX,MTV2,MTV3,ENV(1,1,1),ENV1(1,1,1),
     &       PDAT(1,1,1),HDAT(1,1,1,KST),PSC_MX1,HSIG,HP,KST,
     &       nvcd,idvc,idsl,vcrd)
!      CALL P2SIG(KMAX,MTV2,MTV3,ENV(1,1,1),ENV1(1,1,1),
!     &           KST)

cnew    K=1,2 and the U,V field was doubled here, but never used later
      DO K=1,MTV2
      DO J=1,JX
      DO I=1,IX
        ENV(I,J,K)=ENV(I,J,K)+HDAT(I,J,K,KST)
      END DO
      END DO
      END DO 
cnew

      KDIV2=3+KMAX
      KQ2=KDIV2+4*KMAX

      IG = 0
      DO 781 IV = 1,MTV2

      IREM = -22
C
      IF(IV.GT.KDIV2.AND.IV.LE.KQ2)IREM=MOD(IV-KDIV2,4)
      IF((IV.GE.3.AND.IV.LE.KDIV2).OR.(IV.GT.KQ2).OR.
     1     (IREM.EQ.1.OR.IREM.EQ.2)) THEN
      IG = IG+1

      DO J=1,JX
        DO I=1,IX
          DKY(I,J) = ENV(I,J,IV)
        ENDDO
      ENDDO
      CALL GMOVE(KST,MWAVE,KMAX,IKMAX,IMAX,JMAX,MAXWV2,MTV,MTV1,
     1          HDATA,DKY,IG,IFLAG,PSLB,ZDATG,GLON,GLAT,PS2,idvm)

c      CALL GMOVE(KST,MWAVE,MAXWV2,MTV,MTV1,HDATA,SKIP2,DKY,
c     1          IG,IFLAG,PSLB)

      ENDIF

 781  CONTINUE

 788  CONTINUE

      DO I=1,MAXWV2
        data1%ps(I) = PS2(I)
      END DO
!
      if (mod(idvm/10,10) == 3 .and. idvc == 3) then
        imjm4 = imax*jmax ; km4 = kmax
!     write(6,*)' imjm4=',imjm4,' km4=',km4,' ntrac=',ntrac
!    &,' cpi=',head%cpi,' in HURR_REL'
        wrk1_4 = hdata(:,:,3:kmax+2)
     &         / (1.+(461.50/287.05-1)*HDATA(:,:,3+3*kmax:2+4*kmax))
        call sigio_cnvtdv(imjm4, imjm4, km4, idvc
     &,                   idvm, ntrac, iret, wrk1_4
     &,                   wrk2_4, head%cpi,-1)
!     write(6,*)' iret=',iret,' 2after cnvtdv','wrk2_4=',wrk1_4(1,1,1)
        if (iret /= 0) then
          write(6,*)' return code from cnvtdv = ',iret,' job stopping'
          stop 777
        endif
        hdata(:,:,3:2+kmax) = wrk1_4
      endif

      CALL SPTEZM(0,MWAVE,4,IMAX,JMAX,KMAX,WK_S1,
     &            HDATA(1,1,3),-1) 

      DO K=1,KMAX
        DO I=1,MAXWV2
          data1%t(I,K)=WK_S1(I,K)
        END DO
      END DO

      DO K=1,KMAX
        DO J=1,JMAX
          DO I=1,IMAX
            WK_G(I,J,K) = HDATA(I,J,KMAX+1+2*K)
          END DO
        END DO
      END DO

      CALL SPTEZM(0,MWAVE,4,IMAX,JMAX,KMAX,WK_S1,WK_G,-1)

      DO K=1,KMAX
        DO J=1,JMAX
          DO I=1,IMAX
            WK_G(I,J,K) = HDATA(I,J,KMAX+2+2*K)
          END DO
        END DO
      END DO

      CALL SPTEZM(0,MWAVE,4,IMAX,JMAX,KMAX,WK_S2,WK_G,-1)

      DO K=1,KMAX
        DO I=1,MAXWV2
          data1%d(I,K) = WK_S1(I,K)
          data1%z(I,K) = WK_S2(I,K)
        END DO
      END DO

      CALL SPTEZM(0,MWAVE,4,IMAX,JMAX,KMAX,WK_S1,
     &            HDATA(1,1,3+3*KMAX),-1)

      DO K=1,KMAX
        DO I=1,MAXWV2
          data1%q(I,K,1)=WK_S1(I,K)
        END DO
      END DO

 
C
!      DO K=1,KMAX
c        READ(20)SKIP2
c        READ(20)SKIP2  
cql        READ(20)
cql        READ(20)
!        NCNT2 = NCNT2 + 2
!      END DO
!      DO K=1,KMAX
cql        READ(20)
!         NCNT2 = NCNT2 + 1
c        READ(20)SKIP2
c        WRITE(KUNIT)(SKIP2(NW),NW=1,MAXWV2)                 
!      END DO
!      DO K=1,KMAX
cql        READ(20)(OZ(NW),NW=1,MAXWV2) 
cql        WRITE(KUNIT)(OZ(NW),NW=1,MAXWV2)                 
!        NCNT2 = NCNT2 +1
!        NCNT2 = 4*KMAX+2+K
!        WRITE(KUNIT)(SKIP2(NW,NCNT2),NW=1,MAXWV2)
 
!      END DO

!      DO K=1,IKMAX
!        NCNT2 = NCNT2 +1
!        WRITE(KUNIT)(SKIP2(NW,NCNT2),NW=1,MAXWV2)
!      END DO

      DEALLOCATE ( WORK_3,PS2 )
      DEALLOCATE ( WK_S1, WK_S2, WK_G )

C
      RDIST2 = AMDX*AMDX + AMDY*AMDY
      IF(RDIST2.LE.0.02)THEN
        write(6,*)'TIME= ',ITIM,'h, Model Center is not relocated'
      ELSE
        write(6,*)'TIME= ',ITIM,'h, vortex relocation is completed' 
      END IF

      RETURN
      END
C
      SUBROUTINE FIND_NEWCT(UD,VD)
      PARAMETER (IR=15,IT=24,IX=41,JX=41,ID=7,JD=7)
      DIMENSION TNMX(ID,JD),UD(IX,JX),VD(IX,JX)
      DIMENSION WTM(IR),R0(IT)
      COMMON /POSIT/CLON_NEW,CLAT_NEW,SLON,SLAT,CLON,CLAT,RAD                   
      COMMON /vect/R0,XVECT(IT),YVECT(IT)      
c      COMMON /CT/SLON,SLAT,CLON,CLAT,RAD
c      COMMON /GA/CLON_NEW,CLAT_NEW,R0
C
      PI=ASIN(1.)*2.
      RAD=PI/180.
C
      XLAT = CLAT-3.
      XLON = CLON-3.
c      write(6,*)'STARTING LAT, LON AT FIND NEW CENTER ',XLAT,XLON
C
      DO I=1,ID
      DO J=1,JD
      TNMX(I,J) = 0.
      BLON = XLON + (I-1)
      BLAT = XLAT + (J-1)
C
C.. CALCULATE TANGENTIAL WIND EVERY 1 deg INTERVAL
C..  7*7 deg AROUND 1ST 1ST GUESS VORTEX CENTER
C
      DO 10 JL=1,IR
      WTS= 0.
      DO 20 IL=1,IT
      DR = JL
      DD = (IL-1)*15*RAD
      DLON = DR*COS(DD)
      DLAT = DR*SIN(DD)
      TLON = BLON + DLON
      TLAT = BLAT + DLAT
C.. INTERPOLATION U, V AT TLON,TLAT AND CLACULATE TANGENTIAL WIND
      IDX = IFIX(TLON) - SLON + 1
      IDY = IFIX(TLAT) - SLAT + 1
      DXX  = TLON - IFIX(TLON)
      DYY  = TLAT - IFIX(TLAT)
C
      X1 = UD(IDX  ,IDY+1)*DYY + UD(IDX  ,IDY)*(1-DYY)
      X2 = UD(IDX+1,IDY+1)*DYY + UD(IDX+1,IDY)*(1-DYY)
      Y1 = UD(IDX+1,IDY  )*DXX + UD(IDX,IDY  )*(1-DXX)
      Y2 = UD(IDX+1,IDY+1)*DXX + UD(IDX,IDY+1)*(1-DXX)
      UT = (X1*(1-DXX)+X2*DXX + Y1*(1-DYY)+Y2*DYY)/2.
      IF(IL.EQ.0.OR.IL.EQ.13) UT = Y1
      IF(IL.EQ.7.OR.IL.EQ.19) UT = X1
C
      X1 = VD(IDX  ,IDY+1)*DYY + VD(IDX  ,IDY)*(1-DYY)
      X2 = VD(IDX+1,IDY+1)*DYY + VD(IDX+1,IDY)*(1-DYY)
      Y1 = VD(IDX+1,IDY  )*DXX + VD(IDX,IDY  )*(1-DXX)
      Y2 = VD(IDX+1,IDY+1)*DXX + VD(IDX,IDY+1)*(1-DXX)
      VT = (X1*(1-DXX)+X2*DXX + Y1*(1-DYY)+Y2*DYY)/2.
      IF(IL.EQ.0.OR.IL.EQ.13) VT = Y1
      IF(IL.EQ.7.OR.IL.EQ.19) VT = X1
C.. TANGENTIAL WIND
      WT = -SIN(DD)*UT + COS(DD)*VT
      WTS = WTS+WT
20    CONTINUE
      WTM(JL) = WTS/24.
10    CONTINUE
C
C Southern Hemisphere
      IF(CLAT_NEW.LT.0)THEN
        DO JL=1,IR
          WTM(JL)=-WTM(JL)
        END DO
      END IF
C EnD SH

      TX = -10000000.
      DO KL = 1,IR
      IF(WTM(KL).GE.TX) THEN
      TX = WTM(KL)
      ENDIF
      ENDDO
C
      TNMX(I,J) = TX
      ENDDO
      ENDDO


C.. FIND NEW CENTER
      TTX = -1000000.
      DO I=1,ID
      DO J=1,JD
      IF(TNMX(I,J).GE.TTX) THEN
      TTX = TNMX(I,J)
      NIC = I
      NJC = J
      ENDIF
      ENDDO
      ENDDO
C
      CLAT_NEW = XLAT + (NJC-1)
      CLON_NEW = XLON + (NIC-1)
C
      write(6,*)'NEW CENTER,  I, J IS   ',NIC,NJC
      write(6,*)'NEW CENTER, LAT,LON IS ',CLAT_NEW,CLON_NEW
      write(6,*)'MAX TAN. WIND AT NEW CENTER IS ',TTX
C
      RETURN
      END
C
      SUBROUTINE TWIND(UD,VD,TW)
C
      PARAMETER (IX=41,JX=41,NF=11,IT=24,IR=120)
      DIMENSION UD(IX,JX),VD(IX,JX),TW(IT,IR),R0(IT)
      COMMON /POSIT/CLON_NEW,CLAT_NEW,SLON,SLAT,CLON,CLAT,RAD                   
      COMMON /vect/R0,XVECT(IT),YVECT(IT)      
c      COMMON /CT/SLON,SLAT,CLON,CLAT,RAD
c      COMMON /GA/CLON_NEW,CLAT_NEW,R0
C
!!$OMP PARALLEL DO DEFAULT(PRIVATE)
!!$OMP+ SHARED(UD,VD,TW,R0)
!!$OMP+ SHARED(CLON_NEW,CLAT_NEW,SLON,SLAT,CLON,CLAT,RAD,XVECT,YVECT)
      DO J=1,IR
      DO I=1,IT
C.. DETERMINE LAT, LON AREOUND CIRCLE
      DR = 0.1*J
      DD = (I-1)*15.*RAD
      DLON = DR*COS(DD)
      DLAT = DR*SIN(DD)
      TLON = CLON_NEW + DLON
      TLAT = CLAT_NEW + DLAT
C.. INTERPOLATION U, V AT TLON,TLAT AND CLACULATE TANGENTIAL WIND
      IDX = IFIX(TLON) - SLON + 1
      IDY = IFIX(TLAT) - SLAT + 1
      DXX  = TLON - IFIX(TLON)
      DYY  = TLAT - IFIX(TLAT)
C
      X1 = UD(IDX  ,IDY+1)*DYY + UD(IDX  ,IDY)*(1-DYY)
      X2 = UD(IDX+1,IDY+1)*DYY + UD(IDX+1,IDY)*(1-DYY)
      Y1 = UD(IDX+1,IDY  )*DXX + UD(IDX,IDY  )*(1-DXX)
      Y2 = UD(IDX+1,IDY+1)*DXX + UD(IDX,IDY+1)*(1-DXX)
      UT = (X1*(1-DXX)+X2*DXX + Y1*(1-DYY)+Y2*DYY)/2.
      IF(I.EQ.0.OR.I.EQ.13) UT = Y1
      IF(I.EQ.7.OR.I.EQ.19) UT = X1
C
      X1 = VD(IDX  ,IDY+1)*DYY + VD(IDX  ,IDY)*(1-DYY)
      X2 = VD(IDX+1,IDY+1)*DYY + VD(IDX+1,IDY)*(1-DYY)
      Y1 = VD(IDX+1,IDY  )*DXX + VD(IDX,IDY  )*(1-DXX)
      Y2 = VD(IDX+1,IDY+1)*DXX + VD(IDX,IDY+1)*(1-DXX)
      VT = (X1*(1-DXX)+X2*DXX + Y1*(1-DYY)+Y2*DYY)/2.
      IF(I.EQ.0.OR.I.EQ.13) VT = Y1
      IF(I.EQ.7.OR.I.EQ.19) VT = X1
C.. TANGENTIAL WIND
      TW(I,J) = -SIN(DD)*UT + COS(DD)*VT
C
      ENDDO
      ENDDO
!!$OMP END PARALLEL DO
C SH
      IF(CLAT_NEW.LT.0)THEN
        DO J=1,IR
        DO I=1,IT
          TW(I,J)=-TW(I,J)
        ENDDO
        ENDDO
      END IF
C End SH
C
      RETURN
      END
C
      SUBROUTINE STRT_PT(RMX,TW,RFAVG)
C
      PARAMETER (IX=41,JX=41,NF=11,IT=24,IR=120)
      DIMENSION TW(IT,IR),TWM(IR),TMXX(IT),RMX(IT)
      REAL JMX
C
      DO I=1,IR
      TWM(I) = 0.
      ENDDO
C
C.. CALCULATE MEAN TANGENTIAL WIND
C
      DO 10 J=1,IR
      TM=0.
      DO 20 I=1,IT
      TM = TM + TW(I,J)
20    CONTINUE 
      TWM(J) = TM/24.
c      write(6,*)'MEAN TANGENTIAL WIND ',J,TWM(J)
10    CONTINUE
C
C.. FIND MAXIMUM TANGENTIAL WIND RADIUS
C
      TMX=-100000000000.
      DO J=1,IR
      IF(TWM(J).GE.TMX) THEN
      TMX=TWM(J)
      JMX = J*0.1
      ENDIF
      ENDDO
C
      write(6,*)'MAXIMUM TANGENTIAL WIND RADIUS ',JMX
      JJ=IFIX(JMX*10.)
      write(6,*)'MAXIMUM TANGENTIAL WIND SPEED  ',TWM(JJ)
C
      JXX = 15 * JMX
c      write(6,*)'JXX, 15*JMX is ',JXX
C
      ICK = 1
      CNT = 0.000004 
c      write(6,*)'CNT  ',CNT
C
      DO 30 K=JXX,120
      IF(TWM(K).GE.6..OR.TWM(K).LT.3.) GO TO 30
      DXX = 10000.
      DV = TWM(K) - TWM(K+1)
      DVDR = DV/DXX
      IF(DVDR.LT.CNT) ICK = ICK+1
      IF(ICK.EQ.3) THEN
      RF=K*0.1
      GO TO 40
      ENDIF
30    CONTINUE
C
40    CONTINUE
      IF(ICK.NE.3) THEN
      DO IK=JXX,120     
      IF(TWM(IK).LE.3) THEN
      RF = IK*0.1
      ICK=3
      GO TO 50
      ENDIF
      ENDDO
      ENDIF 
C
50    CONTINUE
      IF(ICK.NE.3) RF = 12.
C
      RFAVG = RF
c
C.. CALCULATE Ra, Rb..  REF. KURIHARA ET AL. 1995
C
      RA = IFIX((0.5 * JMX)*10.)/10.
      RB = IFIX((0.75 * JMX + 0.25 * RF)*10.)/10.
      IRA = IFIX(RA*10.+0.5)
      IRB = IFIX(RB*10.+0.5)
C
c      write(6,*)'Ra, Rb, Rf  ', RA,RB,RF
C
C.. DETERMINE STARTING POINT FOR EVERY 24 DIRECTION
C
      DO I=1,IT
      TMXX(I) = -100000000.
      DO J=1,IR
      IF(TW(I,J).GE.TMXX(I)) THEN
      TMXX(I) = TW(I,J)
      RMX(I) = J*0.1*1.1
      ENDIF
      ENDDO
      ENDDO
C
c      DO I=1,IT
c      write(6,*)'I, MX TANGENTIAL WIND RADIUS ',I,RMX(I),TMXX(I)
c      ENDDO
C
      DO I=1,IT
      IF (RMX(I).GT.RB.OR.RMX(I).LT.RA) THEN
      TMX = -10000000.
      DO KK=IRA,IRB
      IF(TW(I,KK).GE.TMX) RM = KK * 0.1 * 1.1
      ENDDO
      MR = IFIX(RM*10. + 0.5)
      ICL=0
      DO LL = MR,IRB
      IF(TW(I,LL).LT.0.) ICL=ICL+1
      ENDDO
      IF(ICL.EQ.0) RMX(I) = RM*1.1
      ENDIF
      ENDDO
C
c      DO I=1,IT
c      write(6,*)'I, RST ',I,RMX(I)
c      ENDDO
C
      RETURN
      END
C 
      SUBROUTINE FILTER(RS,TW,RF,RFAVG,STRPSF,KST)
      PARAMETER (IX=41,JX=41,IT=24,IR=120,NST=10)
C
      DIMENSION RS(IT),TW(IT,IR),RF(IT),R0(IT),IST(IT)
      REAL STRPSF(NST)
      COMMON /vect/R0,XVECT(IT),YVECT(IT)      
c      COMMON /GA/CLON_NEW,CLAT_NEW,R0
C
      ICK = 1
      CNT = 0.000004 
c      write(6,*)'CNT  ',CNT
C
      DO I=1,IT
      IST(I) = IFIX(RS(I)*10)      
c      write(6,*)'STARTING POINT ',I,IST(I)
      ENDDO
C
      DO 100 I=1,IT
      IS = IST(I)
C
      DO 30 K=IS,IR 
      IF(TW(I,K).GE.6..OR.TW(I,K).LT.3.) GO TO 30
      DXX = 10000.
      DV = TW(I,K) - TW(I,K+1)
      DVDR = DV/DXX
      IF(DVDR.LT.CNT) THEN
      ICK = ICK+1
      ENDIF
      IF(ICK.EQ.3) THEN
      RF(I)=K*0.1 + 0.0000001
c      write(6,*)'1st Catagory ',I
      GO TO 100
      ENDIF
30    CONTINUE
C
40    CONTINUE
      DO IK=IS,IR
      IF(TW(I,IK).LE.3) THEN
      RF(I) = IK*0.1 + 0.00000001
c      write(6,*)'2nd Catagory ',I
      GO TO 100
      ENDIF
      ENDDO
C
50    CONTINUE
c      write(6,*)'3rd Catagory ',I
      RF(I) = 12.
100   CONTINUE
C
c      RMAX=0.
      DO I=1,IT
      R0(I) = 1.25 * RF(I)
!! NEW
!      IF(R0(I).LT.2.0)R0(I)=2.0
       IF(R0(I).LT.3.0)R0(I)=3.0
       IF(R0(I).LT.(1.2*STRPSF(KST)))R0(I)=1.2*STRPSF(KST)
       IF(R0(I).GT.(1.5*STRPSF(KST)))R0(I)=1.5*STRPSF(KST)
c      IF(RMAX.LT.R0(I))RMAX=R0(I)
      write(6,*)'R0,Rf AT EACH DIRECTION ',I,R0(I),RF(I)
      ENDDO
C test for circular domain
c      DO I=1,IT
c         R0(I)=RMAX
cc        R0(I) = RFAVG*1.25
c      write(6,*)'R0,Rf AT EACH DIRECTION ',I,R0(I),RF(I)
c      ENDDO
C
      RETURN 
      END
C
      SUBROUTINE GMOVE(KST,MWAVE,KMAX,IKMAX,IGU,JGU,MAXWV2,MTV,MTV1,
     1            HDATA,DM1,IS1,IFLAG,PSLB,ZDATG,GLON,GLAT,PS2,idvm)

c      SUBROUTINE GMOVE(KST,MWAVE,MAXWV2,MTV,MTV1,HDATA,DM1,
c     1                IS1,IFLAG,PSLB)
c      PARAMETER (IX=41,JX=41,IGU=384,JGU=190)
      PARAMETER (IX=41,JX=41)
      PARAMETER (IT=24,NSG=24000)
C
      integer * 4 idvm
      DIMENSION DMM(IX,JX),DATG(IGU,JGU),DDAT(IGU,JGU)
      DIMENSION ZDATG(IGU,JGU),DM1(IX,JX),T1(IGU,JGU),PSL(IGU,JGU)
      DIMENSION R0(IT),GLAT(IGU,JGU),GLON(IGU,JGU),ING(NSG),JNG(NSG)
      DIMENSION ALAT(JX),ALON(IX)
      COMMON /POSIT/CLON_NEW,CLAT_NEW,SLON,SLAT,CLON,CLAT,RAD
      COMMON /vect/R0,XVECT(IT),YVECT(IT)      
      COMMON /ST/ALON,ALAT
      COMMON /NHC2/MDX,MDY
      COMMON /NHC3/AMDX,AMDY
      COMMON /CHNL/IUT,KSTM
c      COMMON /CT/SLON,SLAT,CLON,CLAT,RAD
c      COMMON /GA/CLON_NEW,CLAT_NEW,R0
      COMMON /TR/ING,JNG,IB
c      COMMON /TR/ZDATG,GLON,GLAT,ING,JNG,IB

      COMMON /HDAT3/NWRT2,NRED2
      REAL HDATA(IGU,JGU,MTV)
      REAL PSLB(IGU,JGU)
      COMMON /CHEN/KUNIT,ITIM

      DIMENSION DATG2(IGU,JGU)
!      REAL(4) SKIP2(MAXWV2,MTV1+IKMAX)

      REAL(4) PS2(MAXWV2)

!      COMMON /COEF5/NCNT,NCNT2
C
C.. SETTING BASIC VARIABLES FOR INTERPOLATING GAUSSIAN GRID
C

      ISE = IS1
      DO I=1,IX
      DO J=1,JX
      DMM(I,J) = DM1(I,J)
      ENDDO
      ENDDO
C
C.. INTERPOLATE TO GAUSSIAN GRID
C
      CALL READ2(IGU,JGU,NRED2,MTV,DATG,HDATA)
c
      DO I=1,IGU
      DO J=1,JGU
        DATG2(I,J)=DATG(I,J)
        DDAT(I,J)=0.
      ENDDO
      ENDDO
C
      RDIST2=AMDX*AMDX+AMDY*AMDY
      IF(RDIST2.GT.0.02)THEN
cc test

!!$OMP PARALLEL DO DEFAULT(SHARED)
!!$OMP+ PRIVATE(I,IW,JW,HLA,HLO,II,JJ,LX,LY,DXX,DYY)
!!$OMP+ PRIVATE(X1,X2,Y1,Y2)
      DO I = 1,IB
      IW = ING(I)
      JW = JNG(I)

c      DO IW = 1, IGU
c      DO JW = 1, JGU
      HLA = GLAT(IW,JW)
      HLO = GLON(IW,JW)
C
      DO II=1,IX-1
       IF(HLO.GT.ALON(II).and.HLO.LE.ALON(II+1))THEN
        DO JJ=1,JX-1
        IF(HLA.GT.ALAT(JJ).and.HLA.LE.ALAT(JJ+1))THEN
          LX=II
          LY=JJ

         DXX = HLO-ALON(LX)
         DYY = HLA-ALAT(LY)
C
         X1 = DMM(LX  ,LY+1)*DYY + DMM(LX  ,LY  )*(1-DYY)
         X2 = DMM(LX+1,LY+1)*DYY + DMM(LX+1,LY  )*(1-DYY)
         Y1 = DMM(LX+1,LY  )*DXX + DMM(LX  ,LY  )*(1-DXX)
         Y2 = DMM(LX+1,LY+1)*DXX + DMM(LX  ,LY+1)*(1-DXX)
         DATG(IW,JW)=(X1*(1-DXX)+X2*DXX + Y1*(1-DYY)+Y2*DYY)/2.

         IF(ISE.GE.2) DDAT(IW,JW)=DATG2(IW,JW)-DATG(IW,JW)
         GO TO 555

        END IF
        END DO
       END IF
      END DO  
 555   CONTINUE
c      ENDDO
c      ENDDO
      ENDDO
!!$OMP END PARALLEL DO
      END IF
c end test

      IF(ISE.EQ.1) THEN
c
c        READ(70) PSL
        PSL=PSLB

        DO I = 1,IB
          IW = ING(I)
          JW = JNG(I)
          DDAT(IW,JW)=PSL(IW,JW)-DATG(IW,JW)
          PSL(IW,JW)=DATG(IW,JW)
        END DO
c
c Move vortex

cc        DO I = 1,IB
cc          IW = ING(I)
cc          JW = JNG(I)
cc          IWX=IW+MDX
cc          JWY=JW+MDY
cc          IF(IWX.GT.IGU)IWX=IWX-IGU
cc          IF(IWX.LT.1)IWX=IWX+IGU
CQLIUC
cc          PSL(IWX,JWY) = PSL(IWX,JWY)+DDAT(IW,JW)
cc        ENDDO

        CALL MOVETX(IGU,JGU,GLON,GLAT,PSL,DDAT)

        PSLB = PSL

        CALL WRIT2(IGU,JGU,NWRT2,MTV,PSL,HDATA)
c
      ELSEIF(ISE.EQ.2) THEN
cyc   REWIND 36
cyc   READ(36) PSL 
       PSL = PSLB
       IF(IFLAG.EQ.1)THEN
        DO I=1,IGU
        DO J=1,JGU
         T1(I,J) = DATG2(I,J)
        ENDDO
        ENDDO
       ELSE
        DO I=1,IGU
        DO J=1,JGU
         T1(I,J) = DATG(I,J)
        ENDDO
        ENDDO
       END IF
        IF(KST.EQ.KSTM)THEN
          CALL SLP2SP(IGU,JGU,ZDATG,KUNIT,MWAVE,MAXWV2,
     &                T1,PSL,PS2,idvm)
        END IF
      END IF

c temperature field
c qliu    
       
      IF(ISE.GE.2.and.ISE.LE.(KMAX+1))then
        IF(IFLAG.EQ.1)THEN
cold          IF(KST.EQ.KSTM) THEN
cql            READ(20)SKIP2
cold            NCNT2 = NCNT2 + 1
cold            WRITE(KUNIT)(SKIP2(NW,NCNT2),NW=1,MAXWV2)
cold          END IF
          CALL WRIT2(IGU,JGU,NWRT2,MTV,DATG2,HDATA)
        ELSE

c Move vortex
cc          DO I = 1,IB
cc            IW = ING(I)
cc            JW = JNG(I)
cc            IWX=IW+MDX
cc            JWY=JW+MDY
cc            IF(IWX.GT.IGU)IWX=IWX-IGU
cc            IF(IWX.LT.1)IWX=IWX+IGU
CQLIUC
cc            DATG(IWX,JWY) = DATG(IWX,JWY)+DDAT(IW,JW)
cc          ENDDO

          CALL MOVETX(IGU,JGU,GLON,GLAT,DATG,DDAT)

cnew          IF(KST.EQ.KSTM) THEN
cql            READ(20)SKIP2
cnew            NCNT2 = NCNT2 + 1
cnew            CALL G2SPC(DATG)
cnew          END IF

          CALL WRIT2(IGU,JGU,NWRT2,MTV,DATG,HDATA)

        END IF
      END IF
C
      IF(ISE.GT.(KMAX+1).and.ISE.LE.(3*KMAX+1))THEN
c Move vortex

cc        DO I = 1,IB
cc          IW = ING(I)
cc          JW = JNG(I)
cc          IWX=IW+MDX
cc          JWY=JW+MDY
cc          IF(IWX.GT.IGU)IWX=IWX-IGU
cc          IF(IWX.LT.1)IWX=IWX+IGU 
CQLIUC
cc          DATG(IWX,JWY) = DATG(IWX,JWY)+DDAT(IW,JW)
cc        ENDDO

         CALL MOVETX(IGU,JGU,GLON,GLAT,DATG,DDAT)
C
cnew        IF(KST.EQ.KSTM) THEN
cnew          CALL G2SPC(DATG)
cnew        END IF

        CALL WRIT2(IGU,JGU,NWRT2,MTV,DATG,HDATA)

      ENDIF

      IF(ISE.GT.(3*KMAX+1))THEN
        IF(IFLAG.EQ.1)THEN
cold          IF(KST.EQ.KSTM) THEN
cold            CALL G2SPC(KUNIT,MWAVE,IGU,JGU,DATG2)
cold          END IF
          CALL WRIT2(IGU,JGU,NWRT2,MTV,DATG2,HDATA)
        ELSE

c Move vortex
cc          DO I = 1,IB
cc            IW = ING(I)
cc            JW = JNG(I)
cc            IWX=IW+MDX
cc            JWY=JW+MDY
cc            IF(IWX.GT.IGU)IWX=IWX-IGU
cc            IF(IWX.LT.1)IWX=IWX+IGU
CQLIUC
cc            DATG(IWX,JWY) = DATG(IWX,JWY)+DDAT(IW,JW)
cc          ENDDO

           CALL MOVETX(IGU,JGU,GLON,GLAT,DATG,DDAT)

cnew          IF(KST.EQ.KSTM) THEN
cnew            CALL G2SPC(DATG)
cnew          END IF

          CALL WRIT2(IGU,JGU,NWRT2,MTV,DATG,HDATA)

        END IF
      ENDIF

C
      RETURN
      END
C
      SUBROUTINE SLP2SP(IGU,JGU,ZDATG,KUNIT,MWAVE,MAXWV2,
     &                  T1,PSL,PS2,idvm)
c      PARAMETER (IGU=384,JGU=190)
C
      integer *4 idvm
      real, parameter :: G=9.8, R=287.05, GAMMA=6.7*0.001
!
      DIMENSION T1(IGU,JGU),PSL(IGU,JGU)
      DIMENSION ZDATG(IGU,JGU)
      REAL(4) PS2(MAXWV2)
c      COMMON /TR/ZDATG,GLON,GLAT,ING,JNG,IB
!
!.. MAKE SFC PRESSURE FROM MSLP
!
!!$OMP PARALLEL DO DEFAULT(SHARED)
!!$OMP+ PRIVATE(JH,IH,PMSL,A,B,C,DD)
      DO JH=1,JGU
        DO IH=1,IGU
          PMSL = LOG(PSL(IH,JH))
          A          = (GAMMA * ZDATG(IH,JH)) / T1(IH,JH)
          B          = LOG(1+A)
          C          = (G*B)/(R*GAMMA)
          DD         = PMSL - C
          PSL(IH,JH) = EXP(DD)/1000.
!         IF (PSL(IH,JH).LE.10.) write(6,*)'SP is Less than 100mb at ',
!    &                               IH,JH,D1
        ENDDO
      ENDDO
!!$OMP END PARALLEL DO
      if (mod(idvm, 10) /= 2) then
        PSL = LOG(PSL)
      endif
!     write(6,*)' in SLP2SP PSL=',PSL(1,90)
C
C.. GAUSSIAN GRID TO SPECTRAL COEFFEICENT
C
      call maxmin(psl,igu*jgu,1,1,1,'global SLP at SLP after int')
      CALL G2SPC(KUNIT,MWAVE,MAXWV2,IGU,JGU,PSL,PS2)
      call maxmin(t1,igu*jgu,1,1,1,'global T1 at SLP after int')
c      CALL G2SPC(KUNIT,T1)
C
      RETURN
      END
C
      SUBROUTINE G2SPC(KUNIT,MWAVE,MAXWV2,IMAX,JMAX,Q1,PS2)
!
      REAL Q1(IMAX,JMAX)
      REAL(4) PS2(MAXWV2)

      REAL,   ALLOCATABLE :: DN(:)

!moor MAXWV2  = (MWAVE+1)*(MWAVE+2)
      MAXWV22 = MAXWV2 + 1

      ALLOCATE ( DN(MAXWV22) )
!
!     call maxmin(dn,MAXWV2,1,1,1,'surface pressure after making')

      call SPTEZ(0,MWAVE,4,IMAX,JMAX,DN,Q1,-1)

      DO I=1,MAXWV2
        PS2(I) = DN(I)
      END DO
!
      DEALLOCATE (DN)

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE MODPR(IM,IX,KM,IDVC,IDSL,SI,AK,BK,PS,PI,PM)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    MODPR       COMPUTE MODEL PRESSURES
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: COMPUTE MODEL PRESSURES.
C
C PROGRAM HISTORY LOG:
C 2001-07-25  MARK IREDELL
C
C USAGE:    CALL MODPR(IM,IX,KM,IDVC,IDSL,SI,AK,BK,PS,PI,PM)
C   INPUT ARGUMENT LIST:
C     IM           INTEGER NUMBER OF POINTS TO COMPUTE
C     IX           INTEGER FIRST DIMENSION
C     KM           INTEGER NUMBER OF LEVELS
C     IDVC         INTEGER VERTICAL COORDINATE ID
C                  (1 FOR SIGMA AND 2 FOR HYBRID)
C     IDSL         INTEGER TYPE OF SIGMA STRUCTURE
C                  (1 FOR PHILLIPS OR 2 FOR MEAN)
C     SI           REAL (KM+1) SIGMA INTERFACE VALUES (IDVC=1)
C     AK           REAL (KM+1) HYBRID INTERFACE A (IDVC=2)
C     BK           REAL (KM+1) HYBRID INTERFACE B (IDVC=2)
C     PS           REAL (IX) SURFACE PRESSURE (PA)
C   OUTPUT ARGUMENT LIST:
C     PI           REAL (IX,KM+1) INTERFACE PRESSURE (PA)
C     PM           REAL (IX,KM) MID-LAYER PRESSURE (PA)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      REAL SI(KM+1),AK(KM+1),BK(KM+1),PS(IX),PI(IX,KM),PM(IX,KM)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(IDVC.EQ.2) THEN
        DO K=1,KM+1
          PI(1:IM,K)=AK(K)+BK(K)*PS
        ENDDO
!      write(6,*)'idvc=',idvc
!      write(6,*)'ak=',ak
!      write(6,*)'bk=',bk
!      write(6,*)'ps(1)=',ps(1)
!      write(6,*)'pi(1,:)=',pi(1,:)
      ELSE
        DO K=1,KM+1
          PI(1:IM,K)=SI(K)*PS
        ENDDO
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(IDSL.EQ.2) THEN
        DO K=1,KM
          PM(1:IM,K)=(PI(1:IM,K)+PI(1:IM,K+1))/2
        ENDDO
      ELSE
        ROCP=287.05/1004.6
        ROCP1=ROCP+1
        ROCPR=1/ROCP
!!$OMP PARALLEL DO DEFAULT(SHARED)
!!$OMP+ PRIVATE(K)
        DO K=1,KM
          PM(1:IM,K)=((PI(1:IM,K)**ROCP1-PI(1:IM,K+1)**ROCP1)/
     &               (ROCP1*(PI(1:IM,K)-PI(1:IM,K+1))))**ROCPR
        ENDDO
!!$OMP END PARALLEL DO
!      write(6,*)'pm(1,:)=',pm(1,:)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
