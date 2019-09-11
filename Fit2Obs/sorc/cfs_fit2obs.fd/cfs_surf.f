C-----------------------------------------------------------------------
C  MAIN PROGRAM SURUFIT
C-----------------------------------------------------------------------
      PROGRAM SURUFIT

      PARAMETER (IDBUG=0,IPR=1)
      PARAMETER (NSTC=9)
      PARAMETER (NPLV=1)
      PARAMETER (NVAR=5)
      PARAMETER (NREG=7)
      PARAMETER (NSUB=2)
      PARAMETER (NBAK=2)

      CHARACTER*80 HDSTR,OBSTR,FCSTR,ANSTR,QMSTR,VMSTR,PSTR
      CHARACTER*8  SUBSET

      real(8)    HDR(14)
      real(8)    PSOB(4),PSPR(4)
      real(8)    BAK(10,255,NBAK)
      real(8)    OBS(10,255),QMS(10,255),vms(10,255)


      real(8)    SPRS(NSTC,NPLV,NVAR,NREG,NSUB,NBAK)
      real(8)    CNTO,CNTN,RAT1,RAT2,WT1,WT2
      real(8)    STC(NSTC,5,NBAK)

      real(4)    GDATA(NREG,NSUB)

      LOGICAL    MANDONLY,REGION
      INTEGER    INDEXV(NVAR)

!     DATA PMAND / 1000, 925, 850, 700, 500, 400, 300,
!    .              250, 200, 150, 100,  70,  50,  30,
!    .               20,  10,   7,   5,   3,   2,   1/

      DATA HDSTR
     ./'SID XOB YOB DHR ELV TYP T29 ITP SQN RQM DUP PRG SRC RUD'/
      DATA PSTR /'POB PAN PFC PQM CAT=0'/
      DATA OBSTR/'POB QOB TOB ZOB UOB VOB'/
      DATA FCSTR/'PFC QFC TFC ZFC UFC VFC'/
      DATA ANSTR/'PAN QAN TAN ZAN UAN VAN'/
      DATA QMSTR/'PQM QQM TQM ZQM WQM CAT'/
      DATA VMSTR/'PQM QVWTA TVWTA ZQM WVWTA CAT'/

      real(8) BMISS /10E10/

      DATA RMISS / -9.99E+33 /
      DATA LUBFR/11/
      data indexv/3,4,5,2,1/
c...  t,z,w,q,ps
c
       CALL OPENBF(LUBFR,'IN ',LUBFR)
c
C-----------------------------------------------------------------------
c... j  is level (21)
c... k  is variable (5)
c... ll is region (7)
c... m  is subset (1)
c... n  is background (2)
C-----------------------------------------------------------------------

C  ZERO THE FIT ARRAYS
C  -------------------

      SPRS = 0.

      bmiss=10e10; call setbmiss(bmiss) ! this sets bufrlib missing value to 10e10

C  READ AND "SURU-FIT" THE PREPDA/BUFR RECORDS
C  -------------------------------------------

10    DO WHILE(IREADMG(LUBFR,SUBSET,IDATE).EQ.0)
      IF(SUBSET/='ADPSFC'.AND.SUBSET/='SFCSHP') cycle
11    DO WHILE(IREADSB(LUBFR).EQ.0)
      CALL UFBINT(LUBFR,HDR,14,1,NLEV,HDSTR)

c... check for report type 
      kx=mod(nint(hdr(6)),100)
      if(kx==80)then      !ships
         itype=2
      elseif(kx==81.or.kx==87)then  !synops
         itype=1
      else
         cycle
      endif

c... check for region...
      XOB=hdr(2)
      YOB=hdr(3)
      IF(.NOT.REGION(XOB,YOB,0)) GO TO 11

c... set parameters
      STC=0; M=ITYPE

c... first surface pressure...
      CALL UFBINT(LUBFR,PSOB,4,1,NLEV,PSTR)
      pso =  psob(1)-984.
      psa =  psob(2)-984.
      psf =  psob(3)-984.
      psq =  psob(4)
      IF(psq.LE.3 .and. psa.lt.1E10) THEN
         STC(1,1,1) = 1.
         STC(2,1,1) = psf
         STC(3,1,1) = pso
         STC(4,1,1) = psf*pso
         STC(5,1,1) = psf*psf
         STC(6,1,1) = pso*pso
         STC(1,1,2) = 1.
         STC(2,1,2) = psa
         STC(3,1,2) = pso
         STC(4,1,2) = psa*pso
         STC(5,1,2) = psa*psa
         STC(6,1,2) = pso*pso
         J=1
         K=1
         DO N=1   ,NBAK
         DO LL=1  ,NREG
         IF(REGION(XOB,YOB,LL)) THEN
            cnto = SPRS(1,J,K,LL,M,N)
            SPRS(1,J,K,LL,M,N) = SPRS(1,J,K,LL,M,N) + STC(1,K,N)
            cntn = SPRS(1,J,K,LL,M,N)
            if(cntn.gt.cnto) then
               wt1 = cnto/cntn
               wt2 = 1.-wt1
               DO I=2,6
               sprso = SPRS(I,J,K,LL,M,N)
               rat1 = wt1*SPRS(I,J,K,LL,M,N)
               rat2 = wt2*STC(I,K,N)
               SPRS(I,J,K,LL,M,N) = rat1 + rat2
               sprsn = SPRS(I,J,K,LL,M,N)
               ENDDO
            endif
         ENDIF
         ENDDO
         ENDDO
       endif

c... now do the rest of the variables....

      CALL UFBINT(LUBFR,OBS,10,255,NLEV,OBSTR)
      CALL UFBINT(LUBFR,BAK(1,1,1),10,255,NLFC,FCSTR)
      CALL UFBINT(LUBFR,BAK(1,1,2),10,255,NLAN,ANSTR)
      CALL UFBINT(LUBFR,QMS,10,255,NLQM,QMSTR)
      CALL UFBINT(LUBFR,VMS,10,255,NLQM,VMSTR)

      if(subset=='ADPSFC') then
         do i=2,5 ! set qm based on varqc marks
         if(qms(i,1)==8.or.qms(i,1)>9) cycle
         if(vms(i,1)>79.and.vms(i,1)<101) qms(i,1)=1
         enddo
      endif

      IF(NLEV.NE.NLQM) THEN
      PRINT *,'***SURUFIT***QM LEVELS NE OB LEVELS'
      CALL ABORT
      ENDIF
      IF(NLEV.NE.NLFC) THEN
      PRINT *,'***SURUFIT***FC LEVELS NE OB LEVELS'
      CALL ABORT
      ENDIF
      IF(NLEV.NE.NLAN) THEN
      PRINT *,'***SURUFIT***AN LEVELS NE OB LEVELS'
      CALL ABORT
      ENDIF

C  CREATE AND ACCUMULATE THE STATISTICS ARRAY FOR EACH REALIZATION
C  ---------------------------------------------------------------

      DO L=1,1 !!NLEV
      STC = 0.

      qms(4,l) = max(qms(3,l),qms(4,l)) ! use tqm for zqm
      POB = OBS(1,L); PQM = QMS(1,L); if(pqm>3) cycle
      CAT = QMS(6,L)
      if(idbug.eq.1) print *,' pob ',pob,' pqm ',pqm,' cat ',cat

      DO IB=1,2  ! background field
      DO IQ=2,5  ! vars q,t,z,w        
      IF(OBS(IQ,L)   >=BMISS) cycle ! protect from missing observation !
      IF(BAK(IQ,L,IB)>=BMISS) cycle ! protect from missing background  !
      IR = IQ+1
      IF(QMS(IQ,L).LE.3 .AND. IQ.LT.5 .AND. CAT.NE.4) THEN
         IF(IQ.EQ.2) THEN
            IF(IB.EQ.1) OBS(IQ,L)=OBS(IQ,L)*1.E-3
            BAK(IQ,L,IB)=BAK(IQ,L,IB)*1.E-3
         ENDIF
         STC(1,IQ,IB) = 1                            ! count
         STC(2,IQ,IB) = BAK(IQ,L,IB)                 ! background
         STC(3,IQ,IB) = OBS(IQ,L)                    ! observation
         STC(4,IQ,IB) = BAK(IQ,L,IB)*OBS(IQ,L)       ! f*o
         STC(5,IQ,IB) = BAK(IQ,L,IB)**2              ! f**2
         STC(6,IQ,IB) = OBS(IQ,L)**2                 ! o**2
      ELSEIF(QMS(IQ,L).LE.3 .AND. IQ.EQ.5) THEN
         STC(1,IQ,IB) = 1                                              ! count
         STC(2,IQ,IB) = BAK(IQ,L,IB)                                   ! u background
         STC(3,IQ,IB) = BAK(IR,L,IB)                                   ! v background
         STC(4,IQ,IB) = OBS(IQ,L)                                      ! u observation
         STC(5,IQ,IB) = OBS(IR,L)                                      ! v observation
         STC(6,IQ,IB) = BAK(IQ,L,IB)*OBS(IQ,L)+BAK(IR,L,IB)*OBS(IR,L)  ! uf*uo + vf*vo
         STC(7,IQ,IB) = BAK(IQ,L,IB)**2 + BAK(IR,L,IB)**2              ! uf**2 + vf**2
         STC(8,IQ,IB) = OBS(IQ,L)**2 + OBS(IR,L)**2                    ! uo**2 + vo**2
         STC(9,IQ,IB) = SQRT(STC(7,IQ,IB)) - SQRT(STC(8,IQ,IB))        ! sqrt(uf**2+vf**2)-sqrt(uo**2+vo**2)
      endif
      ENDDO
      ENDDO

!  store the stats from this ob

      DO N=1   ,NBAK
      DO LL=1  ,NREG
      IF(REGION(XOB,YOB,LL)) THEN
         DO K=2,NVAR
         nstat=6
         if(k.eq.5) nstat=9
         cnto = SPRS(1,L,K,LL,M,N)
         SPRS(1,L,K,LL,M,N) = SPRS(1,L,K,LL,M,N) + STC(1,K,N)
         cntn = SPRS(1,L,K,LL,M,N)
         if(cntn.gt.cnto) then
            wt1 = cnto/cntn
            wt2 = 1.-wt1
            DO I=2,nstat
            sprso = SPRS(I,L,K,LL,M,N)
            rat1 = wt1*SPRS(I,L,K,LL,M,N)
            rat2 = wt2*STC(I,K,N)
            SPRS(I,L,K,LL,M,N) = rat1 + rat2
            sprsn = SPRS(I,L,K,LL,M,N)
            ENDDO
         endif
         ENDDO
      ENDIF
! end region-backg-level-loop
      ENDDO
      ENDDO
      ENDDO
! end ireadsb-ireadmg-loop
      ENDDO
      ENDDO
      CALL CLOSBF(LUBFR)

! FINISH UP - write out grads data file...
! ----------------------------------------

      iw=50
      do ibak=1,nbak
      iw=iw+1
c
      do ivarx=1,nvar
      ivar=indexv(ivarx)
      nstat=6
      if(ivar.eq.5) nstat=9
      if(idbug.eq.1) print *,ibak,' ivar ',ivar,' nstat ',nstat
c
      do nst=1,nstat
c
      nlx=nplv
      if(ivar.eq.1) nlx=1
      do iplv=1,nlx
c
      do isub=1,nsub
      do ireg=1,nreg
c
      gdata(ireg,isub)=sprs(nst,iplv,ivar,ireg,isub,ibak)
c
c... end region-loop
      enddo
c... end subset-loop
      enddo
c
      write(iw) gdata
c
c... end level-loop
      enddo
c... end stat-loop
      enddo
c
c... end variable-loop
      enddo
c
      close(iw)
c... end forecast-loop
      enddo
c
 1231  format('p fcs=',i2,2x,'stat = ',i2,2x,'lev = ',i6,2x,7f12.2)
 1232  format('q fcs=',i2,2x,'stat = ',i2,2x,'lev = ',i6,2x,7f12.2)
 1233  format('t fcs=',i2,2x,'stat = ',i2,2x,'lev = ',i6,2x,7f12.2)
 1234  format('z fcs=',i2,2x,'stat = ',i2,2x,'lev = ',i6,2x,7f12.2)
 1235  format('w fcs=',i2,2x,'stat = ',i2,2x,'lev = ',i6,2x,7f12.2)
c
 3000  format('reg ',i2,' cnto ',f4.0,' cntn ',f4.0,
     *        ' wt1 ',f5.2,' wt2 ',f5.2,2x,5f12.2)
c
      PRINT'("SURUFIT PROCESSING COMPLETED")'
      STOP
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      LOGICAL FUNCTION REGION(X,Y,I)

      PARAMETER(NREG=7)

      DIMENSION X1(NREG),X2(NREG),Y1(NREG),Y2(NREG)

C.... X goes from east to west
C.... Y goes from south to north
c... global,nh,sh,tr,na,eu,as
      DATA X1(1),X2(1),Y1(1),Y2(1) /0.,360.,-90.,90./
      DATA X1(2),X2(2),Y1(2),Y2(2) /0.,360.,20.,80./
      DATA X1(3),X2(3),Y1(3),Y2(3) /0.,360.,-80.,-20./
      DATA X1(4),X2(4),Y1(4),Y2(4) /0.,360.,-20.,20./
      DATA X1(5),X2(5),Y1(5),Y2(5) /235.,295.,25.,55./
      DATA X1(6),X2(6),Y1(6),Y2(6) /350.,25.,35.,70./
      DATA X1(7),X2(7),Y1(7),Y2(7) /65.,145.,5.,45./

C-----------------------------------------------------------------------

C  CHECK FOR A VALID REGION INDEX AND BE OPTIMISTIC
C  ------------------------------------------------

      IF(I.LT.0.OR.I.GT.NREG) THEN
         REGION = .FALSE.
         RETURN
      ELSE
         REGION = .TRUE.
      ENDIF

C  SETUP THE SEARCH PARAMETERS
C  ---------------------------

      IF(I.EQ.0) THEN
         I1 = 1
         I2 = NREG
      ELSE
         I1 = I
         I2 = I
      ENDIF

C  LOOK FOR A REGION MATCH
C  -----------------------

      DO I0=I1,I2
      IF(Y.GE.Y1(I0) .AND. Y.LE.Y2(I0)) THEN
         IF(X1(I0).LE.X2(I0)) THEN
            IF(X.GE.X1(I0) .AND. X.LE.X2(I0)) RETURN
         ELSEIF(X1(I0).GT.X2(I0)) THEN
            IF(X.GE.X1(I0) .OR.  X.LE.X2(I0)) RETURN
         ENDIF
      ENDIF
      ENDDO

C  IF NO MATCH, RETURN FALSE
C  -------------------------

      REGION = .FALSE.
      RETURN
      END
