!
!   Author: Suranjana Saha
C-----------------------------------------------------------------------
C  MAIN PROGRAM SURUFIT
C-----------------------------------------------------------------------
      PROGRAM SURUFIT
 
      PARAMETER (IDBUG=0,IPR=1)
      PARAMETER (NSTC=9)
      PARAMETER (NPLV=21)
      PARAMETER (NVAR=5)
      PARAMETER (NREG=7)
      PARAMETER (NSUB=1)
      PARAMETER (NBAK=2)
 
      CHARACTER*80 HDSTR,OBSTR,FCSTR,ANSTR,QMSTR,PSTR
      CHARACTER*8  SUBSET
c
       real(8)    HDR(14)
       real(8)    PSOB(4),PSPR(4)
       real(8)    BAK(10,255,NBAK)
       real(8)    OBS(10,255),QMS(10,255)
c
      DIMENSION    SPRS(NSTC,NPLV,NVAR,NREG,NSUB,NBAK)
      DIMENSION    PMAND(NPLV),PMID(NPLV),LEVP(1200)
      DIMENSION    FCS(10,255),ANS(10,255)
      DIMENSION    STC(NSTC,5,NBAK)
      DIMENSION    GDATA(NREG,NSUB)
      LOGICAL      MANDONLY,REGION
      INTEGER      INDEXV(NVAR)
 
      DATA PMAND / 1000, 925, 850, 700, 500, 400, 300,
     .              250, 200, 150, 100,  70,  50,  30,
     .               20,  10,   7,   5,   3,   2,   1/
 
      DATA HDSTR
     ./'SID XOB YOB DHR ELV TYP T29 ITP SQN RQM DUP PRG SRC RUD'/
      DATA PSTR /'POB PAN PFC PQM CAT=0'/
      DATA OBSTR/'POB QOB TOB ZOB UOB VOB'/
      DATA FCSTR/'PFC QFC TFC ZFC UFC VFC'/
      DATA ANSTR/'PAN QAN TAN ZAN UAN VAN'/
      DATA QMSTR/'PQM QQM TQM ZQM WQM CAT'/
 
      DATA BMISS /  10E10 /
      DATA RMISS / -9.99E+33 /
      DATA LUBFR/11/
      data indexv/3,4,5,2,1/
c...  t,z,w,q,ps
c
       CALL OPENBF(LUBFR,'IN ',LUBFR)
c
C-----------------------------------------------------------------------
 
C  ZERO THE FIT ARRAYS
C  -------------------
 
      SPRS = 0.
C  --------------------------------------
 
      MANDONLY = .FALSE.
C     MANDONLY = .TRUE.
 
      DO I=1,1200
      LEVP(I) = 0
      ENDDO
 
      IF(.NOT.MANDONLY) THEN
         DO L=1,NPLV
         PD = PMAND(L)
         IF(L.LT.NPLV) PU = PMAND(L+1)
         IF(L.EQ.NPLV) PU = -PMAND(L)
         PMID(L) = .5*(PD+PU)
         ENDDO
 
         DO 1 I=1,1200
         DO L=1,NPLV
         IF(FLOAT(I).GT.PMID(L)) THEN
            LEVP(I) = L
            GO TO 1
         ENDIF
         ENDDO
1        ENDDO
      ELSE
         DO I=1,1200
         DO L=1,NPLV
         IF(FLOAT(I).EQ.PMAND(L)) LEVP(I) = L
         ENDDO
         ENDDO
      ENDIF
 
C  READ AND "SURU-FIT" THE PREPDA/BUFR RECORDS
C  -------------------------------------------
 
10    DO WHILE(IREADMG(LUBFR,SUBSET,IDATE).EQ.0)
c... check for subset...
      IF(ITYP(SUBSET).EQ.0) GO TO 10
11    DO WHILE(IREADSB(LUBFR).EQ.0)

c... check for raobs only...
      CALL UFBINT(LUBFR,HDR,14,  1,NLEV,HDSTR)
      if(subset.eq.'ADPUPA') then
        typx=hdr(6)
        if((typx.eq.220.).or.(typx.eq.120.)) then
          GO TO 1335
        else
          GO TO 11
        endif
      endif

1335  continue

c... check for region...
      XOB=hdr(2)
      YOB=hdr(3)
c
      IF(.NOT.REGION(XOB,YOB,0)) GO TO 11
 
C  READ THE DATA
C  -------------
      STC=0.
c... first surface pressure...
       do nn=1,4
       psob(nn)=bmiss
       enddo
       CALL UFBINT(LUBFR,PSOB, 4,1,NLEV,PSTR)
       if(nlev.eq.1) then
c
       pso =  psob(1)
       psa =  psob(2)
       psf =  psob(3)
       psq =  psob(4)
         IF(psq.LE.3 .and. psa.lt.1E10) THEN
c
            STC(1,1,1) = 1.
            STC(2,1,1) = psf
            STC(3,1,1) = pso
            STC(4,1,1) = psf*pso
            STC(5,1,1) = psf*psf
            STC(6,1,1) = pso*pso
c
            STC(1,1,2) = 1.
            STC(2,1,2) = psa
            STC(3,1,2) = pso
            STC(4,1,2) = psa*pso
            STC(5,1,2) = psa*psa
            STC(6,1,2) = pso*pso
c
c... j  is level (21)
c... k  is variable (5)
c... ll is region (7)
c... m  is subset (1)
c... n  is background (2)
c
         J=1
         K=1
         M = ITYP(SUBSET)
c
         if (M > 0) then
         DO N=1   ,NBAK
         DO LL=1  ,NREG
C
         IF(REGION(XOB,YOB,LL)) THEN
C
            cnto = SPRS(1,J,K,LL,M,N) 
            SPRS(1,J,K,LL,M,N) = SPRS(1,J,K,LL,M,N) + STC(1,K,N)
            cntn = SPRS(1,J,K,LL,M,N) 

            if(cntn.gt.cnto) then
            wt1 = cnto/cntn
            wt2 = 1.-wt1
c
            DO I=2,6
c
            sprso = SPRS(I,J,K,LL,M,N)
            rat1 = wt1*SPRS(I,J,K,LL,M,N)
            rat2 = wt2*STC(I,K,N)
c
            SPRS(I,J,K,LL,M,N) = rat1 + rat2
            sprsn = SPRS(I,J,K,LL,M,N)
c
c           if((i.eq.2).and.(n.eq.1)) then
c           write(6,3000) ll,cnto,cntn,wt1,wt2,sprso,STC(I,K,N),
c    *      rat1,rat2,sprsn
c           endif
c
c... end stat-loop
            ENDDO
c
            endif

         ENDIF
c... end region-loop
         ENDDO
C
c... end forecast-loop
         ENDDO
         endif     ! if itype > 0
C
       endif
       endif
c
c... now do the rest of the variables....
c
C  GENERATE A PRESSURE LEVEL LOOKUP TABLE
      CALL UFBINT(LUBFR,OBS,10,255,NLEV,OBSTR)
      CALL UFBINT(LUBFR,BAK(1,1,1),10,255,NLFC,FCSTR)
      CALL UFBINT(LUBFR,BAK(1,1,2),10,255,NLAN,ANSTR)
      CALL UFBINT(LUBFR,QMS,10,255,NLQM,QMSTR)

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
 
c... start level-loop
      DO L=1,NLEV
      STC = 0.

      qms(4,l) = max(qms(3,l),qms(4,l)) ! use tqm for zqm

      POB = OBS(1,L)
      PQM = QMS(1,L)
      CAT = QMS(6,L)

      if(idbug.eq.1) print *,' pob ',pob,' pqm ',pqm,' cat ',cat

      IF(PQM.LE.3) THEN
      J=LEVP(NINT(POB))
      IF(J.GT.0) THEN
C
c... start forecast-loop
         DO IB=1,2
C
c... start variable-loop
         DO IQ=2,5
c
         IF(OBS(IQ,L)   >=BMISS) GO TO 2234 ! protect from missing observation !
         IF(BAK(IQ,L,IB)>=BMISS) GO TO 2234 ! protect from missing background  !
         IR = IQ+1
C
         IF(QMS(IQ,L).LE.3 .AND. IQ.LT.5 .AND. CAT.NE.4) THEN
c
            IF(IQ.EQ.2) THEN
               IF(IB.EQ.1) OBS(IQ,L)=OBS(IQ,L)*1.E-3
               BAK(IQ,L,IB)=BAK(IQ,L,IB)*1.E-3
            ENDIF
c  count
            STC(1,IQ,IB) = 1.
c  f
            STC(2,IQ,IB) = BAK(IQ,L,IB)
c  o
            STC(3,IQ,IB) = OBS(IQ,L)
c  f * o
            STC(4,IQ,IB) = BAK(IQ,L,IB)*OBS(IQ,L)
c  f**2
            STC(5,IQ,IB) = BAK(IQ,L,IB)**2
c  o**2
            STC(6,IQ,IB) = OBS(IQ,L)**2
C
         ELSEIF(QMS(IQ,L).LE.3 .AND. IQ.EQ.5) THEN
c  count
            STC(1,IQ,IB) = 1.
c  uf
            STC(2,IQ,IB) = BAK(IQ,L,IB)
c  vf
            STC(3,IQ,IB) = BAK(IR,L,IB)
c  uo
            STC(4,IQ,IB) = OBS(IQ,L)
c  vo
            STC(5,IQ,IB) = OBS(IR,L)
c  uf*uo + vf*vo
            STC(6,IQ,IB) = BAK(IQ,L,IB)*OBS(IQ,L)+BAK(IR,L,IB)*OBS(IR,L)
c  uf**2 + vf**2
            STC(7,IQ,IB) = BAK(IQ,L,IB)**2 + BAK(IR,L,IB)**2
c  uo**2 + vo**2
            STC(8,IQ,IB) = OBS(IQ,L)**2 + OBS(IR,L)**2
c  sqrt (uf**2 + vf**2) - sqrt (uo**2 + vo**2)
            STC(9,IQ,IB) = SQRT(STC(7,IQ,IB)) - SQRT(STC(8,IQ,IB))
C
         ENDIF
C
         if(idbug.eq.1) then
         if(j.eq.5) print *,' ib ',ib,' iq ',iq,' stc ',
     *              (stc(k,iq,ib),k=1,3)
         endif
c
 2234    continue
c
c... end variable-loop
         ENDDO
c... end forecast-loop
         ENDDO
 
         M = ITYP(SUBSET)

         if (M > 0) then
         DO N=1   ,NBAK
         DO LL=1  ,NREG
C
         IF(REGION(XOB,YOB,LL)) THEN
C
            DO K=2,NVAR
            nstat=6
            if(k.eq.5) nstat=9

            cnto = SPRS(1,J,K,LL,M,N) 
            SPRS(1,J,K,LL,M,N) = SPRS(1,J,K,LL,M,N) + STC(1,K,N)
            cntn = SPRS(1,J,K,LL,M,N) 

            if(cntn.gt.cnto) then
            wt1 = cnto/cntn
            wt2 = 1.-wt1
c
            DO I=2,nstat
c
            sprso = SPRS(I,J,K,LL,M,N)
            rat1 = wt1*SPRS(I,J,K,LL,M,N)
            rat2 = wt2*STC(I,K,N)
c
            SPRS(I,J,K,LL,M,N) = rat1 + rat2
            sprsn = SPRS(I,J,K,LL,M,N)
c
            if((idbug.eq.1).and.(i.eq.2)) then
            if((j.eq.5).and.(k.eq.2)) then
            if((m.eq.1).and.(n.eq.1)) then
            write(6,3000) ll,cnto,cntn,wt1,wt2,sprso,STC(I,K,N),
     *      rat1,rat2,sprsn
            endif
            endif
            endif
c
c... end stat-loop
            ENDDO
c
            endif

c... end variable-loop
            ENDDO
C
         ENDIF
c... end region-loop
         ENDDO
C
c... end forecast-loop
         ENDDO
         endif     ! if itype > 0
C
c...  only if correct qc flag
      ENDIF
c...  only if correct level
      ENDIF
c... end level-loop
      ENDDO
 
      ENDDO
c... end subset-loop
      ENDDO
 
       CALL CLOSBF(LUBFR)

C  FINISH UP
C  ---------
C   write out grads data file...
 
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
      if(ipr.eq.1) then
      if(ibak.eq.1) then
      if((iplv.eq.1).and.(ivar.eq.1))
     *write(6,1231) ibak,nst,ilev,(gdata(ireg,1),ireg=1,nreg)
      if(iplv.eq.5) then
      ilev=pmand(iplv)
      if(ivar.eq.2)
     *write(6,1232) ibak,nst,ilev,(gdata(ireg,1),ireg=1,nreg)
      if(ivar.eq.3)
     *write(6,1233) ibak,nst,ilev,(gdata(ireg,1),ireg=1,nreg)
      if(ivar.eq.4)
     *write(6,1234) ibak,nst,ilev,(gdata(ireg,1),ireg=1,nreg)
      if(ivar.eq.5)
     *write(6,1235) ibak,nst,ilev,(gdata(ireg,1),ireg=1,nreg)
      endif
      endif
      endif
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
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER FUNCTION ITYP(SUBSET)
 
      PARAMETER(NSUB=1)
 
      CHARACTER*8 SUBSET,SUBTYP(1)
      DATA SUBTYP /'ADPUPA'/
C
C     CHARACTER*8 SUBSET,SUBTYP(15)
C     DATA SUBTYP /'ADPUPA','AIRCAR','AIRCFT','SATWND','PROFLR',
C    .             'VADWND','SATBOG','SATEMP','ADPSFC','SFCSHP',
C    .             'SFCBOG','SPSSMI','SYNDAT','ERS1DA','GOESND'/
 
C  LOOK FOR A MATCH TO RETURN NON-ZERO
C  -----------------------------------
 
      DO I=1,NSUB
      ITYP = I
      IF(SUBSET.EQ.SUBTYP(I)) RETURN
      ENDDO
 
C  IF NO MATCH, RETURN ZERO
C  ------------------------
 
      ITYP = 0
      RETURN
      END
