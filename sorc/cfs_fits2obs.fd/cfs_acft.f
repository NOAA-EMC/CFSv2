C-----------------------------------------------------------------------
C  MAIN PROGRAM SURUFIT
!   Author: Suranjana Saha
C-----------------------------------------------------------------------
c   make sure when you change levels, you check pmandt and pmandb
C-----------------------------------------------------------------------
      PROGRAM SURUFIT
c
      PARAMETER (IDBUG=0,IPR=1)
      PARAMETER (NSTC=9)
      PARAMETER (NPLV=3)
      PARAMETER (NVAR=4)
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
      DIMENSION    PMANDB(NPLV),PMANDT(NPLV)
      DIMENSION    FCS(10,255),ANS(10,255)
      DIMENSION    STC(NSTC,5,NBAK)
      DIMENSION    GDATA(NREG,NSUB)
      LOGICAL      MANDONLY,REGION
      INTEGER      INDEXV(NVAR)
c
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
      data indexv/2,3,4,1/
c...  t,z,w,q
c
      DATA PMANDB / 1000,700,300/
      DATA PMANDT /  700,300,150/
c
      levt1=pmandt(1)
      levb1=pmandb(1)
      levt2=pmandt(2)
      levb2=pmandb(2)
      levt3=pmandt(3)
      levb3=pmandb(3)
c
       CALL OPENBF(LUBFR,'IN ',LUBFR)
c
C-----------------------------------------------------------------------
 
C  ZERO THE FIT ARRAYS
C  -------------------
 
      SPRS = 0.
C  --------------------------------------
 
C  READ AND "SURU-FIT" THE PREPDA/BUFR RECORDS
C  -------------------------------------------
 
10    DO WHILE(IREADMG(LUBFR,SUBSET,IDATE).EQ.0)
c... check for subset...
      IF(ITYP(SUBSET).EQ.0) GOTO 10
11    DO WHILE(IREADSB(LUBFR).EQ.0)

c... check for aircraft only...
      CALL UFBINT(LUBFR,HDR,14,  1,NLEV,HDSTR)
c
c... check for region...
      XOB=hdr(2)
      YOB=hdr(3)
c
      IF(.NOT.REGION(XOB,YOB,0)) GOTO 11
 
C  READ THE DATA
C  -------------
C  GENERATE A PRESSURE LEVEL LOOKUP TABLE
      CALL UFBINT(LUBFR,OBS,10,255,NLEV,OBSTR)
      CALL UFBINT(LUBFR,BAK(1,1,1),10,255,NLFC,FCSTR)
      CALL UFBINT(LUBFR,BAK(1,1,2),10,255,NLAN,ANSTR)
      CALL UFBINT(LUBFR,QMS,10,255,NLQM,QMSTR)
c

C  CREATE AND ACCUMULATE THE STATISTICS ARRAY FOR EACH REALIZATION
C  ---------------------------------------------------------------
 
c... j  is level (21)
c... k  is variable (5)
c... ll is region (7)
c... m  is subset (1)
c... n  is background (2)
c
      POB = OBS(1,1)
      PQM = QMS(1,1)
      CAT = QMS(6,1)
C
      if(idbug.eq.1) print *,' pob ',pob,' pqm ',pqm,' cat ',cat
c
      if(pqm.le.3) then
      j=0
      if((pob.le.levb1).and.(pob.gt.levt1)) j=1
      if((pob.le.levb2).and.(pob.gt.levt2)) j=2
      if((pob.le.levb3).and.(pob.gt.levt3)) j=3

      if(j.gt.0) then
c
      STC = 0.
c... start forecast-loop
         DO IB=1,2
C
c... start variable-loop
         DO IQ=1,4
c
         IQQ = IQ+1
         IR = IQQ+1
C
         IF(QMS(IQQ,1).LE.3 .AND. IQ.LE.3 .AND. CAT.NE.4) THEN
c
            IF(IQ.EQ.1) THEN
            IF(OBS(IQQ,1).EQ.BMISS) GO TO 2234
            IF(IB.EQ.1) OBS(IQQ,1)=OBS(IQQ,1)*1.E-3
            IF(BAK(IQQ,1,IB).EQ.BMISS) GO TO 2234
            BAK(IQQ,1,IB)=BAK(IQQ,1,IB)*1.E-3
            ENDIF
c  count
            STC(1,IQ,IB) = 1.
c  f
            STC(2,IQ,IB) = BAK(IQQ,1,IB)
c  o
            STC(3,IQ,IB) = OBS(IQQ,1)
c  f * o
            STC(4,IQ,IB) = BAK(IQQ,1,IB)*OBS(IQQ,1)
c  f**2
            STC(5,IQ,IB) = BAK(IQQ,1,IB)**2
c  o**2
            STC(6,IQ,IB) = OBS(IQQ,1)**2
C
         ELSEIF(QMS(IQQ,1).LE.3 .AND. IQ.EQ.4) THEN
c  count
            STC(1,IQ,IB) = 1.
c  uf
            STC(2,IQ,IB) = BAK(IQQ,1,IB)
c  vf
            STC(3,IQ,IB) = BAK(IR,1,IB)
c  uo
            STC(4,IQ,IB) = OBS(IQQ,1)
c  vo
            STC(5,IQ,IB) = OBS(IR,1)
c  uf*uo + vf*vo
            STC(6,IQ,IB)=BAK(IQQ,1,IB)*OBS(IQQ,1)+BAK(IR,1,IB)*OBS(IR,1)
c  uf**2 + vf**2
            STC(7,IQ,IB) = BAK(IQQ,1,IB)**2 + BAK(IR,1,IB)**2
c  uo**2 + vo**2
            STC(8,IQ,IB) = OBS(IQQ,1)**2 + OBS(IR,1)**2
c  sqrt (uf**2 + vf**2) - sqrt (uo**2 + vo**2)
            STC(9,IQ,IB) = SQRT(STC(7,IQ,IB)) - SQRT(STC(8,IQ,IB))
C
         ENDIF
C
         if(idbug.eq.1) then
         if(j.eq.3) print *,' ib ',ib,' iq ',iq,' stc ',
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
 
         if (m > 0) then
         DO N=1   ,NBAK
         DO LL=1  ,NREG
C
         IF(REGION(XOB,YOB,LL)) THEN
C
            DO K=1,NVAR
            nstat=6
            if(k.eq.nvar) nstat=9

c        print *,'j is ',j,' k is ',k,' ll is ',ll,' m is ',m,' n is ',n
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
            if((j.eq.4).and.(k.eq.2)) then
            if((m.eq.1).and.(n.eq.1)) then
            write(6,3000) ll,cnto,cntn,wt1,wt2,sprso,STC(I,K,N),
     *      rat1,rat2,sprsn
            endif
            endif
            endif
c
            ENDDO          ! ... end stat-loop
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
      if(ivar.eq.nvar) nstat=9
      if(idbug.eq.1) print *,ibak,' ivar ',ivar,' nstat ',nstat
c
      do nst=1,nstat
c
      do iplv=1,nplv
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
      if(iplv.eq.1) then
      ilevt=pmandt(iplv)
      ilevb=pmandb(iplv)
      if(ivar.eq.1)
     *write(6,1231) ibak,nst,ilevt,ilevb,(gdata(ireg,1),ireg=1,nreg)
      if(ivar.eq.2)
     *write(6,1232) ibak,nst,ilevt,ilevb,(gdata(ireg,1),ireg=1,nreg)
      if(ivar.eq.3)
     *write(6,1233) ibak,nst,ilevt,ilevb,(gdata(ireg,1),ireg=1,nreg)
      if(ivar.eq.4)
     *write(6,1234) ibak,nst,ilevt,ilevb,(gdata(ireg,1),ireg=1,nreg)
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
 1231  format('q fcs=',i2,2x,'stat = ',i2,2x,'lev = ',2i6,2x,7f12.2)
 1232  format('t fcs=',i2,2x,'stat = ',i2,2x,'lev = ',2i6,2x,7f12.2)
 1233  format('z fcs=',i2,2x,'stat = ',i2,2x,'lev = ',2i6,2x,7f12.2)
 1234  format('w fcs=',i2,2x,'stat = ',i2,2x,'lev = ',2i6,2x,7f12.2)
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
      DATA SUBTYP /'AIRCFT'/
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
