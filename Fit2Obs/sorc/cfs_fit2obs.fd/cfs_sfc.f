!
!   Author: Suranjana Saha
C-----------------------------------------------------------------------
C  MAIN PROGRAM SURUFIT
C-----------------------------------------------------------------------
      PROGRAM SURUFIT
 
      PARAMETER (IDBUG=0,IPR=1)
      PARAMETER (FAC=984.0)
      PARAMETER (NSTC=6)
      PARAMETER (NREG=7)
      PARAMETER (NSUB=2)
      PARAMETER (NBAK=2)
 
      CHARACTER*80 HDSTR,PSTR
      CHARACTER*8  SUBSET

      real(8)    HDR(14)
      real(8)    POB(4)

      real(8)    CNTO,CNTN,RAT1,RAT2,WT1,WT2
      real(8)    SPRS(NSTC,NREG,NSUB,NBAK)
      real(8)    STC(NSTC,NBAK)

      real(4)    GDATA(NREG,NSUB)

      LOGICAL      REGION
 
      DATA HDSTR
     ./'SID XOB YOB DHR ELV TYP T29 ITP SQN RQM DUP PRG SRC RUD'/
 
      DATA PSTR /'POB PAN PFC PQM'/

      real(8) BMISS /10E10/

      DATA RMISS /-9.99E+33/
      DATA LUBFR/11/

C-----------------------------------------------------------------------
 
C  ZERO THE FIT ARRAYS
C  -------------------
 
      SPRS = 0.
      STC=0.
 
      bmiss=10e10; call setbmiss(bmiss) ! this sets bufrlib missing value to 10e10
      CALL OPENBF(LUBFR,'IN ',LUBFR)

C  READ AND "SURU-FIT" THE PREPDA/BUFR RECORDS
C  -------------------------------------------
 
10    DO WHILE(IREADMG(LUBFR,SUBSET,IDATE).EQ.0)
c... check for subset...
      IF(ITYP(SUBSET).EQ.0) GOTO 10
11    DO WHILE(IREADSB(LUBFR).EQ.0)

      CALL UFBINT(LUBFR,HDR,14,  1,NLEV,HDSTR)
      typx=hdr(6)

c... check for region...
      XOB=hdr(2)
      YOB=hdr(3)
c     CALL UFBINT(LUBFR,XOB,1,1,IREC,'XOB')
c     CALL UFBINT(LUBFR,YOB,1,1,IREC,'YOB')
      IF(.NOT.REGION(XOB,YOB,0)) GOTO 11
 
C  READ THE DATA
C  -------------
       CALL UFBINT(LUBFR,POB, 4,1,NLEV,PSTR)
       PO=POB(1)
       PA=POB(2)
       PF=POB(3)
       PQ=POB(4)
c
C  CREATE AND ACCUMULATE THE STATISTICS ARRAY FOR EACH REALIZATION
C  ---------------------------------------------------------------
 
      STC = 0.
C
      if((pa.eq.bmiss).or.(pf.eq.bmiss)) go to 11

      IF(PQ.LE.3) THEN
      if(idbug.eq.1) print *,'ob ',po,' ges ',pf, ' anl ',pa
C
c... start forecast-loop
         DO IB=1,2
C
c  count
            STC(1,IB) = 1.
c  f
            if(ib.eq.1) STC(2,IB) = PF-fac
            if(ib.eq.2) STC(2,IB) = PA-fac
c  o
            STC(3,IB) = PO-fac
c  f * o
            STC(4,IB) = STC(2,IB)*STC(3,IB)
c  f**2
            STC(5,IB) = STC(2,IB)**2
c  o**2
            STC(6,IB) = STC(3,IB)**2
C
         if(idbug.eq.1) print *,' ib ',ib,' stc ',(stc(k,ib),k=1,6)
c
c... end forecast-loop
         ENDDO
c
      ENDIF
 
         M = ITYP(SUBSET)
 
         if (m > 0) then
         DO N=1   ,NBAK
         DO LL=1  ,NREG
C
         IF(REGION(XOB,YOB,LL)) THEN
C
            cnto = SPRS(1,LL,M,N) 
            SPRS(1,LL,M,N) = SPRS(1,LL,M,N) + STC(1,N)
            cntn = SPRS(1,LL,M,N) 

            if(cntn.gt.cnto) then
            wt1 = cnto/cntn
            wt2 = 1.-wt1
c
            DO I=2,NSTC

            sprso = SPRS(I,LL,M,N)
            rat1 = wt1*SPRS(I,LL,M,N)
            rat2 = wt2*STC(I,N)
c
            SPRS(I,LL,M,N) = rat1 + rat2
            sprsn = SPRS(I,LL,M,N)
c
            if((idbug.eq.1).and.(i.eq.2)) then
            if((m.eq.1).and.(n.eq.1)) then
            write(6,3000) ll,cnto,cntn,wt1,wt2,sprso,STC(I,N),
     *      rat1,rat2,sprsn
            endif
            endif
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
      do nst=1,6
c
      do isub=1,nsub
      do ireg=1,nreg
      if((nst.eq.2).or.(nst.eq.3)) then
      gdata(ireg,isub)=sprs(nst,ireg,isub,ibak)+fac
      else
      gdata(ireg,isub)=sprs(nst,ireg,isub,ibak)
      endif
      enddo
      enddo
c
      write(iw) gdata
c
      if(ipr.eq.1) 
     *write(6,1232) ibak,nst,(gdata(ireg,1),ireg=1,nreg)
c
c... end stat-loop
      enddo
c
      close(iw)
c... end forecast-loop
      enddo
c
 1232  format('p fcs=',i2,2x,'stat = ',i2,2x,7f12.2)
 3000  format('reg ',i2,' cnto ',f7.0,' cntn ',f7.0,
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
 
      PARAMETER(NSUB=2)
 
      CHARACTER*8 SUBSET,SUBTYP(2)
      DATA SUBTYP /'ADPSFC','SFCSHP'/
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
