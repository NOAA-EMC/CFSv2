!
!   Author: Suranjana Saha
c--------------------------------------------------------------------------
c     the mnemonic tables are in /nwprod/prepobs/ucl/prep.bufrtable
c---------------------------------------------------------------------------
       CHARACTER*1   kdbug,kdbuga,kall,kmas,kwnd
       CHARACTER*255 infile,outfile
       CHARACTER*10  kst
       CHARACTER*6   catname
       CHARACTER*8   klonw,klone,klatn,klats,kdhr
       CHARACTER*2   kchr
       real(8) bmiss

       bmiss=10e10; call setbmiss(bmiss) ! this sets bufrlib missing value to 10e10

       call getenv("idbug",kdbug)
       read(kdbug,'(i1)') idbug
       write(*,*) "idbug= ",idbug
c
       call getenv("idbuga",kdbuga)
       read(kdbuga,'(i1)') idbuga
       write(*,*) "idbuga= ",idbuga
c
       call getenv("iall",kall)
       read(kall,'(i1)') iall
       write(*,*) "iall= ",iall
C
       call getenv("iwnd",kwnd)
       read(kwnd,'(i1)') iwnd
       write(*,*) "iwnd= ",iwnd
C
       call getenv("imas",kmas)
       read(kmas,'(i1)') imas
       write(*,*) "imas= ",imas
C
       call getenv("fchr",kchr)
       read(kchr,'(i2)') ichr
       fchr=float(ichr)
       write(*,*) "fchr= ",fchr
C
       call getenv("fdhr",kdhr)
       read(kdhr,'(f8.1)') fdhr
       write(*,*) "fdhr= ",fdhr
c
       call getenv("nst",kst)
       read(kst,'(i10)') nst
       write(*,*) "nst= ",nst
c
       call getenv("infile",infile)
       write(*,*) "infile= ",infile
c
       call getenv("outfile",outfile)
       write(*,*) "outfile= ",outfile
c
       call getenv("catname",catname)
       write(*,*) "catname= ",catname
c
       call getenv("flonw",klonw)
       read(klonw,'(f8.1)') flonw
       write(*,*) "flonw= ",flonw
c
       call getenv("flone",klone)
       read(klone,'(f8.1)') flone
       write(*,*) "flone= ",flone
c
       call getenv("flatn",klatn)
       read(klatn,'(f8.1)') flatn
       write(*,*) "flatn= ",flatn
c
       call getenv("flats",klats)
       read(klats,'(f8.1)') flats
       write(*,*) "flats= ",flats
c
       if(flonw.lt.0.) flonw=360.+flonw
       if(flone.lt.0.) flone=360.+flone
c
       fhrs=fchr-fdhr
       fhre=fchr+fdhr
c
       print *,fchr,fdhr,fhrs,fhre
       call stats(infile,outfile,catname,flonw,flone,flatn,flats,
     * idbug,idbuga,iall,nst,fhrs,fhre,imas,iwnd)
c
       stop
       end
       subroutine stats(infile,outfile,catname,flonw,flone,flatn,flats,
     * idbug,idbuga,iall,nst,fhrs,fhre,imas,iwnd)
c
       CHARACTER*80 HSTR,PSTR,ZSTR,TSTR,QSTR,USTR,VSTR
       CHARACTER*8  SUBSET
C
       character*8   tstn(nst),stnidc,id
       CHARACTER*255 infile,outfile
c
       CHARACTER*10 indate
       CHARACTER*8  STNID,idj,idi
       CHARACTER*6  catname
       CHARACTER*1  labn
C
       real(8)    HDR(14)
       EQUIVALENCE  (HDR(1),STNID)
       real(8)    POB(4),ZOB(4),TOB(4),QOB(4)
       real(8)    UOB(4),VOB(4)
       DIMENSION    PPR(4),ZPR(4),TPR(4),QPR(4)
       DIMENSION    UPR(4),VPR(4)
       DIMENSION    tlon(nst),tlat(nst),thr(nst),typ(nst),elvt(nst)
       DIMENSION    typint(nst)
       DIMENSION    mmfill(nst),mwfill(nst)
C
       DIMENSION BL(nst)
       DIMENSION PO(nst),PA(nst),PF(nst),PQ(nst)
       DIMENSION ZO(nst),ZA(nst),ZF(nst),ZQ(nst)
       DIMENSION TO(nst),TA(nst),TF(nst),TQ(nst)
       DIMENSION QO(nst),QA(nst),QF(nst),QQ(nst)
       DIMENSION UO(nst),UA(nst),UF(nst),WQ(nst)
       DIMENSION VO(nst),VA(nst),VF(nst)
c
       DATA HSTR
     ./'SID XOB YOB DHR ELV TYP T29 ITP SQN RQM DUP PRG SRC RUD'/
C
       DATA STNID/'        '/
c
c...   get the ob, guess, analysis and quality mark ....
       DATA PSTR /'POB PAN PFC PQM'/
c...   mean-sea-level pressure instead of height
       DATA ZSTR /'PMO PAN PFC PMQ'/
c      DATA ZSTR /'ZOB ZAN ZFC ZQM'/
       DATA TSTR /'TOB TAN TFC TQM'/
       DATA QSTR /'QOB QAN QFC QQM'/
       DATA USTR /'UOB UAN UFC WQM'/
       DATA VSTR /'VOB VAN VFC WQM'/
C
       DATA BMISS /10E10/
       DATA rmiss /99999./
       DATA LUNIN /11/
C----------------------------------------------------
       indate='          '
c
       print *,' infile ',infile
       print *,' outfile ',outfile
       print *,' flonw ',flonw
       print *,' flone ',flone
       print *,' flatn ',flatn
       print *,' flats ',flats
       iodd=0
       if(flonw.gt.flone) then
       iodd=1
       flone=flone+360.
       print *,' iodd ',iodd
       print *,' flone ',flone
       endif
c
       open(11,file=infile,form='unformatted')
c
C  INITIALIZE
C  ----------
       DO IST=1,nst
       PO(IST)   = bmiss
       ZO(IST)   = bmiss
       TO(IST)   = bmiss
       QO(IST)   = bmiss
       UO(IST)   = bmiss
       VO(IST)   = bmiss
       PA(IST)   = bmiss
       ZA(IST)   = bmiss
       TA(IST)   = bmiss
       QA(IST)   = bmiss
       UA(IST)   = bmiss
       VA(IST)   = bmiss
       PF(IST)   = bmiss
       ZF(IST)   = bmiss
       TF(IST)   = bmiss
       QF(IST)   = bmiss
       UF(IST)   = bmiss
       VF(IST)   = bmiss
       PQ(IST)   = bmiss
       ZQ(IST)   = bmiss
       TQ(IST)   = bmiss
       QQ(IST)   = bmiss
       WQ(IST)   = bmiss
       BL(IST)   = bmiss
       mmfill(IST)   = 0
       mwfill(IST)   = 0
       ENDDO
C
       nvar=0
       if((imas.eq.1).and.(iwnd.eq.1)) nvar=1
c
       CALL DATELEN(10)
       CALL OPENBF(LUNIN,'IN ',LUNIN)
c
       IST=0
!      DO WHILE(IREADFT(LUNIN,SUBSET,IDATE).EQ.0)
       DO WHILE(IREADMG(LUNIN,SUBSET,IDATE).EQ.0)
       write(indate,'(i10)') idate
c
c... look for only upper-air data... if you want anything else, the
c    following headers can be used :
c
c    ADPSFC SFCSHP  AIRCFT AIRCAR SATWND UPABOG 
c    SFCBOG SATEMP SATBOG SYNDAT PROFLR SPSSMI
c
       IF(SUBSET.EQ.catname) THEN
C
         DO WHILE(IREADSB(LUNIN).EQ.0)
c
           CALL UFBINT(LUNIN,HDR,14,  1,NLEV,HSTR)
c
           flon=hdr(2)
           flat=hdr(3)
           hr=hdr(4)
           elvx=hdr(5)
           typx=hdr(6)
           kkx = idint(hdr(6)*.01)
           typintx=hdr(8)
c
       if((nvar.eq.0).and.(imas.eq.1).and.(kkx.ne.1)) go to 443
       if((nvar.eq.0).and.(iwnd.eq.1).and.(kkx.ne.2)) go to 443
c
c... check for window hour...
       hrx=hr*10.
       fhrsx=fhrs*10.
       fhrex=fhre*10.
           if((hrx.ge.fhrsx).and.(hrx.lt.fhrex)) then
c
c... look for all stations inside the lat/lon box specified...
c
       flonx=flon
       if((iodd.eq.1).and.(flonx.lt.flonw)) flonx=flonx+360.
c
       if((flonx.ge.flonw).and.(flonx.le.flone).and.
     *    (flat.le.flatn).and.(flat.ge.flats)) then
c
c...  first time....
       if(ist.eq.0) then
       IST=IST+1
       tstn(IST)=stnid
       tlon(IST)=flon
       tlat(IST)=flat
       thr(IST)=hr
       elvt(IST)=elvx
       typint(IST)=typintx
       if(kkx.eq.1) then
       typ(IST)=typx-100
       else
       typ(IST)=typx-200
       endif
       go to 10
       endif
c
c..   check for unique station....
       if(nvar.eq.1) then
       do n=1,ist
       dlon=abs(flon-tlon(n))
       dlat=abs(flat-tlat(n))
       dthr=abs(hr-thr(n))
       if((dlat.le.0.01).and.(dlon.le.0.01).and.(dthr.le.0.001)) then
       mmf=mmfill(n)
       mwf=mwfill(n)
c
c...  check if both wind and mass report have been read..
       if((mmf.eq.1).and.(mwf.eq.1)) go to 443
       if((mmf.eq.0).and.(mwf.eq.0)) go to 11
       if((mmf.eq.1).or.(mwf.eq.1))  go to 10
c
       endif
       enddo
       endif
c
c...  if not, then add a new station....
 11    continue
       IST=IST+1
       if(ist.gt.nst) then
       print *,'number of stations exceeded'
       print *,'increase number to more than ',nst
       go to 1555
       endif
       tstn(IST)=stnid
       tlon(IST)=flon
       tlat(IST)=flat
       thr(IST)=hr
       elvt(IST)=elvx
       typint(IST)=typintx
       if(kkx.eq.1) then
       typ(IST)=typx-100
       else
       typ(IST)=typx-200
       endif
c
 10    continue
       CALL UFBINT(LUNIN,POB, 4,1,NLEV,PSTR)
       PO(IST)=POB(1)
       PA(IST)=POB(2)
       PF(IST)=POB(3)
       PQ(IST)=POB(4)
c
       if((kkx.eq.1).and.(imas.eq.1)) then
       mmfill(ist)=1
c
       do nn=1,4
       pob(nn)=bmiss
       zob(nn)=bmiss
       tob(nn)=bmiss
       qob(nn)=bmiss
       ppr(nn)=bmiss
       zpr(nn)=bmiss
       tpr(nn)=bmiss
       qpr(nn)=bmiss
       enddo
c
       CALL UFBINT(LUNIN,ZOB, 4,1,NLEV,ZSTR)
       CALL UFBINT(LUNIN,TOB, 4,1,NLEV,TSTR)
       CALL UFBINT(LUNIN,QOB, 4,1,NLEV,QSTR)
c
       do i=1,4
       if(zob(i).ne.bmiss) zpr(i)=zob(i)
       if(zpr(i).gt.rmiss) zpr(i)=bmiss
       if(tob(i).ne.bmiss) tpr(i)=tob(i)
       if(tpr(i).gt.rmiss) tpr(i)=bmiss
       if(qob(i).ne.bmiss) qpr(i)=qob(i)
       if(qpr(i).gt.rmiss) qpr(i)=bmiss
       enddo
c
       ZO(IST) =  ZPR(1)
       ZQ(IST) =  ZPR(4)
       TQ(IST) =  TPR(4)
       QQ(IST) =  QPR(4)
c
       TO(IST) =  TPR(1)
       QO(IST) =  QPR(1)
       if(TO(IST).ne.bmiss) TO(IST) =  TO(IST)+273.159
       IF(QO(IST).ne.bmiss) QO(IST) =  QO(IST)*1.E-3
       TV =  TO(IST)
       Q  =  QO(IST)
       if(Q.ne.bmiss) Q=Q*1.E-3
       if((Q.ne.bmiss).and.(TV.ne.bmiss)) TO(IST)=TV/(1.+.608*Q)
c
       if(iall.eq.1) then
       ZA(IST) =  ZPR(2)
       ZF(IST) =  ZPR(3)
c
       TA(IST) =  TPR(2)
       QA(IST) =  QPR(2)
       if(TA(IST).ne.bmiss) TA(IST) =  TA(IST)+273.159
       IF(QA(IST).ne.bmiss) QA(IST) =  QA(IST)*1.E-3
       TV = TA(IST)
       Q = QA(IST)
       if(Q.ne.bmiss) Q=Q*1.E-3
       if((Q.ne.bmiss).and.(TV.ne.bmiss)) TA(IST)=TV/(1.+.608*Q)
c
       TF(IST) =  TPR(3)
       QF(IST) =  QPR(4)
       if(TF(IST).ne.bmiss) TF(IST) =  TF(IST)+273.159
       IF(QF(IST).ne.bmiss) QF(IST) =  QF(IST)*1.E-3
       TV =  TF(IST)
       Q  =  QF(IST)
       if(Q.ne.bmiss) Q=Q*1.E-3
       if((Q.ne.bmiss).and.(TV.ne.bmiss)) TF(IST)=TV/(1.+.608*Q)
       endif
c
       if(idbug.eq.1) then
       pval=po(ist)
       typx=typ(ist)+100
       if(iall.eq.1) then
       write(6,611) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * pval,zo(ist),za(ist),zf(ist),zq(ist)
       write(6,612) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * pval,to(ist),ta(ist),tf(ist),tq(ist)
       write(6,613) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * pval,qo(ist),qa(ist),qf(ist),qq(ist)
       else
       write(6,711) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * pval,zo(ist),zq(ist)
       write(6,712) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * pval,to(ist),tq(ist)
       write(6,713) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * pval,qo(ist),qq(ist)
       endif
       endif
c
       endif
c
c... now for wind report...
       if((kkx.eq.2).and.(iwnd.eq.1)) then
       mwfill(ist)=1
c
       do nn=1,4
       uob(nn)=bmiss
       vob(nn)=bmiss
       upr(nn)=bmiss
       vpr(nn)=bmiss
       enddo
c
       CALL UFBINT(LUNIN,UOB, 4,1,NLEV,USTR)
       CALL UFBINT(LUNIN,VOB, 4,1,NLEV,VSTR)
c
       do i=1,4
       if(uob(i).ne.bmiss) upr(i)=uob(i)
       if(upr(i).gt.rmiss) upr(i)=bmiss
       if(vob(i).ne.bmiss) vpr(i)=vob(i)
       if(vpr(i).gt.rmiss) vpr(i)=bmiss
       enddo
c
       UO(IST) =  UPR(1)
       WQ(IST) =  UPR(4)
       VO(IST) =  VPR(1)
c
       if(iall.eq.1) then
       UA(IST) =  UPR(2)
       UF(IST) =  UPR(3)
       VA(IST) =  VPR(2)
       VF(IST) =  VPR(3)
       endif
c
       if(idbug.eq.1) then
       pval=po(ist)
       typx=typ(ist)+200
       if(iall.eq.1) then
       write(6,614) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * pval,uo(ist),ua(ist),uf(ist),wq(ist)
       write(6,615) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * pval,vo(ist),va(ist),vf(ist)
       else
       write(6,714) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * pval,uo(ist),wq(ist)
       write(6,715) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * pval,vo(ist)
       endif
       endif
c
       endif
c
c... end lat/lon box section
       endif
c
c... end window-hour section
       endif
c
 443   continue
c
       ENDDO
c
c...  end report section
       endif
C
       ENDDO
c
 1555  continue
       nstns=IST
       print *,indate,' number of stations ',nstns
       if(nstns.gt.0) then
c
       open(51,file=outfile,form='unformatted')
c
c.... get unique station ids...
c
c      do j=1,nstns
c      n=0
c      idj=tstn(j)
c      do i=j+1,nstns
c      idi=tstn(i)
c      if(idj.eq.idi) then
c      n=n+1
c      write(labn,'(i1)') n
c      idi(6:6)=labn
c      tstn(i)=idi
c      endif
c      enddo
c      enddo
c
c....  write grads data out...
c
       do ist=1,nstns
c
       id=tstn(ist)
       rlon=tlon(ist)
       rlat=tlat(ist)
       hr=thr(ist)
       iflag=1
       nl=1
c      id(7:7)=' '
c      id(8:8)='\0'
c
       rt=0.0
       write(51) id,rlat,rlon,rt,nl,iflag
c
c... to plot mslp and ps to meaningful digits...
c      if(po(ist).ne.bmiss) then
c      if(po(ist).lt.1000.) then
c      diff=po(ist)-900.
c      else
c      diff=po(ist)-1000.
c      endif
c      po(ist)=10.*diff
c      endif
c      if(zo(ist).ne.bmiss) then
c      if(zo(ist).lt.1000.) then
c      diff=zo(ist)-900.
c      else
c      diff=zo(ist)-1000.
c      endif
c      zo(ist)=10.*diff
c      endif
c
       if(iall.eq.1) then
       if((imas.eq.1).and.(iwnd.eq.1)) then
       write(51) typ(ist),typint(ist),elvt(ist),
     *           po(ist),pa(ist),pf(ist),pq(ist),
     *           zo(ist),za(ist),zf(ist),zq(ist),
     *           to(ist),ta(ist),tf(ist),tq(ist),
     *           qo(ist),qa(ist),qf(ist),qq(ist),
     *           uo(ist),ua(ist),uf(ist),wq(ist),
     *           vo(ist),va(ist),vf(ist),bl(ist)
       if(idbuga.eq.1) then
       print *,id,rlat,rlon,rt,nl,iflag
       write(6,411) indate,hr,po(ist),pa(ist),pf(ist),pq(ist),
     *              zo(ist),za(ist),zf(ist),zq(ist),
     *              to(ist),ta(ist),tf(ist),tq(ist),
     *              qo(ist),qa(ist),qf(ist),qq(ist),
     *              uo(ist),ua(ist),uf(ist),wq(ist),
     *              vo(ist),va(ist),vf(ist)
       endif
       endif
       if((imas.eq.1).and.(iwnd.eq.0)) then
       write(51) typ(ist),typint(ist),elvt(ist),
     *           po(ist),pa(ist),pf(ist),pq(ist),
     *           zo(ist),za(ist),zf(ist),zq(ist),
     *           to(ist),ta(ist),tf(ist),tq(ist),
     *           qo(ist),qa(ist),qf(ist),qq(ist),bl(ist)
       if(idbuga.eq.1) then
       print *,id,rlat,rlon,rt,nl,iflag
       write(6,412) indate,hr,po(ist),pa(ist),pf(ist),pq(ist),
     *              zo(ist),za(ist),zf(ist),zq(ist),
     *              to(ist),ta(ist),tf(ist),tq(ist),
     *              qo(ist),qa(ist),qf(ist),qq(ist)
       endif
       endif
       if((imas.eq.0).and.(iwnd.eq.1)) then
       write(51) typ(ist),elvt(ist),
     *           po(ist),pa(ist),pf(ist),pq(ist),
     *           uo(ist),ua(ist),uf(ist),wq(ist),
     *           vo(ist),va(ist),vf(ist),bl(ist)
       if(idbuga.eq.1) then
       print *,id,rlat,rlon,rt,nl,iflag
       write(6,413) indate,hr,po(ist),pa(ist),pf(ist),pq(ist),
     *              uo(ist),ua(ist),uf(ist),wq(ist),
     *              vo(ist),va(ist),vf(ist)
       endif
       endif
c
       else
c
       if((imas.eq.1).and.(iwnd.eq.1)) then
       write(51) typ(ist),typint(ist),elvt(ist),
     *           po(ist),pq(ist),
     *           zo(ist),zq(ist),
     *           to(ist),tq(ist),
     *           qo(ist),qq(ist),
     *           uo(ist),wq(ist),
     *           vo(ist),bl(ist)
       if(idbuga.eq.1) then
       print *,id,rlat,rlon,rt,nl,iflag
       write(6,511) indate,hr,po(ist),pq(ist),
     *              zo(ist),zq(ist),
     *              to(ist),tq(ist),
     *              qo(ist),qq(ist),
     *              uo(ist),wq(ist),
     *              vo(ist)
       endif
       endif
       if((imas.eq.1).and.(iwnd.eq.0)) then
       write(51) typ(ist),elvt(ist),
     *           po(ist),pq(ist),
     *           zo(ist),zq(ist),
     *           to(ist),tq(ist),
     *           qo(ist),qq(ist),bl(ist)
       if(idbuga.eq.1) then
       print *,id,rlat,rlon,rt,nl,iflag
       write(6,512) indate,hr,po(ist),pq(ist),
     *              zo(ist),zq(ist),
     *              to(ist),tq(ist),
     *              qo(ist),qq(ist)
       endif
       endif
       if((imas.eq.0).and.(iwnd.eq.1)) then
       write(51) typ(ist),typint(ist),elvt(ist),
     *           po(ist),pq(ist),
     *           uo(ist),wq(ist),
     *           vo(ist),bl(ist)
       if(idbuga.eq.1) then
       print *,id,rlat,rlon,rt,nl,iflag
       write(6,513) indate,hr,po(ist),pq(ist),
     *              uo(ist),wq(ist),
     *              vo(ist)
       endif
       endif
c
       endif
c
c... end station-loop
        enddo
c
c..  write footer to end this time level
       id='       '
       rlat=0.
       rlon=0.
       rt=0.
       nl=0
       iflag=0
       write(51) id,rlat,rlon,rt,nl,iflag
c
       close(51)
       endif
c
       close(11)
       CALL CLOSBF(LUNIN)
c
 411   format(a10,1x,f5.2,1x,f5.0,' mb ',1x,f5.0,1x,f5.0,1x,f3.0,1x,
     *        4(3e10.2,1x,f3.0,/),3e10.2)
 412   format(a10,1x,f5.2,1x,f5.0,' mb ',1x,f5.0,1x,f5.0,1x,f3.0,1x,
     *        3(3e10.2,1x,f3.0,/))
 413   format(a10,1x,f5.2,1x,f5.0,' mb ',1x,f5.0,1x,f5.0,1x,f3.0,1x,
     *        3e10.2,1x,f3.0,/,3e10.2)
c
 511   format(a10,1x,f5.2,1x,f5.0,' mb ',1x,f3.0,1x,
     *        4(e10.2,1x,f3.0),1x,e10.2)
 512   format(a10,1x,f5.2,1x,f5.0,' mb ',1x,f3.0,1x,
     *        3(e10.2,1x,f3.0))
 513   format(a10,1x,f5.2,1x,f5.0,' mb ',1x,f3.0,1x,
     *        e10.2,1x,f3.0,1x,e10.2)
c
 611   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'z',1x,f5.0,1x,3f10.2,1x,'qf ',f4.0)
 612   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'t',1x,f5.0,1x,3f10.2,1x,'qf ',f4.0)
 613   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'q',1x,f5.0,1x,3f10.2,1x,'qf ',f4.0)
 614   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'u',1x,f5.0,1x,3f10.2,1x,'qf ',f4.0)
 615   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'v',1x,f5.0,1x,3f10.2)
c
 711   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'z',1x,f5.0,1x,f10.2,1x,'qf ',f4.0)
 712   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'t',1x,f5.0,1x,f10.2,1x,'qf ',f4.0)
 713   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'q',1x,f5.0,1x,f10.2,1x,'qf ',f4.0)
 714   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'u',1x,f5.0,1x,f10.2,1x,'qf ',f4.0)
 715   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'v',1x,f5.0,1x,f10.2)
c
       return
       end
C-----------------------------------------------------------------------
       INTEGER FUNCTION nfill(C)
       CHARACTER*(*) C
       NFILL=LEN(C)
       DO 1 J=1,NFILL
       IF(C(J:J).EQ.' ') THEN
       NFILL=J-1
       RETURN
       ENDIF
 1     CONTINUE
       RETURN
       END
