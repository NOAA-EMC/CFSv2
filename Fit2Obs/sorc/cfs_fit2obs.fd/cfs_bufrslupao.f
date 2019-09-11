!
!   Author: Suranjana Saha
c--------------------------------------------------------------------------
c    compile using : make NAME=bufrslupa
c--------------------------------------------------------------------------
c     the mnemonic tables are in /nwprod/prepobs/ucl/prep.bufrtable
c---------------------------------------------------------------------------
       CHARACTER*1   kdbug,kdbuga,kall,kprs,kmas,kwnd
       CHARACTER*2   ktol
       CHARACTER*6   catname
       CHARACTER*255 infile,outfile
       CHARACTER*5   kst,klev
       CHARACTER*8   klonw,klone,klatn,klats,kdhr
       CHARACTER*2   kchr
       real(8)       bmiss

       dimension plev(28)

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
       read(kst,'(i5)') nst
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
       call getenv("ilev",klev)
       read(klev,'(i4)') ilev
       write(*,*) "ilev= ",ilev
c
       call getenv("iprs",kprs)
       read(kprs,'(i1)') iprs
       write(*,*) "iprs= ",iprs
c
       call getenv("itol",ktol)
       read(ktol,'(i2)') itol
       write(*,*) "itol= ",itol
c
c...  define mandatory levels....
       if(iprs.eq.1) then
         nlevs=21
         plev(1)= 1000
         plev(2)= 925
         plev(3)= 850
         plev(4)= 700
         plev(5)= 500
         plev(6)= 400
         plev(7)= 300
         plev(8)= 250
         plev(9)=200
         plev(10)=150
         plev(11)=100
         plev(12)=70
         plev(13)=50
         plev(14)=30
         plev(15)=20
         plev(16)=10
         plev(17)=7
         plev(18)=5
         plev(19)=3
         plev(20)=2
         plev(21)=1
       endif
c
c...  define sigma levels....
       if(iprs.eq.0) then
         nlevs=28
         plev(1)= 995
         plev(2)= 982
         plev(3)= 964
         plev(4)= 943
         plev(5)= 916
         plev(6)= 884
         plev(7)= 846
         plev(8)= 801
         plev(9)= 751
         plev(10)=694
         plev(11)=633
         plev(12)=568
         plev(13)=502
         plev(14)=436
         plev(15)=372
         plev(16)=312
         plev(17)=258
         plev(18)=210
         plev(19)=168
         plev(20)=133
         plev(21)=103
         plev(22)= 78
         plev(23)= 58
         plev(24)= 42
         plev(25)= 29
         plev(26)= 18
         plev(27)= 10
         plev(28)=  3
       endif
c
         if(iprs.eq.2) nlevs=ilev
c
       if(flonw.lt.0.) flonw=360.+flonw
       if(flone.lt.0.) flone=360.+flone
       fhrs=fchr-fdhr
       fhre=fchr+fdhr
       print *,fchr,fdhr,fhrs,fhre
c
       call stats(infile,outfile,flonw,flone,flatn,flats,
     * iprs,nlevs,plev,idbug,idbuga,itol,iall,nst,fhrs,fhre,
     * catname,imas,iwnd)
c
       stop
       end
       subroutine stats(infile,outfile,flonw,flone,flatn,flats,
     * iprs,nlevs,plev,idbug,idbuga,itol,iall,nst,fhrs,fhre,
     * catname,imas,iwnd)
c
       CHARACTER*80 HSTR,PSTR,ZSTR,TSTR,QSTR,USTR,VSTR,PSTR1
       CHARACTER*8  SUBSET
C
       character*8   tstn(nst),stnidc,id
       CHARACTER*255 infile,outfile
c
       CHARACTER*10 indate
       CHARACTER*6  catname
       CHARACTER*8  STNID,idj,idi
       CHARACTER*1  labn
C
       real(8)    HDR(14)
       EQUIVALENCE  (HDR(1),STNID)
       real(8)    POB(4,255),ZOB(4,255),TOB(4,255),QOB(4,255)
       real(8)    UOB(4,255),VOB(4,255),PSOB(4)
       DIMENSION    PPR(4,nlevs),ZPR(4,nlevs),TPR(4,nlevs),QPR(4,nlevs)
       DIMENSION    UPR(4,nlevs),VPR(4,nlevs),PSPR(4)
       DIMENSION    tlon(nst),tlat(nst),thr(nst),typ(nst),elvt(nst)
       DIMENSION    typint(nst)
C
       DIMENSION PO(nlevs,nst)
       DIMENSION POM(nlevs,nst),POW(nlevs,nst)
       DIMENSION PSO(nst),PSA(nst),PSF(nst),PSQ(nst)
       DIMENSION ZO(nlevs,nst),ZA(nlevs,nst),ZF(nlevs,nst),ZQ(nlevs,nst)
       DIMENSION TO(nlevs,nst),TA(nlevs,nst),TF(nlevs,nst),TQ(nlevs,nst)
       DIMENSION QO(nlevs,nst),QA(nlevs,nst),QF(nlevs,nst),QQ(nlevs,nst)
       DIMENSION UO(nlevs,nst),UA(nlevs,nst),UF(nlevs,nst),WQ(nlevs,nst)
       DIMENSION VO(nlevs,nst),VA(nlevs,nst),VF(nlevs,nst)
c
       DIMENSION POG(nlevs)
       DIMENSION ZOG(nlevs),ZAG(nlevs),ZFG(nlevs),ZQG(nlevs)
       DIMENSION TOG(nlevs),TAG(nlevs),TFG(nlevs),TQG(nlevs)
       DIMENSION QOG(nlevs),QAG(nlevs),QFG(nlevs),QQG(nlevs)
       DIMENSION UOG(nlevs),UAG(nlevs),UFG(nlevs),WQG(nlevs)
       DIMENSION VOG(nlevs),VAG(nlevs),VFG(nlevs)
c
       REAL    plev(nlevs)
       integer kfillm(nlevs,nst),kfillw(nlevs,nst)
       integer mmfill(nst),mwfill(nst)
       integer kmmax(nst),kwmax(nst)
c
       DATA HSTR
     ./'SID XOB YOB DHR ELV TYP T29 ITP SQN RQM DUP PRG SRC RUD'/
C
       DATA STNID/'        '/
c
c...   get the ob, guess, analysis and quality mark ....
c
       DATA PSTR1 /'POB PAN PFC PQM CAT=0'/
       DATA PSTR /'POB PAN PFC PQM'/
       DATA ZSTR /'ZOB ZAN ZFC ZQM'/
       DATA TSTR /'TOB TAN TFC TQM'/
       DATA QSTR /'QOB QAN QFC QQM'/
c      DATA USTR /'DDO UAN UFC WQM'/
c      DATA VSTR /'FFO VAN VFC WQM'/
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
       print *,' catname ',catname
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
       open(51,file=outfile,form='unformatted')
c
C  INITIALIZE
C  ----------
       DO IST=1,nst
       PSO(IST)   = bmiss
       PSA(IST)   = bmiss
       PSF(IST)   = bmiss
       PSQ(IST)   = bmiss
       ENDDO
C
       DO IST=1,nst
       DO L=1,nlevs
       PO(L,IST)   = bmiss
       POM(L,IST)   = bmiss
       POW(L,IST)   = bmiss
       ZO(L,IST)   = bmiss
       TO(L,IST)   = bmiss
       QO(L,IST)   = bmiss
       UO(L,IST)   = bmiss
       VO(L,IST)   = bmiss
       ZA(L,IST)   = bmiss
       TA(L,IST)   = bmiss
       QA(L,IST)   = bmiss
       UA(L,IST)   = bmiss
       VA(L,IST)   = bmiss
       ZF(L,IST)   = bmiss
       TF(L,IST)   = bmiss
       QF(L,IST)   = bmiss
       UF(L,IST)   = bmiss
       VF(L,IST)   = bmiss
       ZQ(L,IST)   = bmiss
       TQ(L,IST)   = bmiss
       QQ(L,IST)   = bmiss
       WQ(L,IST)   = bmiss
       KFILLM(L,IST)   = 0
       KFILLW(L,IST)   = 0
       ENDDO
       MMFILL(IST)   = 0
       MWFILL(IST)   = 0
       ENDDO
C
       nvar=0
       if((imas.eq.1).and.(iwnd.eq.1)) nvar=1
       print *,'nvar ',nvar
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
           hr=hdr(4)
           flon=hdr(2)
           flat=hdr(3)
           typx=hdr(6)
           typintx=hdr(8)
           elvx=hdr(5)
           kkx = idint(hdr(6)*.01)
c
       if((nvar.eq.0).and.(imas.eq.1).and.(kkx.ne.1)) go to 443
       if((nvar.eq.0).and.(iwnd.eq.1).and.(kkx.ne.2)) go to 443
c
c... check for window hour...
       hrx=hr*10.
       fhrsx=fhrs*10.
       fhrex=fhre*10.
           if((hrx.ge.fhrsx).and.(hrx.le.fhrex)) then
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
       tstn(IST)='        '
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
       if((mmf.eq.0).and.(mwf.eq.0)) go to 12
       if((mmf.eq.1).or.(mwf.eq.1))  go to 10
c
       endif
       enddo
       endif
c
c...  if not, then add a new station....
 12    continue
       IST=IST+1
       if(ist.gt.nst) then
       print *,'number of stations exceeded'
       print *,'increase number to more than ',nst
       go to 1555
       endif
       tstn(IST)='        '
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
c
       do nn=1,4
       psob(nn)=bmiss
       enddo
c
       if((kkx.eq.1).and.(imas.eq.1)) then
       do ll=1,255
       do nn=1,4
       pob(nn,ll)=bmiss
       zob(nn,ll)=bmiss
       tob(nn,ll)=bmiss
       qob(nn,ll)=bmiss
       enddo
       enddo
       do ll=1,nlevs
       do nn=1,4
       ppr(nn,ll)=bmiss
       zpr(nn,ll)=bmiss
       tpr(nn,ll)=bmiss
       qpr(nn,ll)=bmiss
       enddo
       enddo
       endif
c
       if((kkx.eq.2).and.(iwnd.eq.1)) then
       do ll=1,255
       do nn=1,4
       uob(nn,ll)=bmiss
       vob(nn,ll)=bmiss
       enddo
       enddo
       do ll=1,nlevs
       do nn=1,4
       ppr(nn,ll)=bmiss
       zpr(nn,ll)=bmiss
       tpr(nn,ll)=bmiss
       qpr(nn,ll)=bmiss
       upr(nn,ll)=bmiss
       vpr(nn,ll)=bmiss
       enddo
       enddo
       endif
c
c... get surface pressure ob
       if(kkx.eq.1) then
       CALL UFBINT(LUNIN,PSOB, 4,1,NLEV,PSTR1)
       if(nlev.eq.1) then
       do i=1,4
       if(psob(i).ne.bmiss) pspr(i)=psob(i)
       if(pspr(i).gt.rmiss) pspr(i)=bmiss
       enddo
       pso(IST) =  pspr(1)
       psa(IST) =  pspr(2)
       psf(IST) =  pspr(3)
       psq(IST) =  pspr(4)
       endif
       write(6,610) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * pso(ist),psa(ist),psf(ist),psq(ist)
       endif
c
c... get pressure at all levels..
       CALL UFBINT(LUNIN,POB, 4,255,NLEV,PSTR)
C..
       lx=0
       DO l=1,nlev
c
       pdata=POB(1,l)
c
       if(iprs.le.1) then
       if(kkx.eq.1) k=GETLEV(pdata,plev,itol,nlevs,kfillm,ist,nst)
       if(kkx.eq.2) k=GETLEV(pdata,plev,itol,nlevs,kfillw,ist,nst)
       else
       k=0
       lx=lx+1
       if(lx.gt.nlevs) then
       print *,'number of levels more than ilev ..cap at level ',pdata
       go to 443
       endif
       if((kkx.eq.1).and.(kfillm(lx,ist).eq.0)) k=lx
       if((kkx.eq.2).and.(kfillw(lx,ist).eq.0)) k=lx
       endif
c
c..  check proper level...
c
       if(k.gt.0) then
c
       if(iprs.le.1) then
       PO(K,IST) =  plev(k)
       else
       if(nvar.eq.0) then
       po(k,ist)=pob(1,l)
       else
       if(kkx.eq.1) POM(K,IST) =  pob(1,l)
       if(kkx.eq.2) POW(K,IST) =  pob(1,l)
       endif
       endif
c
c...  check for mass report....
       if((kkx.eq.1).and.(imas.eq.1)) then
       kmmax(ist)=k
       kfillm(k,ist)=1
       mmfill(ist)=1
c
       CALL UFBINT(LUNIN,ZOB, 4,255,NLEV,ZSTR)
       CALL UFBINT(LUNIN,TOB, 4,255,NLEV,TSTR)
       CALL UFBINT(LUNIN,QOB, 4,255,NLEV,QSTR)
c
       do i=1,4
       if(zob(i,l).ne.bmiss) zpr(i,k)=zob(i,l)
       if(zpr(i,k).gt.rmiss) zpr(i,k)=bmiss
       if(tob(i,l).ne.bmiss) tpr(i,k)=tob(i,l)
       if(tpr(i,k).gt.rmiss) tpr(i,k)=bmiss
       if(qob(i,l).ne.bmiss) qpr(i,k)=qob(i,l)
       if(qpr(i,k).gt.rmiss) qpr(i,k)=bmiss
       enddo
c
       ZO(K,IST) =  ZPR(1,K)
       ZQ(K,IST) =  ZPR(4,K)
       TQ(K,IST) =  TPR(4,K)
       QQ(K,IST) =  QPR(4,K)
c
       TO(K,IST) =  TPR(1,K)
       QO(K,IST) =  QPR(1,K)
       if(TO(K,IST).ne.bmiss) TO(K,IST) =  TO(K,IST)+273.159
       IF(QO(K,IST).ne.bmiss) QO(K,IST) =  QO(K,IST)*1.E-3
       TV =  TO(K,IST)
       Q  =  QO(K,IST)
       if(Q.ne.bmiss) Q=Q*1.E-3
       if((Q.ne.bmiss).and.(TV.ne.bmiss)) TO(K,IST)=TV/(1.+.608*Q)
c
       if(iall.eq.1) then
       ZA(K,IST) =  ZPR(2,K)
       ZF(K,IST) =  ZPR(3,K)
c
       TA(K,IST) =  TPR(2,K)
       QA(K,IST) =  QPR(2,K)
       if(TA(K,IST).ne.bmiss) TA(K,IST) =  TA(K,IST)+273.159
       IF(QA(K,IST).ne.bmiss) QA(K,IST) =  QA(K,IST)*1.E-3
       TV = TA(K,IST)
       Q = QA(K,IST)
       if(Q.ne.bmiss) Q=Q*1.E-3
       if((Q.ne.bmiss).and.(TV.ne.bmiss)) TA(K,IST)=TV/(1.+.608*Q)
c
       TF(K,IST) =  TPR(3,K)
       QF(K,IST) =  QPR(3,K)
       if(TF(K,IST).ne.bmiss) TF(K,IST) =  TF(K,IST)+273.159
       IF(QF(K,IST).ne.bmiss) QF(K,IST) =  QF(K,IST)*1.E-3
       TV =  TF(K,IST)
       Q  =  QF(K,IST)
       if(Q.ne.bmiss) Q=Q*1.E-3
       if((Q.ne.bmiss).and.(TV.ne.bmiss)) TF(K,IST)=TV/(1.+.608*Q)
       endif
c
       if(idbug.eq.1) then
       pval=po(k,ist)
       typx=typ(ist)+100
       if((iprs.gt.1).and.(nvar.eq.1)) pval=pom(k,ist)
       if(iall.eq.1) then
c
       write(6,611) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * k,pval,zo(k,ist),za(k,ist),zf(k,ist),zq(k,ist)
       write(6,612) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * k,pval,to(k,ist),ta(k,ist),tf(k,ist),tq(k,ist)
       write(6,613) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * k,pval,qo(k,ist),qa(k,ist),qf(k,ist),qq(k,ist)
c
       else
c
       write(6,711) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * k,pval,zo(k,ist),zq(k,ist)
       write(6,712) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * k,pval,to(k,ist),tq(k,ist)
       write(6,713) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * k,pval,qo(k,ist),qq(k,ist)
c
       endif
       endif
c
       endif
c
c... now for wind report...
       if((kkx.eq.2).and.(iwnd.eq.1)) then
       kwmax(ist)=k
       kfillw(k,ist)=1
       mwfill(ist)=1
c
       CALL UFBINT(LUNIN,UOB, 4,255,NLEV,USTR)
       CALL UFBINT(LUNIN,VOB, 4,255,NLEV,VSTR)
c
       do i=1,4
       if(uob(i,l).ne.bmiss) upr(i,k)=uob(i,l)
       if(upr(i,k).gt.rmiss) upr(i,k)=bmiss
       if(vob(i,l).ne.bmiss) vpr(i,k)=vob(i,l)
       if(vpr(i,k).gt.rmiss) vpr(i,k)=bmiss
       enddo
c
       UO(K,IST) =  UPR(1,K)
       VO(K,IST) =  VPR(1,K)
       WQ(K,IST) =  UPR(4,K)
c
       if(iall.eq.1) then
       UA(K,IST) =  UPR(2,K)
       UF(K,IST) =  UPR(3,K)
       VA(K,IST) =  VPR(2,K)
       VF(K,IST) =  VPR(3,K)
       endif
c
       if(idbug.eq.1) then
       pval=po(k,ist)
       typx=typ(ist)+200
       if((iprs.gt.1).and.(nvar.eq.1)) pval=pow(k,ist)
       if(iall.eq.1) then
c
       write(6,614) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * k,pval,uo(k,ist),ua(k,ist),uf(k,ist),wq(k,ist)
       write(6,615) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * k,pval,vo(k,ist),va(k,ist),vf(k,ist)
c
       else
c
       write(6,714) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * k,pval,uo(k,ist),wq(k,ist)
       write(6,715) ist,indate,stnid,flon,flat,typx,typintx,elvx,hr,
     * k,pval,vo(k,ist)
c
       endif
       endif
c
       endif
c
c...  end proper level section
       endif
c
c...  end quality mark section
c      endif
c
c... end level-loop
		   ENDDO
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
c
c.... get unique station ids...
c
       do j=1,nstns
        n=0
        idj=tstn(j)
        do i=j+1,nstns
        idi=tstn(i)
        if(idj.eq.idi) then
        n=n+1
        print *,' unique station ',idj,idi,j,i,n
        write(labn,'(i1)') n
        idi(6:6)=labn
        tstn(i)=idi
        endif
        enddo
       enddo
c
       nstf=0
       do ist=1,nstns
c
c...   find a common set of levels
c
       if((iprs.gt.1).and.(nvar.eq.1)) then
c
       im=1
       iw=1
       nl=0
       do k=1,nlevs
c
       zog(k)=bmiss
       tog(k)=bmiss
       qog(k)=bmiss
       zag(k)=bmiss
       tag(k)=bmiss
       qag(k)=bmiss
       zfg(k)=bmiss
       tfg(k)=bmiss
       qfg(k)=bmiss
       zqg(k)=bmiss
       tqg(k)=bmiss
       qqg(k)=bmiss
       uog(k)=bmiss
       vog(k)=bmiss
       uag(k)=bmiss
       vag(k)=bmiss
       ufg(k)=bmiss
       vfg(k)=bmiss
       wqg(k)=bmiss
c
       pm=pom(im,ist)
       pw=pow(iw,ist)
       if(pm.eq.bmiss.and.pw.eq.bmiss) go to 11
       if(pm.ne.bmiss.and.pw.eq.bmiss) then
       pog(k)=pm
       zog(k)=zo(im,ist)
       tog(k)=to(im,ist)
       qog(k)=qo(im,ist)
       zag(k)=za(im,ist)
       tag(k)=ta(im,ist)
       qag(k)=qa(im,ist)
       zfg(k)=zf(im,ist)
       tfg(k)=tf(im,ist)
       qfg(k)=qf(im,ist)
       zqg(k)=zq(im,ist)
       tqg(k)=tq(im,ist)
       qqg(k)=qq(im,ist)
       im=im+1
       nl=nl+1
       go to 11
       endif
       if(pm.eq.bmiss.and.pw.ne.bmiss) then
       pog(k)=pw
       uog(k)=uo(iw,ist)
       vog(k)=vo(iw,ist)
       uag(k)=ua(iw,ist)
       vag(k)=va(iw,ist)
       ufg(k)=uf(iw,ist)
       vfg(k)=vf(iw,ist)
       wqg(k)=wq(iw,ist)
       iw=iw+1
       nl=nl+1
       go to 11
       endif
c
       nl=nl+1
       if(pm.gt.pw) then
       pog(k)=pm
       zog(k)=zo(im,ist)
       tog(k)=to(im,ist)
       qog(k)=qo(im,ist)
       zag(k)=za(im,ist)
       tag(k)=ta(im,ist)
       qag(k)=qa(im,ist)
       zfg(k)=zf(im,ist)
       tfg(k)=tf(im,ist)
       qfg(k)=qf(im,ist)
       zqg(k)=zq(im,ist)
       tqg(k)=tq(im,ist)
       qqg(k)=qq(im,ist)
       im=im+1
       endif
       if(pm.eq.pw) then
       pog(k)=pm
       zog(k)=zo(im,ist)
       tog(k)=to(im,ist)
       qog(k)=qo(im,ist)
       zag(k)=za(im,ist)
       tag(k)=ta(im,ist)
       qag(k)=qa(im,ist)
       zfg(k)=zf(im,ist)
       tfg(k)=tf(im,ist)
       qfg(k)=qf(im,ist)
       zqg(k)=zq(im,ist)
       tqg(k)=tq(im,ist)
       qqg(k)=qq(im,ist)
       uog(k)=uo(iw,ist)
       vog(k)=vo(iw,ist)
       uag(k)=ua(iw,ist)
       vag(k)=va(iw,ist)
       ufg(k)=uf(iw,ist)
       vfg(k)=vf(iw,ist)
       wqg(k)=wq(iw,ist)
       im=im+1
       iw=iw+1
       endif
       if(pm.lt.pw) then
       pog(k)=pw
       uog(k)=uo(iw,ist)
       vog(k)=vo(iw,ist)
       uag(k)=ua(iw,ist)
       vag(k)=va(iw,ist)
       ufg(k)=uf(iw,ist)
       vfg(k)=vf(iw,ist)
       wqg(k)=wq(iw,ist)
       iw=iw+1
       endif
 11    continue
       enddo
c
       else
c
c... check how many levels are there...
       nl=0
       do l=1,nlevs
       pdata=po(l,ist)
       if(ist.eq.1) print *,l,pdata
       if(pdata.ne.bmiss) then
       nl=nl+1
       pog(nl)=po(l,ist)
       if(imas.eq.1) then
       zog(nl)=zo(l,ist)
       tog(nl)=to(l,ist)
       qog(nl)=qo(l,ist)
       zag(nl)=za(l,ist)
       tag(nl)=ta(l,ist)
       qag(nl)=qa(l,ist)
       zfg(nl)=zf(l,ist)
       tfg(nl)=tf(l,ist)
       qfg(nl)=qf(l,ist)
       zqg(nl)=zq(l,ist)
       tqg(nl)=tq(l,ist)
       qqg(nl)=qq(l,ist)
       endif
       if(iwnd.eq.1) then
       uog(nl)=uo(l,ist)
       vog(nl)=vo(l,ist)
       uag(nl)=ua(l,ist)
       vag(nl)=va(l,ist)
       ufg(nl)=uf(l,ist)
       vfg(nl)=vf(l,ist)
       wqg(nl)=wq(l,ist)
       endif
       endif
       enddo
c
       endif
c
       print *,indate,' number of levels for station ',ist,' ',nl
       if(nl.eq.0) go to 1234
       nstf=nstf+1
c
c....  write grads data out...
c
       id(1:8)='        '
       id=tstn(ist)
       rlon=tlon(ist)
       rlat=tlat(ist)
       hr=thr(ist)
       typx=typ(ist)
       typintx=typint(ist)
       elvx=elvt(ist)
       psog=pso(ist)
       psag=psa(ist)
       psfg=psf(ist)
       psqg=psq(ist)
       iflag=0
       id(7:7)=' '
       id(8:8)='\0'
c
       rt=0.0
       write(51) id,rlat,rlon,rt,nl,iflag
c
       if(iall.eq.1) then
c
       if((imas.eq.1).and.(iwnd.eq.1)) then
       write(51) (pog(l),
     *            typx,typintx,elvx,psog,psag,psfg,psqg,pog(l),
     *            zog(l),zag(l),zfg(l),zqg(l),
     *            tog(l),tag(l),tfg(l),tqg(l),
     *            qog(l),qag(l),qfg(l),qqg(l),
     *            uog(l),uag(l),ufg(l),wqg(l),
     *            vog(l),vag(l),vfg(l),l=1,nl)

       if(idbuga.eq.1) then
       print *,id,rlat,rlon,rt,nl,iflag
       do l=1,nl
       write(6,411) indate,hr,psog,psag,psfg,psqg,pog(l),
     *              zog(l),zag(l),zfg(l),zqg(l),
     *              tog(l),tag(l),tfg(l),tqg(l),
     *              qog(l),qag(l),qfg(l),qqg(l),
     *              uog(l),uag(l),ufg(l),wqg(l),
     *              vog(l),vag(l),vfg(l)
       enddo
       endif
       endif
       if((imas.eq.1).and.(iwnd.eq.0)) then
       write(51) (pog(l),
     *            typx,typintx,elvx,psog,psag,psfg,psqg,pog(l),
     *            zog(l),zag(l),zfg(l),zqg(l),
     *            tog(l),tag(l),tfg(l),tqg(l),
     *            qog(l),qag(l),qfg(l),qqg(l),l=1,nl)
       if(idbuga.eq.1) then
       print *,id,rlat,rlon,rt,nl,iflag
       do l=1,nl
       write(6,412) indate,hr,psog,psag,psfg,psqg,pog(l),
     *              zog(l),zag(l),zfg(l),zqg(l),
     *              tog(l),tag(l),tfg(l),tqg(l),
     *              qog(l),qag(l),qfg(l),qqg(l)
       enddo
       endif
       endif
       if((imas.eq.0).and.(iwnd.eq.1)) then
       write(51) (pog(l),
     *            typx,typintx,elvx,psog,psag,psfg,psqg,pog(l),
     *            uog(l),uag(l),ufg(l),wqg(l),
     *            vog(l),vag(l),vfg(l),l=1,nl)
       if(idbuga.eq.1) then
       print *,id,rlat,rlon,rt,nl,iflag
       do l=1,nl
       write(6,413) indate,hr,psog,psag,psfg,psqg,pog(l),
     *              uog(l),uag(l),ufg(l),wqg(l),
     *              vog(l),vag(l),vfg(l)
       enddo
       endif
       endif
c
       else
c
       if((imas.eq.1).and.(iwnd.eq.1)) then
       write(51) (pog(l),
     *            typx,typintx,elvx,psog,psqg,pog(l),
     *            zog(l),zqg(l),
     *            tog(l),tqg(l),
     *            qog(l),qqg(l),
     *            uog(l),wqg(l),
     *            vog(l),l=1,nl)
       if(idbuga.eq.1) then
       print *,id,rlat,rlon,rt,nl,iflag
       do l=1,nl
       write(6,511) indate,hr,psog,psqg,pog(l),
     *              zog(l),zqg(l),
     *              tog(l),tqg(l),
     *              qog(l),qqg(l),
     *              uog(l),wqg(l),
     *              vog(l)
       enddo
       endif
       endif
       if((imas.eq.1).and.(iwnd.eq.0)) then
       write(51) (pog(l),
     *            typx,typintx,elvx,psog,psqg,pog(l),
     *            zog(l),zqg(l),
     *            tog(l),tqg(l),
     *            qog(l),qqg(l),l=1,nl)
       if(idbuga.eq.1) then
       print *,id,rlat,rlon,rt,nl,iflag
       do l=1,nl
       write(6,512) indate,hr,psog,psqg,pog(l),
     *              zog(l),zqg(l),
     *              tog(l),tqg(l),
     *              qog(l),qqg(l)
       enddo
       endif
       endif
       if((imas.eq.0).and.(iwnd.eq.1)) then
       write(51) (pog(l),
     *            typx,typintx,elvx,psog,psqg,pog(l),
     *            uog(l),wqg(l),
     *            vog(l),l=1,nl)
       if(idbuga.eq.1) then
       print *,id,rlat,rlon,rt,nl,iflag
       do l=1,nl
       write(6,513) indate,hr,psog,psqg,pog(l),
     *              uog(l),wqg(l),
     *              vog(l)
       enddo
       endif
       endif
c
       endif
c
 1234  continue
c... end station-loop
        enddo
c
       print *,indate,' total number of stations for grads ',nstf
c
c..  write footer to end this time level
       id='       '
       rlat=0.
       rlon=0.
       rt=0.
       nlev=0
       iflag=0
       write(51) id,rlat,rlon,rt,nlev,iflag
c
       CALL CLOSBF(LUNIN)
       close(51)
c
 411   format(a10,1x,f5.2,1x,4f6.1,1x,f6.1,' mb ',1x,
     *        4(3e10.2,1x,f3.0,/),3e10.2)
 412   format(a10,1x,f5.2,1x,4f6.1,1x,f6.1,' mb ',1x,
     *        3(3e10.2,1x,f3.0,/))
 413   format(a10,1x,f5.2,1x,4f6.1,1x,f6.1,' mb ',1x,
     *        3e10.2,1x,f3.0,1x,3e10.2)
c
 511   format(a10,1x,f5.2,1x,2f6.1,1x,f6.1,' mb ',1x,
     *        2(e10.2,1x,f3.0),1x,e10.2)
 512   format(a10,1x,f5.2,1x,2f6.1,1x,f6.1,' mb ',1x,
     *        3(e10.2,1x,f3.0))
 513   format(a10,1x,f5.2,1x,2f6.1,1x,f6.1,' mb ',1x,
     *        e10.2,1x,f3.0,1x,e10.2)
c
 610   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'ps',1x,3f10.2,1x,'qf ',f4.0)
 611   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'z',1x,i2,1x,f6.1,1x,3f10.2,1x,'qf ',f4.0)
 612   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'t',1x,i2,1x,f6.1,1x,3f10.2,1x,'qf ',f4.0)
 613   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'q',1x,i2,1x,f6.1,1x,3f10.2,1x,'qf ',f4.0)
 614   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'u',1x,i2,1x,f6.1,1x,3f10.2,1x,'qf ',f4.0)
 615   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'v',1x,i2,1x,f6.1,1x,3f10.2)
c
 711   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'z',1x,i2,1x,f6.1,1x,f10.2,1x,'qf ',f4.0)
 712   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'t',1x,i2,1x,f6.1,1x,f10.2,1x,'qf ',f4.0)
 713   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'q',1x,i2,1x,f6.1,1x,f10.2,1x,'qf ',f4.0)
 714   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'u',1x,i2,1x,f6.1,1x,f10.2,1x,'qf ',f4.0)
 715   format(i6,1x,a10,1x,a8,1x,f6.2,1x,f6.2,1x,f4.0,1x,f4.0,1x,f6.1,
     * 1x,f5.2,1x,'v',1x,i2,1x,f6.1,1x,f10.2)
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
      FUNCTION GETLEV(P,plev,itol,nlevs,kfill,ist,nst)
c
      dimension plev(nlevs)
      integer kfill(nlevs,nst)
c
       IP = NINT(P)
c
c..  first check if exact match exists...
       do nl=1,nlevs
       getlev=0
       lsig=plev(nl)
c
       if((ip.eq.lsig).and.(kfill(nl,ist).eq.0)) then
       getlev=nl
c      print *,'from getlev exact ',nl,p,ip,kk,getlev
       return
       endif
c
c      print *,'from getlev exact ',nl,p,ip,kk,getlev
       enddo
c
       if(itol.eq.0) return
c
       do nl=1,nlevs
       getlev=0
       lsig=plev(nl)
c
       do kk=lsig-itol,lsig+itol
       if((ip.eq.kk).and.(kfill(nl,ist).eq.0)) then
       getlev=nl
c      print *,'from getlev tol ',nl,p,ip,kk,getlev
       return
       endif
       enddo
c
c      print *,'from getlev tol ',nl,p,ip,kk,getlev
       enddo
c
      RETURN
      END
