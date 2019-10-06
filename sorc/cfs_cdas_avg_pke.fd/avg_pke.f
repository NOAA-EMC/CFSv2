      program avg_pke
      implicit none
      integer, parameter :: komax=200, kom37=komax-37

      CHARACTER*120 indir,iname,index,indxdir
      CHARACTER*10 cdump
      real dlat
!
      real, allocatable :: deg(:)
      real po(komax)
!
      integer            syear, smonth, sday, shour
     &,                  eyear, emonth, eday, ehour
     &,                  dhour, fhour,  io, jo, ko, idbug
!
      logical fcst_avrg
!
       namelist /nampke/ io, jo, ko, po, idbug
     &,                  indir,  indxdir, index, iname, cdump
     &,                  syear,  smonth, sday, shour
     &,                  eyear,  emonth, eday, ehour
     &,                  dhour,  fhour,  fcst_avrg
!
      data idbug/0/, io/144/, jo/73/, ko/37/
      data syear/0/, smonth/0/, sday/0/, shour/0/
      data eyear/0/, emonth/0/, eday/0/, ehour/0/, dhour/0/, fhour/0/
!
      data po/1000,975,950,925,900,875,850,825,800,775,750,700,
     &         650,600,550,500,450,400,350,300,250,225,200,175,
     &         150,125,100,70,50,30,20,10,7,5,3,2,1,kom37*0/
!
      integer j,ierr,nrank,ntask

      include 'mpif.h'
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      call mpi_init(ierr)
      call mpi_comm_rank(MPI_COMM_WORLD,nrank,ierr)
      call mpi_comm_size(MPI_COMM_WORLD,ntask,ierr)
      !!if(ierr/=0.or.nrank>0) goto 99

      indir     = 'indir'
      indxdir   = 'indxdir'
      index     = 'index'
      iname     = 'iname'
      cdump     = 'gdas'
      fcst_avrg = .false.

      open(5,file='nampke')
      read (5, nampke)
      write(6, nampke)

      allocate (deg(jo))
      dlat = 180.0 / float(jo-1)
      do j=1,jo
        deg(j) = 90.0 - float(j-1)*dlat
      enddo

      print*,'call avg'
      call avg(io,    jo,     ko,   indir, indxdir, fhour
     &,         syear, smonth, sday, shour
     &,         eyear, emonth, eday, ehour, dhour
     &,         iname,idbug,deg,po,cdump,index,fcst_avrg)

99    call mpi_finalize(ierr)
      stop
      end
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      subroutine avg(idim,jdim,kdim,inputdir,indxdir,ifhr,
     &                isyr,ismth,isday,iscy,ieyr,iemth,ieday,iecy,idcy,
     &                iname,idbug,deg,po,cdump,index,fcst_avrg)
!
      implicit none
!
      logical fcst_avrg
      integer idim, jdim,  kdim, ifhr, isyr, ismth, isday, iscy
     &,        ieyr, iemth, ieday, iecy, idcy, idbug
      CHARACTER*120 inputdir,iname,ingrib,indfile,index,indexx,indxdir
      CHARACTER*120 dgout
      character*4  cycl(25),cyclx
      CHARACTER*10 indate
      CHARACTER*10 cdump
      CHARACTER*4  LABY
      CHARACTER*2  LABM(12)
      CHARACTER*2  LABD(31)
      CHARACTER*2  LABC(24)
!
      real psmean(idim,jdim)
      real umean(idim,jdim,kdim),  vmean(idim,jdim,kdim)
      real tmean(idim,jdim,kdim),  qmean(idim,jdim,kdim)
      real wmean(idim,jdim,kdim),  uvar(idim,jdim,kdim)
      real vvar(idim,jdim,kdim),   uvmean(idim,jdim,kdim)
     &,    tvar(idim,jdim,kdim),   qvar(idim,jdim,kdim)
     &,    wvar(idim,jdim,kdim),   psvar(idim,jdim)
      real utmean(idim,jdim,kdim), uqmean(idim,jdim,kdim)
      real vtmean(idim,jdim,kdim), wtmean(idim,jdim,kdim)
      real wqmean(idim,jdim,kdim), vqmean(idim,jdim,kdim)
!
      real up(idim,jdim,kdim),     vp(idim,jdim,kdim)
     &,    tp(idim,jdim,kdim),     qp(idim,jdim,kdim)
     &,    wp(idim,jdim,kdim),     psp(idim,jdim)
     &,    tke(idim,jdim,kdim),    upvp(idim,jdim,kdim)
     &,    uptp(idim,jdim,kdim),   upqp(idim,jdim,kdim)
     &,    vptp(idim,jdim,kdim),   vpqp(idim,jdim,kdim)
     &,    wptp(idim,jdim,kdim),   wpqp(idim,jdim,kdim)
!
      real gridu(idim,jdim), gridv(idim,jdim), gridt(idim,jdim)
      real                   gridq(idim,jdim), gridw(idim,jdim)
      real gridps(idim,jdim)
!     real gridr(idim,jdim), gridq(idim,jdim), gridw(idim,jdim)
      real deg(jdim)

      real po(kdim)
      integer KPDS(200),  KGDS(200)
      integer KPDSX(200), KGDSX(200)
      integer JPDS(200),  JGDS(200)
      integer ICYCL(24)
      integer nfill, iw3jdn
!
      logical*1 lbms(idim,jdim)
!
      DATA LABC/'00','01','02','03','04','05',
     &          '06','07','08','09','10','11',
     &          '12','13','14','15','16','17',
     &          '18','19','20','21','22','23'/
      DATA ICYCL/0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,
     &          12,13,14,15,16,17,18,19,20,21,22,23/
      data cycl/'.00Z','.01Z','.02Z','.03Z','.04Z',
     &          '.05Z','.06Z','.07Z','.08Z','.09Z',
     &          '.10Z','.11Z','.12Z','.13Z','.14Z',
     &          '.15Z','.16Z','.17Z','.18Z','.19Z',
     &          '.20Z','.21Z','.22Z','.23Z','    '/
!
      DATA LABD/'01','02','03','04','05','06','07','08','09','10',
     &          '11','12','13','14','15','16','17','18','19','20',
     &          '21','22','23','24','25','26','27','28','29','30',
     &          '31'/
!
      DATA LABM/'01','02','03','04','05','06','07','08','09','10',
     &          '11','12'/
!
      real    globu, globv, globt, globq, globw, globr
     &,       fntmx, um, vm, tm, wm, qm, uvm, utm, vtm, wtm
     &,       uu, vv, tt, qq, ww, ek, uqm, vqm, wqm, psm
     &,       u, v, t, q, w, ps
      integer ncns, ncne, ncn, ndays, icomm, m1, m2, iret
     &,       i, j, k, nl, n, ntmx, icy, inx, plev, kskp
     &,       idayyr, iday, idaywk, imth, iyr, kpds5, ndata
     &,       ntask, ierr, nrank, iyb, im, item

      include 'mpif.h'
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       call mpi_comm_rank(MPI_COMM_WORLD,nrank,ierr)
       call mpi_comm_size(MPI_COMM_WORLD,ntask,ierr)
       print*,' task ',nrank,' of ',ntask

       indate='          '

       ncns  = iw3jdn(isyr,ismth,isday)
       ncne  = iw3jdn(ieyr,iemth,ieday)
       ndays = ncne - ncns + 1

       if(nrank==0) then
       print *,' ncns ',ncns,' ncne ',ncne,' ndays ',ndays, nrank
       print *,' ncns ',ncns,' ncne ',ncne,' ndays ',ndays
       print *,' iscy ',iscy,' iecy ',iecy
       print *,' idim ',idim,' jdim ',jdim,' kdim ',kdim
       print *,' ifhr ',ifhr
       print *,' iname ',iname
       print *,' inputdir ',inputdir
       print *,' indxdir ',indxdir
       endif

       m1 = nint(float(kdim* nrank   )/float(ntask)) + 1
       m2 = nint(float(kdim*(nrank+1))/float(ntask))

       print*,m1,m2,nrank,ntask

       if (nrank == 0) call w3tagb('avg_pke',2007,0332,0055,'NP23')

       write(dgout,'(''dgout.'',i3.3)') nrank
       write(*,*) "dgout= ",dgout
       call baopenwt(51,dgout,ierr)
         if(ierr.ne.0) then
         print *,'error opening file ',dgout
         goto 99
       endif

       psmean = 0.0
       psvar  = 0.0
       do nl=1,kdim
         do j=1,jdim
           do i=1,idim
             umean(i,j,nl)  = 0.0
             vmean(i,j,nl)  = 0.0
             tmean(i,j,nl)  = 0.0
             qmean(i,j,nl)  = 0.0
             wmean(i,j,nl)  = 0.0
             uvar(i,j,nl)   = 0.0
             vvar(i,j,nl)   = 0.0
             tvar(i,j,nl)   = 0.0
             qvar(i,j,nl)   = 0.0
             wvar(i,j,nl)   = 0.0
             uvmean(i,j,nl) = 0.0
             utmean(i,j,nl) = 0.0
             uqmean(i,j,nl) = 0.0
             vtmean(i,j,nl) = 0.0
             vqmean(i,j,nl) = 0.0
             wtmean(i,j,nl) = 0.0
             wqmean(i,j,nl) = 0.0
           enddo
         enddo
       enddo
!
!      START OF THE TIME LOOP
!
       ntmx=0
       DO NCN=NCNS,NCNE
!
         CALL W3FS26(NCN,IYR,IMTH,IDAY,IDAYWK,IDAYYR)
         WRITE(LABY,'(I4)') IYR
         indate(1:4) = LABY
         indate(5:6) = LABM(IMTH)
         indate(7:8) = LABD(IDAY)
!
         do icy=iscy,iecy,idcy
           ntmx = ntmx + 1
!
           i = icy + 1
           indate(9:10) = LABC(i)
!
           ingrib=inputdir(1:nfill(inputdir))//'/'// 
     &     iname(1:nfill(iname))//'.'//cdump(1:nfill(cdump))//
     &     '.'//indate
           write(*,*) "ingrib= ",ingrib
           call baopenr(11,ingrib,ierr)
           if(ierr.ne.0) then
             print *,'error opening file ',ingrib
             goto 99
           endif
           inx = 0
           if (len(trim(index)) .ne. 0 ) then
             inx = 12
             indexx=indxdir(1:nfill(indxdir))//'/'// 
     &       index(1:nfill(index))//'.'//cdump(1:nfill(cdump))//
     &       '.'//indate
             write(*,*) "indexx= ",indexx
             call baopenr(inx,indexx,ierr)
             if(ierr.ne.0) then
               print *,'error opening file ',indexx
               goto 99
             endif
           endif
!
           do nl = m1,m2                     ! first do loop for kk record
!          do nl=1,kdim
             plev = nint(po(nl))
!
!.... get u-wind

             do k=1,200
               JPDS(k) = -1
               JGDS(k) = -1
             enddo
             JPDS(5)  = 33
             JPDS(6)  = 100
             JPDS(7)  = plev
             JPDS(8)  = mod(IYR-1,100) + 1
             JPDS(9)  = IMTH
             JPDS(10) = IDAY
             JPDS(11) = ICY
             JPDS(14) = ifhr
             JPDS(21) = ((IYR-1)/100) + 1
             N = -1
             CALL GETGB(11,inx,idim*jdim,N,JPDS,JGDS,
     &                  NDATA,KSKP,KPDS,KGDS,LBMS,GRIDU,IRET)
             if(iret.ne.0) then
               print *,nl,' error in GETGB for uwind ',iret,jpds
               goto 99
             endif
!
             
!
             if (ntmx == 1) then
               do k=1,200
                 KPDSX(k)=KPDS(k)
                 KGDSX(k)=KGDS(k)
               enddo
               iyb = (kpds(21)-1)*100 + kpds(8)
               im = kpds(9)
             endif
!
!... get v-wind
             do k=1,200
               JPDS(k) = -1
               JGDS(k) = -1
             enddo
             JPDS(5)  = 34
             JPDS(6)  = 100
             JPDS(7)  = plev
             JPDS(8)  = mod(IYR-1,100) + 1
             JPDS(9)  = IMTH
             JPDS(10) = IDAY
             JPDS(11) = ICY
             JPDS(14) = ifhr
             JPDS(21) = ((IYR-1)/100) + 1
             N = -1
             CALL GETGB(11,inx,idim*jdim,N,JPDS,JGDS,
     &                  NDATA,KSKP,KPDS,KGDS,LBMS,GRIDV,IRET)
             if(iret.ne.0) then
               print *,nl,' error in GETGB for vwind ',iret,jpds
               goto 99
             endif
!
!... get Temp...
             do k=1,200
               JPDS(k) = -1
               JGDS(k) = -1
             enddo
             JPDS(5)  = 11
             JPDS(7)  = plev
             JPDS(8)  = mod(IYR-1,100) + 1
             JPDS(9)  = IMTH
             JPDS(10) = IDAY
             JPDS(11) = ICY
             JPDS(14) = ifhr
             JPDS(21) = ((IYR-1)/100) + 1
             N = -1
             CALL GETGB(11,inx,idim*jdim,N,JPDS,JGDS,
     &                  NDATA,KSKP,KPDS,KGDS,LBMS,GRIDT,IRET)
             if(iret.ne.0) then
               print *,nl,' error in GETGB for temp ',iret,jpds
               call abort
             endif
!
!... get rh
!            do k=1,200
!              JPDS(k) = -1
!              JGDS(k) = -1
!            enddo
!            JPDS(5)  = 52
!            JPDS(7)  = plev
!            JPDS(8)  = mod(IYR-1,100) + 1
!            JPDS(9)  = IMTH
!            JPDS(10) = IDAY
!            JPDS(11) = ICY
!            JPDS(14) = ifhr
!            JPDS(21) = ((IYR-1)/100) + 1
!            N = -1
!            CALL GETGB(11,inx,idim*jdim,N,JPDS,JGDS,
!    &                  NDATA,KSKP,KPDS,KGDS,LBMS,GRIDR,IRET)
!            if(iret.ne.0) then
!              print *,nl,' error in GETGB for moisture ',iret,jpds
!              call abort
!            endif
!
!... get vertical velocity
             do k=1,200
               JPDS(k) = -1
               JGDS(k) = -1
             enddo
             JPDS(5)  = 39
             JPDS(7)  = plev
             JPDS(8)  = mod(IYR-1,100) + 1
             JPDS(9)  = IMTH
             JPDS(10) = IDAY
             JPDS(11) = ICY
             JPDS(14) = ifhr
             JPDS(21) = ((IYR-1)/100) + 1
             N = -1
!            if (plev > 90 ) then
               CALL GETGB(11,inx,idim*jdim,N,JPDS,JGDS,
     &                    NDATA,KSKP,KPDS,KGDS,LBMS,GRIDW,IRET)
               if(iret.ne.0) then
                 print *,nl,' error in GETGB for omega ',iret,jpds
                 call abort
               endif
!            else
!              gridw = 0.0
!            endif
!
!... get specific humidity
             do k=1,200
               JPDS(k) = -1
               JGDS(k) = -1
             enddo
             JPDS(5)  = 51
             JPDS(7)  = plev
             JPDS(8)  = mod(IYR-1,100) + 1
             JPDS(9)  = IMTH
             JPDS(10) = IDAY
             JPDS(11) = ICY
             JPDS(14) = ifhr
             JPDS(21) = ((IYR-1)/100) + 1
             N = -1
             CALL GETGB(11,inx,idim*jdim,N,JPDS,JGDS,
     &                  NDATA,KSKP,KPDS,KGDS,LBMS,GRIDQ,IRET)
             if(iret.ne.0) then
               print *,nl,' error in GETGB for q ',iret,jpds
               call abort
             endif
!
             if(IDBUG.eq.1) then
               CALL GRIDAV(GRIDU,IDIM,JDIM,DEG,GLOBU)
               CALL GRIDAV(GRIDV,IDIM,JDIM,DEG,GLOBV)
               CALL GRIDAV(GRIDT,IDIM,JDIM,DEG,GLOBT)
               globr = 0.
               globw = 0.
               globq = 0.
               if(plev.ge.100) then
                 CALL GRIDAV(GRIDW,IDIM,JDIM,DEG,GLOBW)
               endif
!              CALL GRIDAV(GRIDR,IDIM,JDIM,DEG,GLOBR)
               CALL GRIDAV(GRIDQ,IDIM,JDIM,DEG,GLOBQ)
               print *,indate,' lev ',plev,globu,globv,globt,globr
     &,                               globq,globw
             endif
!
             do j=1,jdim
               do i=1,idim
                 u = gridu(i,j)
                 v = gridv(i,j)
                 t = gridt(i,j)
                 q = gridq(i,j)
                 w = gridw(i,j)
                 umean(i,j,nl)  = umean(i,j,nl)  + u
                 vmean(i,j,nl)  = vmean(i,j,nl)  + v
                 tmean(i,j,nl)  = tmean(i,j,nl)  + t
                 qmean(i,j,nl)  = qmean(i,j,nl)  + q
                 wmean(i,j,nl)  = wmean(i,j,nl)  + w
                 uvar(i,j,nl)   = uvar(i,j,nl)   + u * u
                 vvar(i,j,nl)   = vvar(i,j,nl)   + v * v
                 tvar(i,j,nl)   = tvar(i,j,nl)   + t * t
                 qvar(i,j,nl)   = qvar(i,j,nl)   + q * q
                 wvar(i,j,nl)   = wvar(i,j,nl)   + w * w
                 uvmean(i,j,nl) = uvmean(i,j,nl) + u * v
                 utmean(i,j,nl) = utmean(i,j,nl) + u * t
                 uqmean(i,j,nl) = uqmean(i,j,nl) + u * q
                 vtmean(i,j,nl) = vtmean(i,j,nl) + v * t
                 vqmean(i,j,nl) = vqmean(i,j,nl) + v * q
                 wtmean(i,j,nl) = wtmean(i,j,nl) + w * t
                 wqmean(i,j,nl) = wqmean(i,j,nl) + w * q
               enddo
             enddo

           enddo                             !... end level-loop
!
!.... get surface pressure

           if (nrank == 0) then
             do k=1,200
               JPDS(k) = -1
               JGDS(k) = -1
             enddo
             JPDS(5)  = 1
             JPDS(6)  = 1
             JPDS(7)  = 0
             JPDS(8)  = mod(IYR-1,100) + 1
             JPDS(9)  = IMTH
             JPDS(10) = IDAY
             JPDS(11) = ICY
             JPDS(14) = ifhr
             JPDS(21) = ((IYR-1)/100) + 1
             N = -1
             CALL GETGB(11,inx,idim*jdim,N,JPDS,JGDS,
     &                  NDATA,KSKP,KPDS,KGDS,LBMS,gridps,IRET)
             if(iret.ne.0) then
               print *,nl,' error in GETGB for sfc ps ',iret,jpds
               goto 99
             endif
             do j=1,jdim
               do i=1,idim
                 ps = gridps(i,j)
                 psmean(i,j)  = psmean(i,j) + ps
                 psvar(i,j)   = psvar(i,j)  + ps * ps
                 if (i == 1 .and. j == 40) print *,' ps=',ps
               enddo
             enddo
           endif
!
           call baclose(11,ierr)
           if(ierr.ne.0) then
             print *,'error closing file ',ingrib
             goto 99
           endif
!
         enddo                               !... end cycle-loop
       enddo                                 !... end day-loop
!
       print *,' number of time levels is ',ntmx
       fntmx=1.0/ntmx
!
       if (nrank == 0) then
         psp = 0.0
         do j=1,jdim
           do i=1,idim
             psm = psmean(i,j) * fntmx
             psp(i,j) =  psvar(i,j)*fntmx - psm * psm
             if (i == 1 .and. j == 40) print *,' psm=',psm,
     &' psvar=',psvar(i,j)
           enddo
         enddo
       endif
          
       do nl=m1,m2
         plev = nint(po(nl))
         do j=1,jdim
           do i=1,idim
             up(i,j,nl)   = 0.
             vp(i,j,nl)   = 0.
             tp(i,j,nl)   = 0.
             qp(i,j,nl)   = 0.
             wp(i,j,nl)   = 0.
             tke(i,j,nl)  = 0.
             upvp(i,j,nl) = 0.
             uptp(i,j,nl) = 0.0
             vptp(i,j,nl) = 0.0
!
             um  = umean(i,j,nl)  * fntmx
             vm  = vmean(i,j,nl)  * fntmx
             tm  = tmean(i,j,nl)  * fntmx
             qm  = qmean(i,j,nl)  * fntmx
             wm  = wmean(i,j,nl)  * fntmx
             uvm = uvmean(i,j,nl) * fntmx
             utm = utmean(i,j,nl) * fntmx
             vtm = vtmean(i,j,nl) * fntmx
             uu  = uvar(i,j,nl)   * fntmx
             vv  = vvar(i,j,nl)   * fntmx
             tt  = tvar(i,j,nl)   * fntmx
             qq  = qvar(i,j,nl)   * fntmx
             ww  = wvar(i,j,nl)   * fntmx
!
             up(i,j,nl)   = uu - um*um
             vp(i,j,nl)   = vv - vm*vm
             tp(i,j,nl)   = tt - tm*tm
             qp(i,j,nl)   = qq - qm*qm
             wp(i,j,nl)   = ww - wm*wm
             ek           = 0.5 * (uu+vv)
             tke(i,j,nl)  = tke(i,j,nl) + ek
             upvp(i,j,nl) = uvm - um*vm
             uptp(i,j,nl) = utm - um*tm
             vptp(i,j,nl) = vtm - vm*tm
!
             upqp(i,j,nl) = 0.0
             vpqp(i,j,nl) = 0.0
             wptp(i,j,nl) = 0.0
             wpqp(i,j,nl) = 0.0
             qm  = qmean(i,j,nl)  * fntmx
             wm  = wmean(i,j,nl)  * fntmx
             uqm = uqmean(i,j,nl) * fntmx
             wtm = wtmean(i,j,nl) * fntmx
             wqm = wqmean(i,j,nl) * fntmx
             upqp(i,j,nl) = uqm - um*qm
             vpqp(i,j,nl) = vqm - vm*qm
             wptp(i,j,nl) = wtm - wm*tm
             wpqp(i,j,nl) = wqm - wm*qm
           enddo
         enddo
       enddo                          !... end level-loop
!
!... now write out stuff..
!
       do k=1,200
         KPDS(k) = KPDSX(k)
         KGDS(k) = KGDSX(k)
       enddo
       if (fcst_avrg) then
          KPDS(13) = 3                ! Forecast time unit - month
          KPDS(14) = (ISYR-IYB)*12 + ISMTH - IM ! Start month
          KPDS(15) =  KPDS(14) + 1              ! End   month
          KPDS(16) = 3                ! Time range inidicator - average
          KPDS(20) = 0                ! Number of missing time
          KPDS(21) = ((IYB-1)/100) + 1
       else
         kpds(14) = ifhr
         item = mod(idcy,24)
         if (item == 0 ) then         !  for time mean for a given cycle
           kpds(15) = 24
         elseif (idcy < 24) then     !  for time mean of daily mean
           kpds(11) = 0
           kpds(15) = idcy
         else
           print *,' Bad choice of idcy =', idcy
         endif
         KPDS(16) = 113               ! Time range inidicator - average
        
       endif

       kpds(17) = ntmx                ! Number of time included in avrg
       KPDS(22) = KPDS(22) + 1
!
!.. tke...
       kpds(5) = 158
       kpds(19) = 2
       kpds(22) = 1
       if(idbug.eq.1) print *,'*****  Total Kinetic Energy '
       call wrtpgb(tke,idim,jdim,kdim,po,
     &             kpds,kgds,deg,ndata,idbug,m1,m2)
!
!.. upvp
       kpds(5) = 150
       kpds(19) = 2
       kpds(22) = 1
       if(idbug.eq.1) print *,'*****  Covariance of U & V '
       call wrtpgb(upvp,idim,jdim,kdim,po,
     &             kpds,kgds,deg,ndata,idbug,m1,m2)
!
!.. uptp
       kpds(5) = 151
       kpds(19) = 2
       kpds(22) = 1
       if(idbug.eq.1) print *,'*****  Covariance of U & T '
       call wrtpgb(uptp,idim,jdim,kdim,po,
     &             kpds,kgds,deg,ndata,idbug,m1,m2)
!
!.. vptp
       kpds(5) = 152
       kpds(19) = 2
       kpds(22) = 1
       if(idbug.eq.1) print *,'*****  Covariance of V & T '
       call wrtpgb(vptp,idim,jdim,kdim,po,
     &             kpds,kgds,deg,ndata,idbug,m1,m2)
!
!.. upqp
       kpds(5)  = 166
       kpds(19) = 133
       kpds(22) = 5
       if(idbug.eq.1) print *,'*****  Covariance of U & Q '
       call wrtpgb(upqp,idim,jdim,kdim,po,
     &             kpds,kgds,deg,ndata,idbug,m1,m2)
!
!.. vpqp
       kpds(5)  = 167
       kpds(19) = 133
       kpds(22) = 5
       if(idbug.eq.1) print *,'*****  Covariance of V & Q '
       call wrtpgb(vpqp,idim,jdim,kdim,po,
     &             kpds,kgds,deg,ndata,idbug,m1,m2)
!
!.. wptp
       kpds(5)  = 168
       kpds(19) = 133
       kpds(22) = 3
       if(idbug.eq.1) print *,'*****  Covariance of W & T '
       call wrtpgb(wptp,idim,jdim,kdim,po,
     &             kpds,kgds,deg,ndata,idbug,m1,m2)
!
!.. wpqp
       kpds(5)  = 169
       kpds(19) = 133
       kpds(22) = 9
       if(idbug.eq.1) print *,'*****  Covariance of W & Q '
       call wrtpgb(wpqp,idim,jdim,kdim,po,
     &             kpds,kgds,deg,ndata,idbug,m1,m2)
!
!.. up...
       kpds(5)  = 164
       kpds(19) = 133
       kpds(22) = 1
       if(idbug.eq.1) print *,'*****  Variance of U '
       call wrtpgb(up,idim,jdim,kdim,po,
     &             kpds,kgds,deg,ndata,idbug,m1,m2)
!
!.. vp...
       kpds(5)  = 165
       kpds(19) = 133
       kpds(22) = 1
       if(idbug.eq.1) print *,'*****  Variance of V '
       call wrtpgb(vp,idim,jdim,kdim,po,
     &             kpds,kgds,deg,ndata,idbug,m1,m2)
!
!.. tp...
       kpds(5)  = 234
       kpds(19) = 133
       kpds(22) = 1
       if(idbug.eq.1) print *,'*****  Variance of T '
       call wrtpgb(tp,idim,jdim,kdim,po,
     &             kpds,kgds,deg,ndata,idbug,m1,m2)
!
!.. qp...
       kpds(5)  = 206
       kpds(19) = 133
       kpds(22) = 9
       if(idbug.eq.1) print *,'*****  Variance of q '
       call wrtpgb(qp,idim,jdim,kdim,po,
     &             kpds,kgds,deg,ndata,idbug,m1,m2)
!
!.. wp...
       kpds(5) = 220
       kpds(19) = 133
       kpds(22) = 5
       if(idbug.eq.1) print *,'*****  Variance of w '
       call wrtpgb(wp,idim,jdim,kdim,po,
     &             kpds,kgds,deg,ndata,idbug,m1,m2)

!.. psp...
       if (nrank == 0) then
         kpds(5) = 203
         kpds(6) = 1
         kpds(19) = 133
         kpds(22) = -3
         if(idbug.eq.1) print *,'*****  Variance of ps '
         call wrtpgb(psp,idim,jdim,1,po,
     &               kpds,kgds,deg,ndata,idbug,1,1)
       endif

       call baclose(51,ierr)
       if(ierr.ne.0) then
         print *,'error closing file ',dgout
          goto 99
       endif

       call mpi_barrier(MPI_COMM_WORLD,ierr)
99     if (nrank == 0) call w3tage('avg_pke')
       return
       end
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
       subroutine wrtpgb(grid,idim,jdim,kdim,po,
     &                   kpds,kgds,deg,ndata,idbug,m1,m2)
!
       implicit none
!
       integer idim, jdim, kdim, ndata, idbug, m1, m2
       real grid(idim,jdim,kdim),  deg(jdim)
!
       integer KPDS(200),KGDS(200)
       real    po(kdim)
       logical lbms(idim,jdim)
       real    glob
       integer nl, iret
!
!      do nl=1,kdim
       do nl=m1,m2
         KPDS(7) = nint(po(nl))
         CALL PUTGB(51,NDATA,KPDS,KGDS,LBMS,GRID(1,1,nl),IRET)
         if(iret.ne.0) then
           print *,kpds(7),' error in PUTGB for : iret ',iret,kpds
           return
         endif
         if(idbug.eq.1) then
           call gridav(grid(1,1,nl),idim,jdim,deg,glob)
           print *,'lev ',kpds(7),' ntime  ',kpds(17),' cmean ',glob
         endif
       enddo
!
       return
       end
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
      SUBROUTINE gridav(z,lon,lat,deg,gridv)
      dimension z(lon,lat),deg(lat)
c
c       grid averaging algorithm: spherical integration
c       not fancy but has a pole correction
c
c       algorithm:
c
c       dtheta = (deg(i+1)-deg(i)/2 + (deg(i) - deg(i-1))/2
c               = (deg(i+1) - deg(i-1))/2
c
c       integral = sum for all point: point_value * dtheta * cos(theta)
c               / sum for all points: dtheta * cos(theta)
c
c       however there is a problem with the poles,
c       point_value*dos(theta) goes to zero
c
c       one solution:
c       let dtheta = (90 degrees - (deg(1)+deg(2))/2)
c       and assume that f*cos(theta) is linear
c
c       and then integrate the over the interval
c       this yields an effective dtheta of
c
c       dth = 0.5 * (90.- (deg(1)+deg(2))/2)**2 / (90.- deg(1))
c       note:
c       grids must from north and work down
c
      gridv=0.
      PI=4.*ATAN(1.)
      X=0.
      W=0.
      DO 11 J=1,LAT
      COSL=COS(DEG(J)*PI/180.)
c       wne: pole problem fix
        if (j.eq.1) then
           dth = 0.5*(90.- (deg(1)+deg(2))/2.)**2 * PI/180.
        else if (j.eq.lat) then
           dth = 0.5*((deg(lat)+deg(lat-1))/2.+90.)**2 * PI/180.
        else
           dth = 0.5 * (deg(j-1) - deg(j+1)) * COSL
        endif
      DO 10 I=1,LON
      X=X+Z(I,J)*DTH
      W=W+DTH
10    CONTINUE
11    CONTINUE
      GRIDV=X/W
      RETURN
      END
