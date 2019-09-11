!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!  the subroutine is to call the read_convdiag for both guess and analysis
!  conventional diagnostic files   
!
!  PRGMMR:   X. Su       ORG: NP20                 Date: 2007-11-15
!
! dwork_anl   : array for surface pressure data from the analysis
! dwork_ges

   program convdiag
   parameter(maxn=1500000,mxtb=1500000,maxsub=49)

   character(8),dimension(maxn) :: awork,cwork
   real(4),dimension(maxn,0:20) :: bwork,dwork

   character(8),dimension(3,mxtb) :: swork
   character(8),dimension(maxsub) :: subsets 
   real(8),dimension(3,mxtb) :: rwork
   equivalence (rwork,swork)

   character(len=50) :: hdstr,obstr,qmstr,pcstr,rcstr,fcstr,errstr,anstr,vqcstr
   character(len=50) :: file_anl,file_ges,xystr
   data file_anl /'diag_conv_anl'/
   data file_ges /'diag_conv_ges'/

   character(8) subset,sid
   integer iwork(max(maxn,mxtb)),iord(mxtb),jord(maxn),kord(maxn)
   integer jnt(300,0:10),knt(300,0:10),nnt(300,0:10),mmt(300,0:10)
   integer nrep(300),nrex(300,2)
   logical match,pilot,duplev(0:10,255),gbak,gana,vbak,vana,oiqc,pobx,zobx
   real(8) obs(10,255),obs_out(10,255),pob_out(10,255),zob_out(10,255)
   real(8) qms(10,255),qms_out(10,255)
   real(8) pcs(10,255),pcs_out(10,255)
   real(8) rcs(10,255),rcs_out(10,255)
   real(8) fcs(10,255),fcs_out(10,255)
   real(8) ans(10,255),ans_out(10,255)
   real(8) hdr(10),rid,bmiss /10e10/
   real(8) err_out(10,255)
   real(8) vqc_out(10,255)
   real(8) xyt(10,255)     

   data gsicode/16/
   data lunin  /20/
   data lunot  /50/
   data nsub   /00/

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

   jnt=0; knt=0; nnt=0; mmt=0; nrep=0; nrex=0; iwrk=1; nmatch=0
   
   bmiss=10e10; call setbmiss(bmiss) ! this sets bufrlib missing value to 10e10

!  read the 1stguess conv_diag events file and sort into shape

   call read_convdiag(file_ges,awork,bwork,mwrk,mmt)
   call read_convdiag(file_anl,cwork,dwork,nwrk,nnt)
   if(mwrk.ne.nwrk) then
      print*,'mwrk,nwrk=',mwrk,nwrk
      call bort('ges and anl diag files not the same size')
   endif; print*,'after READ_CONVDIAG size=',nwrk

!  sort the diag files enough ways to make them line up correctly 

   call orders(0,iwork,awork,kord,nwrk,1,8,2)
   call orders(0,iwork,cwork,jord,nwrk,1,8,2)
   do i=0,8; if(i==5)cycle
   call orders(12,iwork,bwork(1,i),kord,nwrk,1,4,2)
   call orders(12,iwork,dwork(1,i),jord,nwrk,1,4,2)
   enddo
   call orders(10,iwork,awork,kord,nwrk,1,8,2)
   call orders(10,iwork,cwork,jord,nwrk,1,8,2)

!  check to be sure both diag files are the same shape

   do m=0,8 
   if(m==5)cycle
   do n=1,nwrk
   jj=jord(n); kk=kord(n)
   if(bwork(kk,m).ne.dwork(jj,m)) then
      print*,n,m,bwork(kk,m),dwork(jj,m)
      call bort('ges and anl diag files not the same shape')
   endif
   enddo 
   enddo

!  read in the prepbufr station ids

   CALL UFBMEM(LUNIN,0,IMSG,MUNIT)
   CALL UFBTAM(RWORK,3,MXTB,NPRP,'SID IREC ISUB')
   CALL ORDERS(0,IWORK,SWORK,IORD,NPRP,3,8,2)
   print*,'after UFBMEM nprp=',nprp

!  define mnemonic strings to screen the prepbufr data elements

    hdstr = 'SID XOB YOB ELV DHR TYP                        '
    obstr = 'POB QOB TOB ZOB UOB VOB CAT                    '
    qmstr = 'PQM QQM TQM ZQM WQM                            '
    pcstr = 'PPC QPC TPC ZPC WPC                            '
    rcstr = 'PRC QRC TRC ZRC WRC                            '
    fcstr = 'PFC QFC TFC ZFC UFC VFC                        '
    anstr = 'PAN QAN TAN ZAN UAN VAN                        '
    errstr= 'POETU QOETU TOETU WOETU ESBAK RF10M            '
    vqcstr= 'PVWTG QVWTG TVWTG WVWTG PVWTA QVWTA TVWTA WVWTA'
    xystr = 'XDR YDR HRDR                                   '

!  read the prepbufr file in order of station id              

    loopp: DO IPRP=1,NPRP 
    np=iord(iprp);irec=rwork(2,np);isub=rwork(3,np)
    call ufbmms(irec,isub,subset,idate)
    sid=swork(1,np); !print*,sid

    obs_out=bmiss; qms_out=bmiss; err_out=bmiss; vqc_out=bmiss
    pcs_out=bmiss; rcs_out=bmiss
    pob_out=bmiss; zob_out=bmiss
    ans_out=bmiss; fcs_out=bmiss; duplev=.false.

    call ufbint(lunin,hdr,10,1,iret,hdstr)   ! read header from one observation
    call ufbint(lunin,obs,10,255,nlev,obstr) ! read the observation elements
    call ufbint(lunin,qms,10,255,iret,qmstr) ! read the current quality marks
    call ufbint(lunin,xyt,10,255,iret,xystr) ! read balloon drift coordinates

    xob=hdr(2);yob=hdr(3);elv=hdr(4);dhr=hdr(5);typ=hdr(6);ntyp=nint(typ)
    pilot=ntyp.eq.221.or.ntyp.eq.223.or.ntyp.eq.224.or.ntyp.eq.228.or.ntyp.eq.229 

! for each prepbufr report find the matching diagnose records

10  loopd: do n=iwrk,nwrk; nd=jord(n); nb=kord(n)
    !print*,n,iwrk,nwrk,nd,cwork(nd),' ',sid
    if(sid.lt.cwork(nd)) exit loopd  !no (more) diags for this report  
    if(sid.gt.cwork(nd)) iwrk=iwrk+1   
    if(sid.gt.cwork(nd)) goto 10         

    xdw = dwork(nd,3); ydw = dwork(nd,2); edw = dwork(nd,4)          
    ddw = dwork(nd,7); tdw = dwork(nd,1)
    !print*,xdw,ydw,edw,ddw,tdw
    if(tdw<0)cycle

    if(typ==290) elv=0.0 ! windsat elevation in diag file

    match = typ.eq.tdw .and. abs(elv-edw)<=1
    if(ntyp.eq.285) match = typ.eq.tdw
    IF(SUBSET.NE.'ADPUPA') then
       match = match .and. abs(xob-xdw).le..001
       match = match .and. abs(yob-ydw).le..001
       match = match .and. abs(dhr-ddw).le..001
    endif

! GSV0GBRA     3.00   133.00    50.78     4.00 11297.00   215.90 11297.00     1.94  
! T0QVVSRA     4.00   233.00    52.97     0.49 11874.00   196.80 11884.00     2.65     1.00a
! 0S26229Q     4.00   285.00    62.36    19.68     0.00  1006.66    10.00     2.72     2.00a

!   if(typ==233 .and. abs(xob-xdw).le..001 .and. abs(yob-ydw).le..001 &
!   .and. typ==tdw .and. edw==11874 .and. .not.match .and. sid=='T0QVVSRA') &
!   print*,'debug',sid,xob,yob,elv,edw,dhr,typ,match

    if(typ==999 .and. abs(xob-xdw).le..001 .and. abs(yob-ydw).le..001 &
    .and. typ==tdw                      ) &
    print'(a8,6(f8.2,2x),l1)',sid,xob,yob,elv,edw,dhr,ddw,match

! try and match this diagnose record to a prepbufr level

    if(match) then

       nmatch=nmatch+1

       loopl: do l=1,nlev
       ityp = nint(dwork(nd,0))        ! diagnose record type
       pod=dwork(nd,5); pob=obs(1,l)   ! diagnose and prepbufr record
       zod=dwork(nd,6); zob=obs(4,l)   ! diagnose and prepbufr height
       cid=dwork(nd,09)                ! convinfo useage (0=use 100=nouse)
       gcb=bwork(nb,10)                ! gsi usage (-1=nouse 1=use) from ges
       gca=dwork(nd,10)                ! gsi usage (-1=nouse 1=use) from analysis 
       vcb=bwork(nb,11)                ! qc weight from background
       vca=dwork(nd,11)                ! qc weight from analysis
       
! see if the levels match

       if(duplev(ityp,l))cycle ! don't apply duplicate events in the same place

       if(pilot.and.zob.lt.bmiss) then ! check which vertical coordinate applies
          if(zob.eq.elv) zob=zob+10
          if(zob.ne.zod) cycle
       else
          if(pob.ne.pod.and.ntyp.lt.280) cycle
       endif

       if(xyt(1,l)<bmiss .and. xyt(2,l)<bmiss) then
          if(mod(ntyp,100) /= 32) then
             xdr=xyt(1,l); if(abs(xdr-xdw)>.001) cycle
             ydr=xyt(2,l); if(abs(ydr-ydw)>.001) cycle
          endif
       endif

       dwork(nd,1)=-dwork(nd,1) ! set the oi type not to match any other report
       duplev(ityp,l)=.true.    ! duplev marks which events apply to which levels
       duplev(0000,l)=.true.    ! duplev marks which events apply to which levels

! for matching levels apply the qc results where they are relevant

       if(ityp.eq.1) qmin=qms(1,l) ! ps record
       if(ityp.eq.2) qmin=qms(2,l) ! q  record
       if(ityp.eq.3) qmin=qms(3,l) ! t  record
       if(ityp.eq.4) qmin=qms(5,l) ! w  record
       if(ityp.eq.5) qmin=qms(5,l) ! ws record

       gbak=gcb==1.0;gana=gca==1.0;vbak=vcb>=.25;vana=vca>=.25      

       rcout=0; oiqc=qmin>=4.and.qmin<=7

       if(cid==0)then; 
       if(.not.gbak .and. .not.gana) then; qmout=10; rcout=1; else&
       if(.not.gbak .and.      gana) then; qmout=1 ; rcout=2; else&
       if(     gbak .and. .not.gana) then; qmout=10; rcout=3; endif
       if(gana)then
       if(.not.vbak .and. .not.vana) then; qmout=11; rcout=4; else&
       if(.not.vbak .and.      vana) then; qmout=1 ; rcout=5; else&
       if(     vbak .and. .not.vana) then; qmout=11; rcout=6; endif;endif
       if(rcout==0.and.oiqc) then; qmout=qmin-4; rcout=1; endif
       elseif(qmin.le.3    ) then; qmout=9     ; rcout=1; endif

       if(qmout<4.and.vca<.8) qmout=4 ! screen data <80% confidence from fits

! compose any diagnose events for the prepbufr file

       if(ityp.eq.1) then !  ps 
          if(rcout.gt.0.) then
             obs_out(1,l) = obs(1,l) 
             qms_out(1,l) = qmout 
             pcs_out(1,l) = gsicode
             rcs_out(1,l) = rcout 
          endif
          fcs_out(1,l) = bwork(nb,16)     
          ans_out(1,l) = dwork(nd,16)     
          err_out(1,l) = bwork(nb,12)
          vqc_out(1,l) = bwork(nb,11)*100.0
          vqc_out(5,l) = dwork(nd,11)*100.0
       elseif(ityp.eq.2) then !  q  
          if(rcout.gt.0.) then
             obs_out(2,l) = obs(2,l)
             qms_out(2,l) = qmout 
             pcs_out(2,l) = gsicode 
             rcs_out(2,l) = rcout
          endif
          fcs_out(2,l) = max(bwork(nb,16)*1.e6,0.)
          ans_out(2,l) = max(dwork(nd,16)*1.e6,0.)
          err_out(2,l) = (bwork(nb,12)/bwork(nb,17))*10.0
          err_out(5,l) = bwork(nb,17)*1000000.0
          vqc_out(2,l) = bwork(nb,11)*100.0
          vqc_out(6,l) = dwork(nd,11)*100.0
       elseif(ityp.eq.3) then !  t 
          if(rcout.gt.0.) then
             obs_out(3,l) = obs(3,l)
             qms_out(3,l) = qmout 
             pcs_out(3,l) = gsicode
             rcs_out(3,l) = rcout 
          endif
          fcs_out(3,l) = bwork(nb,16)-273.15
          ans_out(3,l) = dwork(nd,16)-273.15
          err_out(3,l) = bwork(nb,12)
          vqc_out(3,l) = bwork(nb,11)*100.0
          vqc_out(7,l) = dwork(nd,11)*100.0
       elseif(ityp.eq.4) then !  uv
          if(rcout.gt.0.) then
             obs_out(5,l) = obs(5,l)
             obs_out(6,l) = obs(6,l)
             qms_out(5,l) = qmout
             pcs_out(5,l) = gsicode
             rcs_out(5,l) = rcout 
          endif
          fcs_out(5,l) = bwork(nb,16)
          fcs_out(6,l) = bwork(nb,18)
          ans_out(5,l) = dwork(nd,16)
          ans_out(6,l) = dwork(nd,18)
          err_out(4,l) = bwork(nb,12)
          err_out(6,l) = bwork(nb,19)
          vqc_out(4,l) = bwork(nb,11)*100.0
          vqc_out(8,l) = dwork(nd,11)*100.0
       elseif(ityp.eq.5) then !  spd
          if(rcout.gt.0.) then
             obs_out(5,l) = 0        
             obs_out(6,l) = bwork(nb,15)
             qms_out(5,l) = qmout 
             pcs_out(5,l) = gsicode
             rcs_out(5,l) = rcout 
          endif
          fcs_out(5,l) = 0        
          fcs_out(6,l) = bwork(nb,16)
          ans_out(5,l) = 0        
          ans_out(6,l) = dwork(nd,16)
          err_out(4,l) = bwork(nb,12)
          err_out(6,l) = bwork(nb,19)
          vqc_out(4,l) = bwork(nb,11)*100.0
          vqc_out(8,l) = dwork(nd,11)*100.0
       else
          print*,'unknown ityp in loopl=',ityp
          call bort('unknown ityp in loopl')
       endif

! check to see if a pressure or height coordinate was modified 

       pobx=pod /= pob.and.max(pod,pob)<bmiss.and.pob_out(1,l)>=bmiss
       zobx=zod /= zob.and.max(zod,zob)<bmiss.and.zob_out(1,l)>=bmiss

       if(pobx.or.zobx)then
       !if(ntyp==250)print'(a8,5(1x,f8.1))',sid,typ,pod,pob,zod,zob
       endif

       if(pobx)nrex(ntyp,1)=nrex(ntyp,1)+1
       if(zobx)nrex(ntyp,2)=nrex(ntyp,2)+1

       if(pobx) pob_out(1,l)=pod
       if(zobx) zob_out(1,l)=zod

! account for this event and end the event and level loops

       knt(ntyp,0000)=knt(ntyp,0000)+1
       knt(ntyp,ityp)=knt(ntyp,ityp)+1
       cycle loopd  
       enddo loopl
       jnt(ntyp,0000)=jnt(ntyp,0000)+1
       jnt(ntyp,ityp)=jnt(ntyp,ityp)+1
    endif
    enddo loopd

! see if any level not found in the convdiag file

      do l=1,nlev
      if(.not.duplev(0,l)) then
         !print*,'---',sid,' ',xob,yob,dhr,typ
         obs_out(1,l)=obs(1,l)
         qms_out(1,l)=9.        
      endif
      enddo

! figure out where to write this report          

      isubset = 0
      do i=1,nsub
      if(subset.eq.subsets(i)) isubset = i
      enddo
      if(isubset.eq.0) then
         if(nsub+1.gt.maxsub) call bort('nsub too big')
         subsets(nsub+1) = subset
         nsub = nsub+1
         isubset = nsub
         open(50+isubset,file='preppost_'//subset,form='unformatted')
         call openbf(50+isubset,'OUT',lunin)
         if(nsub==1) call maxout(20000)
      endif
      lunot=50+isubset

      pobx=.false.
      zobx=.false.

! write out the observation with postevents added

    call openmb(lunot,subset,idate); call ufbcpy(lunin,lunot)
    if(subset /= 'GOESND'.and.subset /= 'GPSIPW') then
       call ufbint(lunot,obs_out,10,nlev,iret,obstr) 
       call ufbint(lunot,qms_out,10,nlev,iret,qmstr)
       call ufbint(lunot,pcs_out,10,nlev,iret,pcstr)
       call ufbint(lunot,rcs_out,10,nlev,iret,rcstr)
       call ufbint(lunot,fcs_out,10,nlev,iret,fcstr)
       call ufbint(lunot,ans_out,10,nlev,iret,anstr)
       call ufbint(lunot,err_out,10,nlev,iret,errstr)
       call ufbint(lunot,vqc_out,10,nlev,iret,vqcstr)
       do l=1,nlev ! this section updates the modified pob or zob values
       if(pob_out(1,l)<bmiss)then
          pob_out(2,l)=qms(1,l)
          pob_out(3,l)=gsicode 
          pob_out(4,l)=000
          pobx=.true.
       endif
       if(zob_out(1,l)<bmiss)then
          zob_out(2,l)=qms(4,l)
          zob_out(3,l)=gsicode 
          zob_out(4,l)=000
          zobx=.true.
       endif
       enddo
       if(pobx)call ufbint(lunot,pob_out,10,nlev,iret,'POB PQM PPC PRC')
       if(zobx)call ufbint(lunot,zob_out,10,nlev,iret,'ZOB ZQM ZPC ZRC')
    endif
    call writsb(lunot)
    nrep(ntyp)=nrep(ntyp)+1

    enddo loopp ! end of prepbufr match and write loop

! be sure to close all bufr output files to get all the data out

    do i=1,nsub
    call closbf(50+i) 
    enddo

! print an accounting of the posting

    nnt=knt-nnt
    print*
    print*,'data counts by report type'
    do i=1,300
    if(nrep(i).gt.0) print'(i3,8(2x,i8))',i,jnt(i,0),knt(i,0),(nnt(i,j),j=0,5)
    enddo

    print*
    print*,'nmatch=',nmatch
    print*

    stop

! print any diagnose records which were not matched to prepbufr data

    iprt=0
    do i=1,nwrk
    ityp = nint(dwork(i,1))
    if(ityp.gt.0) then
       print'(a8,9(1x,f8.2),a)',cwork(i),(dwork(i,j),j=0,8),'a'
       iprt=iprt+1; if(iprt.gt.100)stop
    endif
    enddo

! diagnostic for modifed p or z

    iptot=0;iztot=0

    do i=1,300
    if(nrex(i,1)+nrex(i,2)>0)print'(3i10)',i,nrex(i,1),nrex(i,2)
    iptot=iptot+nrex(i,1); iztot=iztot+nrex(i,2)
    enddo
    print'(a10,2i10)','TOT',iptot,iztot

    stop
    end
