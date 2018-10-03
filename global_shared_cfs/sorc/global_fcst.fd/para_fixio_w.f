       subroutine para_fixio_w(ioproc,sfc_fld, nw,cfile,xhour,idate,
     &                         global_lats_r,lonsperlar)
!!
      use resol_def
      use layout1
      use sfcio_module
      use sfc_flx_esmfmod
      implicit none
!!
      type(sfc_var_data)        :: sfc_fld
!
      integer nw,ioproc
      character*(*) cfile
      real(kind=kind_io8) xhour
      integer              global_lats_r(latr)
      integer              lonsperlar(latr)
!!
!!
!mi   real(kind=kind_io4) buff4(lonr,latr,4)
      real(kind=kind_ior) buff4(lonr,latr)
      real(kind=kind_io8) bfo(lonr,lats_node_r)
      real(kind=kind_io8) buffi(lonr,lats_node_r)
      integer kmsk(lonr,lats_node_r)
      integer idate(4),k
!!
!     character*8 labfix(4)
!     real(kind=kind_io4) yhour
!     integer,save:: version
!clux data version/200004/
!mi   data version/200412/
!
      type(sfcio_head) head
      type(sfcio_dbta) data
      integer iret
      logical first
      save head, first
      data first /.true./
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      if (me.eq.ioproc) then
        if (first) then
          head%clabsfc = char(0)//char(0)//char(0)//char(0)//
     &                   char(0)//char(0)//char(0)//char(0)
          head%latb    = latr
          head%lonb    = lonr
          head%ivs     = ivssfc_restart
          head%irealf  = 2
          head%lsoil   = lsoil
          call sfcio_alhead(head,iret)
          head%lpl     = lonsperlar(1:latr/2)
          if (lsoil .eq. 4) then
            head%zsoil   = (/-0.1,-0.4,-1.0,-2.0/)
          elseif (lsoil .eq. 2) then
            head%zsoil   = (/-0.1,-2.0/)
          endif
          first = .false.
        endif
        head%fhour   = xhour
        head%idate   = idate
!
        print 99,nw,xhour,idate
99      format(1h ,'in fixio nw=',i7,2x,'hour=',f8.2,3x,'idate=',
     &  4(1x,i4))
        call sfcio_aldbta(head,data,iret)
      endif
!!
      kmsk= nint(sfc_fld%slmsk)
!
      call uninterpred(1,kmsk,bfo,sfc_fld%tsea,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%tsea=buff4
!
      do k=1,lsoil
      buffi(:,:) = sfc_fld%smc(:,k,:)
      call uninterpred(1,kmsk,bfo,buffi,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%smc(:,:,k)=buff4
      enddo
!
      call uninterpred(1,kmsk,bfo,sfc_fld%sheleg,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%sheleg=buff4
!
      do k=1,lsoil
      buffi(:,:) = sfc_fld%stc(:,k,:)
      call uninterpred(1,kmsk,bfo,buffi,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%stc(:,:,k)=buff4
      enddo
!
      call uninterpred(1,kmsk,bfo,sfc_fld%tg3,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%tg3=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%zorl,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%zorl=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%alvsf,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%alvsf=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%alvwf,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%alvwf=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%alnsf,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%alnsf=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%alnwf,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%alnwf=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%slmsk,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%slmsk=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%vfrac,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%vfrac=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%canopy,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%canopy=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%f10m,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%f10m=buff4

      call uninterpred(1,kmsk,bfo,sfc_fld%t2m,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%t2m=buff4

      call uninterpred(1,kmsk,bfo,sfc_fld%q2m,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%q2m=buff4

!
      call uninterpred(1,kmsk,bfo,sfc_fld%vtype,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%vtype=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%stype,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%stype=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%facsf,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%facsf=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%facwf,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%facwf=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%uustar,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%uustar=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%ffmm,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%ffmm=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%ffhh,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%ffhh=buff4
!
!c-- xw: for sea-ice nov04
      call uninterpred(1,kmsk,bfo,sfc_fld%hice,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%hice=buff4
!
      call uninterpred(1,kmsk,bfo,sfc_fld%fice,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%fice=buff4

      call uninterpred(1,kmsk,bfo,sfc_fld%tisfc,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%tisfc=buff4
!c-- xw: end sea-ice nov04
!
!lu: the addition of 8 noah-related records starts here ........................
!tprcp
      call uninterpred(1,kmsk,bfo,sfc_fld%tprcp,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%tprcp=buff4
!srflag
      call uninterpred(1,kmsk,bfo,sfc_fld%srflag,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%srflag=buff4
!snwdph
      call uninterpred(1,kmsk,bfo,sfc_fld%snwdph,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%snwdph=buff4
!slc
      do k=1,lsoil
      buffi(:,:) = sfc_fld%slc(:,k,:)
      call uninterpred(1,kmsk,bfo,buffi,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%slc(:,:,k)=buff4
      enddo
!shdmin
      call uninterpred(1,kmsk,bfo,sfc_fld%shdmin,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%shdmin=buff4
!shdmax
      call uninterpred(1,kmsk,bfo,sfc_fld%shdmax,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%shdmax=buff4
!slope
      call uninterpred(1,kmsk,bfo,sfc_fld%slope,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%slope=buff4
!snoalb
      call uninterpred(1,kmsk,bfo,sfc_fld%snoalb,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%snoalb=buff4
!lu: the addition of 8 noah records ends here .........................

      call uninterpred(1,kmsk,bfo,sfc_fld%oro,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%orog=buff4

      if(me.eq.ioproc) then
        call sfcio_swohdc(nw,cfile,head,data,iret)
        call sfcio_axdbta(data,iret)
      endif
!
      return
      end
