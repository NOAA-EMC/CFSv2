      subroutine read_mtn_hprim_oz(slmsk,hprime,needoro,oro,oro_uf,
     &           iozondp,ozplin,global_lats_r,lonsperlar)
!
!***********************************************************************
!
      use resol_def
      use namelist_def, only : use_ufo
      use layout1
      use mpi_def
      use ozne_def
      implicit none

!
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
      real (kind=kind_io8) slmsk(lonr,lats_node_r),
     &  hprime(lonr,nmtvr,lats_node_r),oro(lonr,lats_node_r)
     &, oro_uf(lonr,lats_node_r)
 
      integer iozondp
      real (kind=kind_io8) ozplin(latsozp,levozp,pl_coeff,timeoz)
 
      real(kind=kind_io4) buff1(lonr,latr),buffm(lonr,latr,nmtvr)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      real(kind=kind_io8) buff2(lonr,lats_node_r)
      integer kmsk0(lonr,latr)
      integer i,j,k,nmtn
      integer needoro
      logical iotask
!
      kmsk0=0
!
!     read hprime from file mtnvar
!     ****************************
      nmtn = 24
      if (io_task) then
        read(nmtn) buffm
!!      do k=1,nmtvr
!!        write(200) buffm(:,:,k)
!!      enddo
      endif

      do k=1,nmtvr
       call split2d(buffm(1,1,k),buffo,global_lats_r)
       call interpred(1,kmsk0,buffo,buff2,global_lats_r,
     &                lonsperlar)
!$omp parallel do private(i,j)
       do j=1,lats_node_r
         do i=1,lonr
           hprime(i,k,j) = buff2(i,j)
         enddo
       enddo
      enddo
 
      if (iozondp.eq.1) call readoz_disprd(ozplin)
!
!     reading the grib orography and scattering the data
!
      if (needoro == 1) then

        if (io_task) then
          call orord(101,lonr,latr,buff1,'orography')
          print *,'read grb orography'
        endif
        call split2d(buff1,buffo,global_lats_r)
        call interpred(1,kmsk0,buffo,oro,global_lats_r,lonsperlar)
      endif
!                                                   read unfiltered orography
      if (use_ufo) then
        if (io_task) then
          call orord(101,lonr,latr,buff1,'orography_uf')
          print *,'read grb orography_uf'
        endif
        call split2d(buff1,buffo,global_lats_r)
        call interpred(1,kmsk0,buffo,oro_uf,global_lats_r,lonsperlar)
      else
!$omp parallel do private(i,j)
         do j=1,lats_node_r
           do i=1,lonr
            oro_uf(i,j) = 0.0
          enddo
        enddo
      endif
 
      return
      end
      subroutine read_sfc(sfc_fld,needoro,nread,
     &                    cfile,global_lats_r,lonsperlar)
!
!***********************************************************************
!
      use sfcio_module
      use resol_def
      use layout1
      use mpi_def
      use sfc_flx_esmfmod
      use namelist_soilveg , only : salp_data, snupx
      use physcons, only : tgice => con_tice
      implicit none
!
      type(sfc_var_data)        :: sfc_fld
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)

      integer needoro

      real(kind=kind_io4) buff1(lonr,latr)
!    &,    buff4(lonr,latr,4),xhour
!$$$     &     buff4(lonr,latr,4),slmskful(lonr,latr),xhour
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      real(kind=kind_io8) buff3(lonr,lats_node_r)
      integer nread,i,j,k
      character*(*) cfile
      integer kmsk(lonr,latr)
!$$$      common /comfixio/slmskful
      real t1,t2,timef,rsnow
      type(sfcio_head) head
      type(sfcio_data) data
      integer iret, vegtyp
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      t1=timef()

      if (me == 0) print *,' nread=',nread,' cfile=',cfile
      if (io_task) then
        call sfcio_srohdc(nread,cfile,head,data,iret)

        print 99,nread,head%fhour,head%idate,
     &         head%lonb,head%latb,head%lsoil,head%ivs,iret,lats_node_r
99      format(1h ,'in fixio nread=',i3,2x,'hour=',f8.2,3x,'idate=',
     &  4(1x,i4),4x,'lonsfc,latsfc,lsoil,ivssfc,iret=',6i8)

        if(iret.ne.0) goto 5000
!       if(head%ivs.ne.200412.and.head%ivs.ne.200501) goto 5000
        if(head%lonb.ne.lonr) goto 5000
        if(head%latb.ne.latr) goto 5000
        if(head%lsoil.ne.lsoil) goto 5000

      endif

      kmsk=0

      if (io_task) buff1=data%tsea
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%tsea,global_lats_r,lonsperlar)

      do k=1, lsoil
        if (io_task) buff1=data%smc(:,:,k)
        call split2d(buff1, buffo,global_lats_r)
        call interpred(1,kmsk,buffo,buff3,global_lats_r,lonsperlar)
        sfc_fld%smc(:,k,:)=buff3(:,:)
      enddo

      if (io_task) buff1=data%sheleg
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%sheleg,
     &               global_lats_r,lonsperlar)

      do k = 1, lsoil
        if (io_task) buff1=data%stc(:,:,k)
        call split2d(buff1, buffo,global_lats_r)
        call interpred(1,kmsk,buffo,buff3,global_lats_r,lonsperlar)
        sfc_fld%stc(:,k,:)=buff3(:,:)
      enddo

      if (io_task) buff1=data%tg3
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%tg3,global_lats_r,lonsperlar)

      if (io_task) buff1=data%zorl
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%zorl,global_lats_r,lonsperlar)

      sfc_fld%cv  = 0
      sfc_fld%cvb = 0
      sfc_fld%cvt = 0

      if (io_task) buff1=data%alvsf
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%alvsf,
     &               global_lats_r,lonsperlar)
      if (io_task) buff1=data%alvwf
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%alvwf,
     &               global_lats_r,lonsperlar)
      if (io_task) buff1=data%alnsf
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%alnsf,
     &               global_lats_r,lonsperlar)
      if (io_task) buff1=data%alnwf
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%alnwf,
     &               global_lats_r,lonsperlar)

!     the mask cannot be interpolated
      if (io_task) buff1=data%slmsk
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%slmsk,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%vfrac
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%vfrac,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%canopy
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%canopy,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%f10m
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%f10m,global_lats_r,lonsperlar)

      if (io_task) buff1=data%vtype
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%vtype,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%stype
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%stype,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%facsf
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%facsf,
     &               global_lats_r,lonsperlar)
      if (io_task) buff1=data%facwf
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%facwf,
     &               global_lats_r,lonsperlar)

!szunyogh 06/16/99
        if (io_task) buff1=data%uustar
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,sfc_fld%uustar,
     &               global_lats_r,lonsperlar)

        if (io_task) buff1=data%ffmm
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,sfc_fld%ffmm,
     &                  global_lats_r,lonsperlar)

        if (io_task) buff1=data%ffhh
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,sfc_fld%ffhh,
     &                  global_lats_r,lonsperlar)

!c-- xw: for sea-ice nov04
!    sea-ice (hice/fice) was added to the surface files.

         if (io_task) buff1=data%hice
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,sfc_fld%hice,
     &                  global_lats_r,lonsperlar)

         if (io_task) buff1=data%fice
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,sfc_fld%fice,
     &                  global_lats_r,lonsperlar)

         if (io_task) buff1=data%tisfc
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,sfc_fld%tisfc,
     &                  global_lats_r,lonsperlar)
         if (lats_node_r > 0 )  then
           if (sfc_fld%tisfc(1,1) < 0.0)  then
!$omp parallel do private(j,i)
             do j=1,lats_node_r
               do i=1,lonr
                 sfc_fld%tisfc(i,j) = sfc_fld%tsea(i,j)
                 if(sfc_fld%slmsk(i,j) >=  2. .and.
     &             sfc_fld%fice(i,j)  >= 0.5) then
                   sfc_fld%tisfc(i,j) = (sfc_fld%tsea(i,j)
     &            -tgice*(1.-sfc_fld%fice(i,j))) / sfc_fld%fice(i,j)
                   sfc_fld%tisfc(i,j)=min(sfc_fld%tisfc(i,j),tgice)
                 endif
               enddo
             enddo
           endif
         endif

!c-- xw: end sea-ice

!lu   11/10/2004
!*     surface files for gfs/noah contain 8 additional records:
!*     tprcp, srflag, snwdph, slc, shdmin, shdmax, slope, snoalb

         if (io_task) buff1=data%tprcp
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,sfc_fld%tprcp,
     &                  global_lats_r,lonsperlar)

!* srflag
         if (io_task) buff1=data%srflag
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,sfc_fld%srflag,
     &                  global_lats_r,lonsperlar)

!* snwdph
         if (io_task) buff1=data%snwdph
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,sfc_fld%snwdph,
     &                  global_lats_r,lonsperlar)

!* slc
         do k=1, lsoil
         if (io_task) buff1=data%slc(:,:,k)
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,buff3,global_lats_r,lonsperlar)
         sfc_fld%slc(:,k,:) = buff3(:,:)
         enddo

!* shdmin
         if (io_task) buff1=data%shdmin
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,sfc_fld%shdmin,
     &                  global_lats_r,lonsperlar)

!* shdmax
         if (io_task) buff1=data%shdmax
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,sfc_fld%shdmax,
     &                  global_lats_r,lonsperlar)

!* slope
         if (io_task) buff1=data%slope
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,sfc_fld%slope,
     &                  global_lats_r,lonsperlar)

!* snoalb
         if (io_task) buff1=data%snoalb
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,sfc_fld%snoalb,
     &                  global_lats_r,lonsperlar)
!     print *,' snoalb=',sfc_fld%snoalb(1,:)
!lu [+67l]: the addition of 8 noah records ends here .........................

       if(needoro == 1) then
         if (io_task) then
           buff1=data%orog
           needoro=1
           if(all(data%orog.ne.sfcio_realfill)) needoro=0
           print *,'read sfc orography'
         endif
         call split2d(buff1, buffo,global_lats_r)
         call interpred(1,kmsk,buffo,sfc_fld%oro,
     &                  global_lats_r,lonsperlar)
         call skip(needoro)
       endif
!
!wei initialize snow fraction(sheleg is in mm)
!$omp parallel do private(j,i,vegtyp,rsnow)
      do j=1,lats_node_r
        do i=1,lonr
          sfc_fld%sncovr(i,j) = 0.0
          if (sfc_fld%slmsk(i,j) > 0.001) then
            vegtyp = sfc_fld%vtype(i,j)
            if( vegtyp == 0 ) vegtyp = 7
            rsnow  = 0.001*sfc_fld%sheleg(i,j)/snupx(vegtyp)
            if (0.001*sfc_fld%sheleg(i,j) < snupx(vegtyp)) then
              sfc_fld%sncovr(i,j) = 1.0 - ( exp(-salp_data*rsnow)
     &                                    - rsnow*exp(-salp_data))
            else
              sfc_fld%sncovr(i,j) = 1.0
            endif
!           if (i == 1)
!    &       print*,snupx(vegtyp),salp_data,sfc_fld%sncovr(i,j),
!    &       '************debug',sfc_fld%sheleg(i,j),vegtyp,' j=',j
!    &,      ' snoalb1=',sfc_fld%snoalb(i,j)
!
          endif
        enddo
       enddo
!

       if (io_task) then
         call sfcio_axdata(data,iret)
         t2=timef()
         print *,'fixio time ',t2-t1,t1,t2
       endif
!
      return
 5000 print *, ' error in input in fixio'
      call mpi_quit(3333)
      end
!
      subroutine read_nst(nst_fld, nread, cfile,
     &                   global_lats_r, lonsperlar)
!
!***********************************************************************
!
      use namelist_def
      use nstio_module
      use resol_def
      use layout1
      use mpi_def
      use nst_var_esmfmod
      implicit none
!
      type(nst_var_data)       :: nst_fld
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)

!     real (kind=kind_io8) slmsk(lonr,lats_node_r),

      real(kind=kind_io4) buff1(lonr,latr)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      integer nread,i,j,k
      character*(*) cfile
      integer kmsk(lonr,latr)
      real t1,t2,timef
      type(nstio_head) head
      type(nstio_data) data
      integer iret

c
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      t1=timef()

      if (io_task) then

        call nstio_srohdc(nread,cfile,head,data,iret)

        print 99,nread,head%fhour,head%idate,
     &     head%lonb,head%latb,head%lsea,head%ivo,iret,lats_node_r
99      format(1h ,'in fixio nread=',i3,2x,'hour=',f8.2,3x,'idate=',
     &  4(1x,i4),4x,'lonnst,latnst,lsea,ivsnst,iret=',6i8)

        if(iret.ne.0) goto 5000
        if(head%lonb.ne.lonr) goto 5000
        if(head%latb.ne.latr) goto 5000
        if(head%lsea.ne.lsea) goto 5000

      endif

      kmsk=0
!
!     assign ocnf(lonr,lats_node_r,nf_ocn)
!
      if (io_task) buff1=data%xt
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%xt,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%xs
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%xs,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%xu
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%xu,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%xv
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%xv,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%xz
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%xz,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%zm
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%zm,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%xtts
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%xtts,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%xzts
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%xzts,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%dt_cool
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%dt_cool,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%z_c
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%z_c,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%c_0
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%c_0,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%c_d
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%c_d,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%w_0
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%w_0,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%w_d
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%w_d,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%d_conv
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%d_conv,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%ifd
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%ifd,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%tref
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%tref,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%qrain
      call split2d(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%qrain,
     &               global_lats_r,lonsperlar)

       if (io_task) then
         call nstio_axdata(data,iret)
         t2=timef()
         print *,'fixio for nst time ',t2-t1,t1,t2
       endif
!
      return
 5000 print *, ' error in input in read_nst'
      call mpi_quit(4444)
      end
!
      subroutine set_nst(tsea, nst_fld)
c
c***********************************************************************
c
      use namelist_def
      use resol_def
      use layout1
      use nst_var_esmfmod
      use module_nst_parameters, only: z_w_max
      use mpi_def
      implicit none
c
      type(nst_var_data)       :: nst_fld
      real (kind=kind_io8) tsea(lonr,lats_node_r)

      integer i,j,k
      real t1,t2,timef

c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      t1=timef()

      nst_fld%xt      = 0.0
      nst_fld%xs      = 0.0
      nst_fld%xu      = 0.0
      nst_fld%xv      = 0.0
      nst_fld%xz      = z_w_max
      nst_fld%zm      = 0.0
      nst_fld%xtts    = 0.0
      nst_fld%xzts    = 0.0
      nst_fld%dt_cool = 0.0
      nst_fld%z_c     = 0.0
      nst_fld%c_0     = 0.0
      nst_fld%c_d     = 0.0
      nst_fld%w_0     = 0.0
      nst_fld%w_d     = 0.0
      nst_fld%d_conv  = 0.0
      nst_fld%ifd     = 0.0
      nst_fld%tref    = tsea
      nst_fld%qrain   = 0.0
!
      t2=timef()
      print *,'fixio for set_nst time ',t2-t1,t1,t2
!
      return
      end
!
!***********************************************************************
!
      subroutine nst_reset_nonwater(tsea,nst_fld)
c
c***********************************************************************
c
      use resol_def
      use layout1
      use nst_var_esmfmod
      use module_nst_parameters, only: z_w_max
      use mpi_def
      implicit none
c
      type(nst_var_data)       :: nst_fld
      real (kind=kind_io8) tsea(lonr,lats_node_r)

      integer i,j
      real t1,t2,timef
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      t1=timef()

      do j = 1, lats_node_r
        do i = 1, lonr
          if ( nst_fld%slmsk(i,j) /= 0.0 ) then
            nst_fld%xt(i,j)      = 0.0
            nst_fld%xs(i,j)      = 0.0
            nst_fld%xu(i,j)      = 0.0
            nst_fld%xv(i,j)      = 0.0
            nst_fld%xz(i,j)      = z_w_max
            nst_fld%zm(i,j)      = 0.0
            nst_fld%xtts(i,j)    = 0.0
            nst_fld%xzts(i,j)    = 0.0
            nst_fld%dt_cool(i,j) = 0.0
            nst_fld%z_c(i,j)     = 0.0
            nst_fld%c_0(i,j)     = 0.0
            nst_fld%c_d(i,j)     = 0.0
            nst_fld%w_0(i,j)     = 0.0
            nst_fld%w_d(i,j)     = 0.0
            nst_fld%d_conv(i,j)  = 0.0
            nst_fld%ifd(i,j)     = 0.0
            nst_fld%tref(i,j)    = tsea(i,j)
            nst_fld%qrain(i,j)   = 0.0
          endif
        enddo
      enddo

            t2=timef()
            print *,'fixio for nst_reset_nonwater time ',t2-t1,t1,t2
!
      return
      end
!
!***********************************************************************
!
      subroutine interpred(iord,kmsk,f,fi,global_lats_r,lonsperlar)
!!
      use resol_def
      use layout1
      implicit none
!!
      integer              global_lats_r(latr)
      integer,intent(in):: iord
      integer,intent(in):: kmsk(lonr,lats_node_r)
      integer,intent(in):: lonsperlar(latr)
      real(kind=kind_io8),intent(in):: f(lonr,lats_node_r)
      real(kind=kind_io8),intent(out):: fi(lonr,lats_node_r)
      integer j,lons,lat
!!
      do j=1,lats_node_r
          lat=global_lats_r(ipt_lats_node_r-1+j)
          lons=lonsperlar(lat)
          if(lons.ne.lonr) then
            call intlon(iord,1,1,lonr,lons,
     &                  kmsk(1,j),f(1,j),fi(1,j))
cjfe        fi(lons+1:lonr,j)=-9999.e9
            fi(lons+1:lonr,j)=0.
          else
            fi(:,j)=f(:,j)
          endif
        enddo
      end subroutine
c
!***********************************************************************
!
      subroutine interpred_a(iord,kmsk,f,fi,global_lats_a,lonsperlat)
!!
      use resol_def
      use layout1
      implicit none
!!
      integer              global_lats_a(latg)
      integer,intent(in):: iord
      integer,intent(in):: kmsk(lonf,lats_node_a)
      integer,intent(in):: lonsperlat(latg)
      real(kind=kind_io8),intent(in):: f(lonf,lats_node_a)
      real(kind=kind_io8),intent(out):: fi(lonf,lats_node_a)
      integer j,lons,lat
!!
      do j=1,lats_node_a
          lat=global_lats_a(ipt_lats_node_a-1+j)
          lons=lonsperlat(lat)
          if(lons.ne.lonf) then
            call intlon(iord,1,1,lonf,lons,
     &                  kmsk(1,j),f(1,j),fi(1,j))
            fi(lons+1:lonf,j)=0.
          else
            fi(:,j)=f(:,j)
          endif
        enddo
      end subroutine
c***********************************************************************
c
      subroutine intlon(iord,imon,imsk,m1,m2,k1,f1,f2)
      use machine
      implicit none
      integer,intent(in):: iord,imon,imsk,m1,m2
      integer,intent(in):: k1(m1)
      real (kind=kind_io8),intent(in):: f1(m1)
      real (kind=kind_io8),intent(out):: f2(m2)
      integer i2,in,il,ir
      real (kind=kind_io8) r,x1
      r=real(m1)/real(m2)
      do i2=1,m2
         x1=(i2-1)*r
         il=int(x1)+1
         ir=mod(il,m1)+1
          if(iord.eq.2.and.(imsk.eq.0.or.k1(il).eq.k1(ir))) then
            f2(i2)=f1(il)*(il-x1)+f1(ir)*(x1-il+1)
          else
            in=mod(nint(x1),m1)+1
            f2(i2)=f1(in)
          endif
      enddo
      end subroutine
c
c**********************************************************************
c
      subroutine readoz_disprd(ozplin)
 
      use resol_def
      use layout1
      use ozne_def
      implicit none
!!
      integer n,k,i
      real (kind=kind_phys) ozplin(latsozp,levozp,pl_coeff,timeoz)
      real(kind=kind_io4) tempin(latsozp)
!
      do i=1,timeoz
        do n=1,pl_coeff
          do k=1,levozp
            read(kozpl) tempin
            ozplin(:,k,n,i) = tempin(:)
          enddo
        enddo
      enddo
 
      return
      end
c
c***********************************************************************
c
      subroutine orord(lugb,ioro,joro,oro,fnorog)
!
      use resol_def
      use layout1
      implicit none
!!
      integer lugb, ioro, joro, kpdoro, ior, jor, i,k
      character*(*) fnorog
!
      real (kind=kind_io4) oro(ioro,joro)
      real (kind=kind_io8) orog(ioro,joro), blnm, bltm
      logical gausm
!
!     fnorog = 'orography'
      kpdoro = 8
      ior    = ioro
      jor    = joro
      call fixrdg(lugb,ior,jor,fnorog,
     &            kpdoro,orog,gausm,blnm,bltm,me)
!
      if (ior .ne. ioro .or. jor .ne. joro) then
         print *,' orography file not o.k. run aborted'
         call abort
      endif
      oro = orog
!
      return
      end
c
c***********************************************************************
c
      subroutine split2d(x,xl,global_lats_r)
c
c***********************************************************************
c
      use resol_def
      use layout1
      use mpi_def
      implicit none
!!
      real(kind=kind_io4) x(lonr,latr)
      real (kind=kind_io8) xl(lonr,lats_node_r)
      real(kind=kind_io4) tmp(lonr,latr)
      integer global_lats_r(latr)
      integer nodesr
!     integer maxfld,nodesr
!     integer proc,j,lat,msgtag,i,buff,ierr
      integer proc,j,lat,i
      integer ifld/0/
      save ifld
      real t1,t2,t3,t4,timef,ta,tb
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!!
      xl=0.
!     maxfld=50
      ifld=ifld+1
!!
      if (io_task) then
        ta=timef()
        t3=ta
c        do proc=1,nodes-1
         do proc=1,1
c
c         sending the data
c         ----------------
         tmp=0.
         do j=1,latr
            do i=1,lonr
              tmp(i,j)=x(i,j)
            enddo
         enddo
!moor    msgtag=1000+proc*nodes*maxfld+ifld
         t1=timef()
!sela    print *,' gwvx broadcasting from ',nodes-1
         call mpi_bcast
     1 (tmp,lonr*latr,mpi_r_io,nodes-1,mpi_comm_all,info)
         call mpi_comm_rank(mpi_comm_all,i,info)
c         call mpi_send(tmp,lonr*latr,mpi_r_io,proc-1,msgtag,
c     &                  mpi_comm_all,info)
         t2=timef()
!sela    print 102,t2-t1
 
 102    format(' send time ',f10.5)
        enddo
        t4=timef()
      else
        if (.not.liope) then
          nodesr=nodes
        else
          nodesr=nodes+1
        endif
!moor   msgtag=1000+(me+1)*nodesr*maxfld+ifld
!sela    print *,' gwvx broadcastrec  from ',nodesr-1
         call mpi_bcast
     1 (tmp,lonr*latr,mpi_r_io,nodesr-1,mpi_comm_all,info)
         call mpi_comm_rank(mpi_comm_all,i,info)
!sela    print *,'gwvx ipt ',ipt
c        call mpi_recv(tmp,lonr*latr,mpi_r_io,nodesr-1,
c     &                msgtag,mpi_comm_all,stat,info)
        do j=1,lats_node_r
           lat=global_lats_r(ipt_lats_node_r-1+j)
           do i=1,lonr
              xl(i,j)=tmp(i,lat)
           enddo
        enddo
!!
      endif
!!
!!     for pes nodes-1
      if (.not.liope) then
        if (me.eq.nodes-1) then
          do j=1,lats_node_r
             lat=global_lats_r(ipt_lats_node_r-1+j)
             do i=1,lonr
                xl(i,j)=x(i,lat)
             enddo
          enddo
        endif
      endif
!!
      tb=timef()
         call mpi_comm_rank(mpi_comm_all,i,info)
 
!sela  if (io_task)print 103,tb-ta,t4-t3
 103  format(' global and send times  split2d',2f10.5)
      return
      end
c
c***********************************************************************
c
      subroutine skip(jump)
 
c*************************************************************************
 
      use resol_def
      use layout1
      use mpi_def
      implicit none
 
      integer jump,ipe
 
      if (icolor.eq.2) then
         ipe=nodes-1
      else
         ipe=nodes
      endif
 
      call mpi_bcast(jump,1,mpi_integer,ipe,mpi_comm_all,info)
 
      return
      end
!
c
c***********************************************************************
c
      subroutine excha(lats_nodes_r,global_lats_r,x1,x2,y1,y2
     &,                trcx,trcy)
c
c***********************************************************************
c
      use resol_def
      use layout1
      use mpi_def
      implicit none
 
      integer n,i,j,ierr,ilat,lat,node,nsend
      integer              global_lats_r(latr)
      integer              lats_nodes_r(nodes)
      real(kind=kind_io8) x1(lats_node_r),x2(lats_node_r)
     &,                   trcx(lats_node_r,ntrac,2)
      real(kind=kind_io8) y1(latr),y2(latr)
     &,                   trcy(latr,ntrac,2)
cjfe  real(kind=kind_mpi) tmps(2,lats_node_r_max,nodes)
cjfe  real(kind=kind_mpi) tmpr(2,lats_node_r_max,nodes)
      real(kind=kind_io8) tmps(2+ntrac*2,lats_node_r_max,nodes)
      real(kind=kind_io8) tmpr(2+ntrac*2,lats_node_r_max,nodes)
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      if (nodes.ne.1) then
        do node=1,nodes
          do i=1,lats_node_r
            lat=global_lats_r(ipt_lats_node_r-1+i)
            tmps(1,i,node) = x1(i)
            tmps(2,i,node) = x2(i)
            do n=1,ntrac
              tmps(2+n,      i,node) = trcx(i,n,1)
              tmps(2+n+ntrac,i,node) = trcx(i,n,2)
            enddo
          enddo
        enddo
!!
        nsend = (2+ntrac+ntrac)*lats_node_r_max

!jfe    call mpi_alltoall(tmps,nsend,mpi_r_mpi,
!jfe &                    tmpr,nsend,mpi_r_mpi,
!jfe &                    mc_comp,ierr)
        call mpi_alltoall(tmps,nsend,mpi_r_def,
     &                    tmpr,nsend,mpi_r_def,
     &                    mc_comp,ierr)
!!
        ilat=1
        do node=1,nodes
          do i=1,lats_nodes_r(node)
             lat = global_lats_r(ilat)
             y1(lat) = tmpr(1,i,node)
             y2(lat) = tmpr(2,i,node)
             do n=1,ntrac
               trcy(lat,n,1) = tmpr(2+n,      i,node)
               trcy(lat,n,2) = tmpr(2+n+ntrac,i,node)
             enddo
             ilat = ilat + 1
          enddo
        enddo
!!
      else
        y1   = x1
        y2   = x2
        trcy = trcx
      endif
!!
      return
      end
c
c***********************************************************************
c
      subroutine sumlat(n,x,nodes)
c
c***********************************************************************
c
      use mpi_def
      implicit none
 
      integer n,i,j,np,nodes
      real(kind=kind_io8) x(n),y(n)
      real(kind=kind_io4) z(n)
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      if (nodes.ne.1) then
        do i=1,n
          y(i)=x(i)
        enddo
        call mpi_allreduce(y,x,n,mpi_r_def,mpi_sum,
     &                    mc_comp   ,info)
      endif
        do i=1,n
          z(i)=x(i)
        enddo
        do i=1,n
          x(i)=z(i)
        enddo
!!
      return
      end
c
c***********************************************************************
c
      subroutine unsplit2d(ioproc,x,xl,global_lats_r)
c
c***********************************************************************
c
      use resol_def
      use layout1
      use mpi_def
      implicit none
!!
      real(kind=kind_io4) x(lonr,latr)
      real (kind=kind_io8) xl(lonr,lats_node_r)
      real(kind=kind_io4) tmp(lonr,latr+2)
      integer global_lats_r(latr),ipt_lats_node_rl,nodesr
      integer lats_nodes_rl
      integer maxfld,ioproc,nproct
      integer proc,j,lat,msgtag,i
      integer ifldu/0/
      save ifldu
      integer illen,ncc
      data ncc/0/
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!!
      x=0.
      maxfld=50
      ifldu=ifldu+1
!!
      if (me.ne.ioproc) then
c
c         sending the data
c         ----------------
         tmp=0.
         tmp(lonr,latr+1)=ipt_lats_node_r
         tmp(lonr,latr+2)=lats_node_r
         do j=1,lats_node_r
            do i=1,lonr
              tmp(i,j)=xl(i,j)
            enddo
         enddo
         if (.not.liope) then
           nodesr=nodes
         else
           nodesr=nodes+1
         endif
         msgtag=1000+(me+1)*nodesr*maxfld+ifldu
          call mpi_send(tmp(lonr,latr+1),1,mpi_r_io,ioproc,
     &                  msgtag,mpi_comm_all,info)
          call mpi_send(tmp(lonr,latr+2),1,mpi_r_io,ioproc,
     &                  msgtag,mpi_comm_all,info)
         illen=tmp(lonr,latr+2)
c send the local grid domain
         call mpi_send(tmp(1,1),illen*lonr,mpi_r_io,ioproc,
     &                  msgtag,mpi_comm_all,info)
      else
!!
!!     for pes ioproc
        if (.not.liope) then
          nproct=nodes
          do j=1,lats_node_r
             lat=global_lats_r(ipt_lats_node_r-1+j)
             do i=1,lonr
                x(i,lat)=xl(i,j)
             enddo
          enddo
        else
          nproct=nodes-1
        endif
        do proc=1,nproct
         if (proc.ne.ioproc+1) then
         msgtag=1000+proc*nodes*maxfld+ifldu
          call mpi_recv(tmp(lonr,latr+1),1,mpi_r_io,proc-1,
     &                msgtag,mpi_comm_all,stat,info)
          call mpi_recv(tmp(lonr,latr+2),1,mpi_r_io,proc-1,
     &                msgtag,mpi_comm_all,stat,info)
         illen=tmp(lonr,latr+2)
          call mpi_recv(tmp(1,1),illen*lonr ,mpi_r_io,proc-1,
     &                msgtag,mpi_comm_all,stat,info)
         if (.not.liope) then
           ipt_lats_node_rl=tmp(lonr,latr+1)
           lats_nodes_rl=tmp(lonr,latr+2)
         else
           ipt_lats_node_rl=tmp(lonr,lats_node_r_max+1)
           lats_nodes_rl=tmp(lonr,lats_node_r_max+2)
         endif
         do j=1,lats_nodes_rl
           lat=global_lats_r(ipt_lats_node_rl-1+j)
           do i=1,lonr
              x(i,lat)=tmp(i,j)
           enddo
         enddo
         endif   !(proc.ne.ioproc+1)
        enddo
!!
      endif
         ncc=ncc+1
 
!!
      return
      end
c
c
c***********************************************************************
c
      subroutine uninterpred(iord,kmsk,f,fi,global_lats_r,lonsperlar)
!!
      use resol_def
      use layout1
      implicit none
!!
      integer              global_lats_r(latr)
      integer,intent(in):: iord
      integer,intent(in):: kmsk(lonr,lats_node_r)
      integer,intent(in):: lonsperlar(latr)
      real(kind=kind_io8),intent(out):: f(lonr,lats_node_r)
      real(kind=kind_io8),intent(in):: fi(lonr,lats_node_r)
      integer j,lons,lat
!!
      do j=1,lats_node_r
          lat=global_lats_r(ipt_lats_node_r-1+j)
          lons=lonsperlar(lat)
          if(lons.ne.lonr) then
            call intlon(iord,1,1,lons,lonr,
     &                  kmsk(1,j),fi(1,j),f(1,j))
          else
            f(:,j) = fi(:,j)
          endif
        enddo
      end subroutine

      subroutine uninterpred_a(iord,kmsk,f,fi,global_lats_a,lonsperlat)
!!
      use resol_def
      use layout1
      implicit none
!!
      integer              global_lats_a(latg)
      integer,intent(in):: iord
      integer,intent(in):: kmsk(lonf,lats_node_a)
      integer,intent(in):: lonsperlat(latg)
      real(kind=kind_io8),intent(out):: f(lonf,lats_node_a)
      real(kind=kind_io8),intent(in):: fi(lonf,lats_node_a)
      integer j,lons,lat
!!
      do j=1,lats_node_a
          lat=global_lats_a(ipt_lats_node_a-1+j)
          lons=lonsperlat(lat)
          if(lons.ne.lonf) then
            call intlon(iord,1,1,lons,lonf,
     &                  kmsk(1,j),fi(1,j),f(1,j))
          else
            f(:,j)=fi(:,j)
          endif
        enddo
      end subroutine

      subroutine uninterprez(iord,kmsk,f,fi,global_lats_r,lonsperlar)
!!
      use resol_def
      use mod_state
      use layout1
      implicit none
!!
      integer              global_lats_r(latr)
      integer,intent(in):: iord
      integer,intent(in):: kmsk(lonr,lats_node_r)
      integer,intent(in):: lonsperlar(latr)
      real(kind=kind_io8),intent(out):: f(lonr,lats_node_r)
      real(kind=kind_io8),intent(in) :: fi(lonr,lats_node_r)
!      real(kind=4) f4(lonr,lats_node_r)
      integer j,lons,lat
      integer i
!!
      do j=1,lats_node_r
          lat=global_lats_r(ipt_lats_node_r-1+j)
          lons=lonsperlar(lat)
          if(lons.ne.lonr) then
            call intlon(iord,1,1,lons,lonr,
     &                  kmsk(1,j),fi(1,j),f(1,j))
!           do i=1,lons
!             f4(i,j) = fi(i,j)
!           enddo
          else
            do i=1,lonr
              f(i,j)  = fi(i,j)
!             f4(i,j) = fi(i,j)
            enddo
          endif
        enddo
      do j=1,lats_node_r
        do i=1,lonr
          buff_mult_piecea(i,ngrid,j) = f (i,j)
        end do
      end do
      ngrid = ngrid + 1
      end subroutine
      subroutine unsplit2z(x,global_lats_r)
c
c***********************************************************************
c
      use resol_def
      use mod_state
      use layout1
      use mpi_def
      implicit none
!!
      real(kind=kind_io4) x(lonr,latr)
!     real(kind=kind_io4) tmp(lonr,latr+2)
      integer global_lats_r(latr),ipt_lats_node_rl
      integer lats_nodes_rl
      integer maxfld,nproct
!     integer maxfld,ioproc,nproct
      integer proc,j,lat,i
      integer ifldu/0/
      save ifldu
       character*8 cna
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!!
      write(cna,985)600+ngrid
 985   format('fort.',i3)
      x=0.
      maxfld=50
      ifldu=ifldu+1
!!
!     if (me.ne.ioproc) then
!           continue
!     else
!!
!!     for pes ioproc
!        if (.not.liope) then
!            continue
!        else
!          nproct=nodes-1
!        endif
           nproct=nodes_comp
        do proc=1,nproct
            ipt_lats_node_rl=ivar_global_a(1,proc)
            lats_nodes_rl=ivar_global_a(2,proc)
         do j=1,lats_nodes_rl
           lat=global_lats_r(ipt_lats_node_rl-1+j)
           do i=1,lonr
!              x(i,lat)=tmp(i,j)
              x(i,lat)=buff_mult_piecesa(i,ngrid,j,proc)
           enddo
         enddo
!         endif   !(proc.ne.ioproc+1)
        enddo
!!
!        call baclose(563,i)
!         print *,cna,' unsplitfclose  ',i
!        call baopenw(563,cna,i)
!         print *,cna,' unsplitf open  ',i
!     endif
        ngrid=ngrid+1
!!
      return
      end
 
c
c***********************************************************************
c
      subroutine unsplit2d_r(ioproc,x,xl,global_lats_r)
c
c***********************************************************************
c
      use resol_def
      use layout1
      use mpi_def
      implicit none
!!
      real(kind=kind_ior) x(lonr,latr)
      real (kind=kind_io8) xl(lonr,lats_node_r)
      real(kind=kind_ior) tmp(lonr,latr+2)
      integer global_lats_r(latr),ipt_lats_node_rl,nodesr
      integer lats_nodes_rl
      integer maxfld,ioproc,nproct
      integer proc,j,lat,msgtag,i
      integer ifldu/0/
      save ifldu
      integer illen,ncc
      data ncc/0/
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!!
!     x=0.               ! commented by moorthi on 20051117
      maxfld=50
      ifldu=ifldu+1
!!
      if (me.ne.ioproc) then
c
c         sending the data
c         ----------------
         tmp=0.
         tmp(lonr,latr+1)=ipt_lats_node_r
         tmp(lonr,latr+2)=lats_node_r
         do j=1,lats_node_r
            do i=1,lonr
              tmp(i,j)=xl(i,j)
            enddo
         enddo
         if (.not.liope) then
           nodesr=nodes
         else
           nodesr=nodes+1
         endif
         msgtag=1000+(me+1)*nodesr*maxfld+ifldu
          call mpi_send(tmp(lonr,latr+1),1,mpi_r_io_r,ioproc,
     &                  msgtag,mpi_comm_all,info)
          call mpi_send(tmp(lonr,latr+2),1,mpi_r_io_r,ioproc,
     &                  msgtag,mpi_comm_all,info)
         illen=tmp(lonr,latr+2)
c send the local grid domain
         call mpi_send(tmp(1,1),illen*lonr,mpi_r_io_r,ioproc,
     &                  msgtag,mpi_comm_all,info)
      else
!!
!!     for pes ioproc
        x = 0.0               ! added by moorthi on 2005111700
        if (.not.liope) then
          nproct=nodes
          do j=1,lats_node_r
             lat=global_lats_r(ipt_lats_node_r-1+j)
             do i=1,lonr
                x(i,lat)=xl(i,j)
             enddo
          enddo
        else
          nproct=nodes-1
        endif
        do proc=1,nproct
         if (proc.ne.ioproc+1) then
         msgtag=1000+proc*nodes*maxfld+ifldu
          call mpi_recv(tmp(lonr,latr+1),1,mpi_r_io_r,proc-1,
     &                msgtag,mpi_comm_all,stat,info)
          call mpi_recv(tmp(lonr,latr+2),1,mpi_r_io_r,proc-1,
     &                msgtag,mpi_comm_all,stat,info)
         illen=tmp(lonr,latr+2)
          call mpi_recv(tmp(1,1),illen*lonr ,mpi_r_io_r,proc-1,
     &                msgtag,mpi_comm_all,stat,info)
         if (.not.liope) then
           ipt_lats_node_rl=tmp(lonr,latr+1)
           lats_nodes_rl=tmp(lonr,latr+2)
         else
           ipt_lats_node_rl=tmp(lonr,lats_node_r_max+1)
           lats_nodes_rl=tmp(lonr,lats_node_r_max+2)
         endif
         do j=1,lats_nodes_rl
           lat=global_lats_r(ipt_lats_node_rl-1+j)
           do i=1,lonr
              x(i,lat)=tmp(i,j)
           enddo
         enddo
         endif   !(proc.ne.ioproc+1)
        enddo
!!
      endif
         ncc=ncc+1
 
!!
      return
      end
c
c***********************************************************************
c
      subroutine unsplit2d_a(ioproc,x,xl,global_lats_a)
c
c***********************************************************************
c
      use resol_def
      use layout1
      use mpi_def
      implicit none
!!
      real(kind=kind_ior) x(lonf,latg)
      real (kind=kind_io8) xl(lonf,lats_node_a)
      real(kind=kind_ior) tmp(lonf,latg+2)
      integer global_lats_a(latg),ipt_lats_node_rl,nodesr
      integer lats_nodes_rl
      integer maxfld,ioproc,nproct
      integer proc,j,lat,msgtag,i
      integer ifldu/0/
      save ifldu
      integer illen,ncc
      data ncc/0/
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!!
!     x=0.               ! commented by moorthi on 20051117
      maxfld=50
      ifldu=ifldu+1
!!
      if (me.ne.ioproc) then
c
c         sending the data
c         ----------------
         tmp=0.
         tmp(lonf,latg+1)=ipt_lats_node_a
         tmp(lonf,latg+2)=lats_node_a
         do j=1,lats_node_a
            do i=1,lonf
              tmp(i,j)=xl(i,j)
            enddo
         enddo
         if (.not.liope) then
           nodesr=nodes
         else
           nodesr=nodes+1
         endif
         msgtag=1000+(me+1)*nodesr*maxfld+ifldu
          call mpi_send(tmp(lonf,latg+1),1,mpi_r_io_r,ioproc,
     &                  msgtag,mpi_comm_all,info)
          call mpi_send(tmp(lonf,latg+2),1,mpi_r_io_r,ioproc,
     &                  msgtag,mpi_comm_all,info)
         illen=tmp(lonf,latg+2)
c send the local grid domain
         call mpi_send(tmp(1,1),illen*lonf,mpi_r_io_r,ioproc,
     &                  msgtag,mpi_comm_all,info)
      else
!!
!!     for pes ioproc
        x = 0.0               ! added by moorthi on 2005111700
        if (.not.liope) then
          nproct=nodes
          do j=1,lats_node_a
             lat=global_lats_a(ipt_lats_node_a-1+j)
             do i=1,lonf
                x(i,lat)=xl(i,j)
             enddo
          enddo
        else
          nproct=nodes-1
        endif
        do proc=1,nproct
         if (proc.ne.ioproc+1) then
         msgtag=1000+proc*nodes*maxfld+ifldu
          call mpi_recv(tmp(lonf,latg+1),1,mpi_r_io_r,proc-1,
     &                msgtag,mpi_comm_all,stat,info)
          call mpi_recv(tmp(lonf,latg+2),1,mpi_r_io_r,proc-1,
     &                msgtag,mpi_comm_all,stat,info)
         illen=tmp(lonf,latg+2)
          call mpi_recv(tmp(1,1),illen*lonf ,mpi_r_io_r,proc-1,
     &                msgtag,mpi_comm_all,stat,info)
         if (.not.liope) then
           ipt_lats_node_rl=tmp(lonf,latg+1)
           lats_nodes_rl=tmp(lonf,latg+2)
         else
           ipt_lats_node_rl=tmp(lonf,lats_node_a_max+1)
           lats_nodes_rl=tmp(lonf,lats_node_a_max+2)
         endif
         do j=1,lats_nodes_rl
           lat=global_lats_a(ipt_lats_node_rl-1+j)
           do i=1,lonf
              x(i,lat)=tmp(i,j)
           enddo
         enddo
         endif   !(proc.ne.ioproc+1)
        enddo
!!
      endif
      ncc=ncc+1
 
!!
      return
      end
c
c***********************************************************************
c
      subroutine split2d_r(x,xl,global_lats_r)
c
c***********************************************************************
c
      use resol_def
      use layout1
      use mpi_def
      implicit none
!!
      real(kind=kind_ior) x(lonr,latr)
      real (kind=kind_io8) xl(lonr,lats_node_r)
      real(kind=kind_ior) tmp(lonr,latr)
      integer global_lats_r(latr)
      integer nodesr
!     integer maxfld,nodesr
      integer proc,j,lat,i
!     integer proc,j,lat,msgtag,i,buff,ierr
      integer ifld/0/
      save ifld
      real t1,t2,t3,t4,timef,ta,tb
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!!
      xl=0.
!     maxfld=50
      ifld=ifld+1
!!
      if (io_task) then
        ta=timef()
        t3=ta
c        do proc=1,nodes-1
         do proc=1,1
c
c         sending the data
c         ----------------
         tmp=0.
         do j=1,latr
            do i=1,lonr
              tmp(i,j)=x(i,j)
            enddo
         enddo
!moor    msgtag=1000+proc*nodes*maxfld+ifld
         t1=timef()
!sela    print *,' gwvx broadcasting from ',nodes-1
         call mpi_bcast
     1 (tmp,lonr*latr,mpi_r_io_r,nodes-1,mpi_comm_all,info)
         call mpi_comm_rank(mpi_comm_all,i,info)
c         call mpi_send(tmp,lonr*latr,mpi_r_io_r,proc-1,msgtag,
c     &                  mpi_comm_all,info)
         t2=timef()
!sela    print 102,t2-t1
 
 102    format(' send time ',f10.5)
        enddo
        t4=timef()
      else
        if (.not.liope) then
          nodesr=nodes
        else
          nodesr=nodes+1
        endif
!moor   msgtag=1000+(me+1)*nodesr*maxfld+ifld
!sela    print *,' gwvx broadcastrec  from ',nodesr-1
         call mpi_bcast
     1 (tmp,lonr*latr,mpi_r_io_r,nodesr-1,mpi_comm_all,info)
         call mpi_comm_rank(mpi_comm_all,i,info)
!sela    print *,'gwvx ipt ',ipt
c        call mpi_recv(tmp,lonr*latr,mpi_r_io_r,nodesr-1,
c     &                msgtag,mpi_comm_all,stat,info)
        do j=1,lats_node_r
           lat=global_lats_r(ipt_lats_node_r-1+j)
           do i=1,lonr
              xl(i,j)=tmp(i,lat)
           enddo
        enddo
!!
      endif
!!
!!     for pes nodes-1
      if (.not.liope) then
        if (me.eq.nodes-1) then
          do j=1,lats_node_r
             lat=global_lats_r(ipt_lats_node_r-1+j)
             do i=1,lonr
                xl(i,j)=x(i,lat)
             enddo
          enddo
        endif
      endif
!!
      tb=timef()
         call mpi_comm_rank(mpi_comm_all,i,info)
 
!sela  if (io_task)print 103,tb-ta,t4-t3
 103  format(' global and send times  split2d',2f10.5)
      return
      end

c
c***********************************************************************
c
      subroutine split2d_a(x,xl,global_lats_a)
c
c***********************************************************************
c
      use resol_def
      use layout1
      use mpi_def
      implicit none
!!
      real(kind=kind_ior) x(lonf,latg)
      real (kind=kind_io8) xl(lonf,lats_node_a)
      real(kind=kind_ior) tmp(lonf,latg)
      integer global_lats_a(latg)
      integer nodesr
!     integer maxfld,nodesr
      integer proc,j,lat,i
!     integer proc,j,lat,msgtag,i,buff,ierr
      integer ifld/0/
      save ifld
      real t1,t2,t3,t4,timef,ta,tb
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!!
      xl=0.
!     maxfld=50
      ifld=ifld+1
!!
      if (icolor.eq.2.and.me.eq.nodes-1) then
        ta=timef()
        t3=ta
c        do proc=1,nodes-1
         do proc=1,1
c
c         sending the data
c         ----------------
         tmp=0.
         do j=1,latg
            do i=1,lonf
              tmp(i,j)=x(i,j)
            enddo
         enddo
!moor    msgtag=1000+proc*nodes*maxfld+ifld
         t1=timef()
!sela    print *,' gwvx broadcasting from ',nodes-1
         call mpi_bcast
     1 (tmp,lonf*latg,mpi_r_io_r,nodes-1,mpi_comm_all,info)
         call mpi_comm_rank(mpi_comm_all,i,info)
c         call mpi_send(tmp,lonf*latg,mpi_r_io_r,proc-1,msgtag,
c     &                  mpi_comm_all,info)
         t2=timef()
!sela    print 102,t2-t1
 
 102    format(' send time ',f10.5)
        enddo
        t4=timef()
      else
        if (.not.liope) then
          nodesr=nodes
        else
          nodesr=nodes+1
        endif
!moor   msgtag=1000+(me+1)*nodesr*maxfld+ifld
!sela    print *,' gwvx broadcastrec  from ',nodesr-1
         call mpi_bcast
     1 (tmp,lonf*latg,mpi_r_io_r,nodesr-1,mpi_comm_all,info)
         call mpi_comm_rank(mpi_comm_all,i,info)
!sela    print *,'gwvx ipt ',ipt
c        call mpi_recv(tmp,lonf*latg,mpi_r_io_r,nodesr-1,
c     &                msgtag,mpi_comm_all,stat,info)
        do j=1,lats_node_a
           lat=global_lats_a(ipt_lats_node_a-1+j)
           do i=1,lonf
              xl(i,j)=tmp(i,lat)
           enddo
        enddo
!!
      endif
!!
!!     for pes nodes-1
      if (.not.liope) then
        if (me.eq.nodes-1) then
          do j=1,lats_node_a
             lat=global_lats_a(ipt_lats_node_a-1+j)
             do i=1,lonf
                xl(i,j)=x(i,lat)
             enddo
          enddo
        endif
      endif
!!
      tb=timef()
      call mpi_comm_rank(mpi_comm_all,i,info)
 
!sela  if(icolor.eq.2.and.me.eq.nodes-1)print 103,tb-ta,t4-t3
 103  format(' global and send times  split2d_a',2f10.5)
      return
      end

      subroutine read_sfc_r(sfc_fld,needoro,nread,
     &                    cfile,global_lats_r,lonsperlar)
!
!***********************************************************************
!
      use sfcio_module
      use resol_def
      use layout1
      use mpi_def
      use sfc_flx_esmfmod
      use namelist_soilveg , only : salp_data, snupx
      use physcons, only : tgice => con_tice
      implicit none
!
      type(sfc_var_data)        :: sfc_fld
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)

      integer needoro

      real(kind=kind_ior) buff1(lonr,latr)
!    &,    buff4(lonr,latr,4)
!     real(kind=kind_io4) xhour
c$$$     &     buff4(lonr,latr,4),slmskful(lonr,latr),xhour
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      real(kind=kind_io8) buff3(lonr,lats_node_r)
      integer nread,i,j,k
      character*(*) cfile
      integer kmsk(lonr,latr)
c$$$      common /comfixio/slmskful
      real t1,t2,timef,rsnow
      type(sfcio_head) head
      type(sfcio_dbta) data
      integer iret, vegtyp
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      t1=timef()

      if (io_task) then
        call sfcio_srohdc(nread,cfile,head,data,iret)

        print 99,nread,head%fhour,head%idate,
     &           head%lonb,head%latb,head%lsoil,head%ivs,iret
99      format(1h ,'in fixio nread=',i3,2x,'hour=',f8.2,3x,'idate=',
     &  4(1x,i4),4x,'lonsfc,latsfc,lsoil,ivssfc,iret=',5i8)

        if(iret.ne.0) goto 5000
!       if(head%ivs.ne.200412.and.head%ivs.ne.200501) goto 5000
        if(head%lonb.ne.lonr) goto 5000
        if(head%latb.ne.latr) goto 5000
        if(head%lsoil.ne.lsoil) goto 5000

      endif

      kmsk=0

      if (io_task) buff1=data%tsea
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%tsea,global_lats_r,lonsperlar)

      do k=1, lsoil
        if (io_task) buff1=data%smc(:,:,k)
        call split2d_r(buff1, buffo,global_lats_r)
        call interpred(1,kmsk,buffo,buff3,global_lats_r,lonsperlar)
        sfc_fld%smc(:,k,:) = buff3(:,:)
      enddo

      if (io_task) buff1=data%sheleg
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%sheleg,
     &               global_lats_r,lonsperlar)

      do k = 1, lsoil
        if (io_task) buff1=data%stc(:,:,k)
        call split2d_r(buff1, buffo,global_lats_r)
        call interpred(1,kmsk,buffo,buff3,global_lats_r,lonsperlar)
        sfc_fld%stc(:,k,:) = buff3(:,:)
      enddo

      if (io_task) buff1=data%tg3
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%tg3,global_lats_r,lonsperlar)

      if (io_task) buff1=data%zorl
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%zorl,global_lats_r,lonsperlar)

      sfc_fld%cv  = 0
      sfc_fld%cvb = 0
      sfc_fld%cvt = 0

      if (io_task) buff1=data%alvsf
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%alvsf,
     &               global_lats_r,lonsperlar)
      if (io_task) buff1=data%alvwf
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%alvwf,
     &               global_lats_r,lonsperlar)
      if (io_task) buff1=data%alnsf
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%alnsf,
     &               global_lats_r,lonsperlar)
      if (io_task) buff1=data%alnwf
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%alnwf,
     &               global_lats_r,lonsperlar)

!     the mask cannot be interpolated
      if (io_task) buff1=data%slmsk
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%slmsk,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%vfrac
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%vfrac,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%canopy
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%canopy,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%f10m
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%f10m,global_lats_r,lonsperlar)

      if (io_task) buff1=data%vtype
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%vtype,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%stype
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%stype,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%facsf
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%facsf,
     &               global_lats_r,lonsperlar)
      if (io_task) buff1=data%facwf
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%facwf,
     &               global_lats_r,lonsperlar)

!szunyogh 06/16/99
      if (io_task) buff1=data%uustar
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%uustar,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%ffmm
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%ffmm,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%ffhh
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%ffhh,
     &               global_lats_r,lonsperlar)

!c-- xw: for sea-ice nov04
!    sea-ice (hice/fice) was added to the surface files.

      if (io_task) buff1=data%hice
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%hice,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%fice
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%fice,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%tisfc
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%tisfc,
     &               global_lats_r,lonsperlar)
      if (lats_node_r > 0 )  then
        if (sfc_fld%tisfc(1,1) < 0.0) then
!$omp parallel do private(j,i)
          do j=1,lats_node_r
            do i=1,lonr
              sfc_fld%tisfc(i,j) = sfc_fld%tsea(i,j)
              if(sfc_fld%slmsk(i,j) >=  2. .and.
     &          sfc_fld%fice(i,j)  >= 0.5) then
                sfc_fld%tisfc(i,j) = (sfc_fld%tsea(i,j)
     &         -tgice*(1.-sfc_fld%fice(i,j))) / sfc_fld%fice(i,j)
                sfc_fld%tisfc(i,j) = min(sfc_fld%tisfc(i,j),tgice)
              endif
            enddo
          enddo
        endif
      endif


!c-- xw: end sea-ice

!lu   11/10/2004
!*     surface files for gfs/noah contain 8 additional records:
!*     tprcp, srflag, snwdph, slc, shdmin, shdmax, slope, snoalb

      if (io_task) buff1=data%tprcp
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%tprcp,
     &               global_lats_r,lonsperlar)

!* srflag
      if (io_task) buff1=data%srflag
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%srflag,
     &               global_lats_r,lonsperlar)

!* snwdph
      if (io_task) buff1=data%snwdph
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%snwdph,
     &               global_lats_r,lonsperlar)

!* slc
      do k=1, lsoil
        if (io_task) buff1=data%slc(:,:,k)
        call split2d_r(buff1, buffo,global_lats_r)
        call interpred(1,kmsk,buffo,buff3,global_lats_r,lonsperlar)
        sfc_fld%slc(:,k,:) = buff3(:,:)
      enddo

!* shdmin
      if (io_task) buff1=data%shdmin
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%shdmin,
     &               global_lats_r,lonsperlar)

!* shdmax
      if (io_task) buff1=data%shdmax
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%shdmax,
     &               global_lats_r,lonsperlar)

!* slope
      if (io_task) buff1=data%slope
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%slope,
     &               global_lats_r,lonsperlar)

!* snoalb
      if (io_task) buff1=data%snoalb
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,sfc_fld%snoalb,
     &               global_lats_r,lonsperlar)
!lu [+67l]: the addition of 8 noah records ends here .........................

      if(needoro == 1) then
        if (io_task) then
          buff1=data%orog
          needoro=1
          if(all(data%orog.ne.sfcio_realfill)) needoro=0
          print *,'read sfc orography'
        endif
        call split2d_r(buff1, buffo,global_lats_r)
        call interpred(1,kmsk,buffo,sfc_fld%oro,
     &                 global_lats_r,lonsperlar)
        call skip(needoro)
      endif
!
!wei initialize snow fraction(sheleg is in mm)
!$omp parallel do private(j,i,vegtyp,rsnow)
      do j=1,lats_node_r
        do i=1,lonr
          sfc_fld%sncovr(i,j) = 0.0
          if (sfc_fld%slmsk(i,j) > 0.001) then
            vegtyp = sfc_fld%vtype(i,j)
            if( vegtyp == 0 ) vegtyp = 7
            rsnow  = 0.001*sfc_fld%sheleg(i,j)/snupx(vegtyp)
            if (0.001*sfc_fld%sheleg(i,j) < snupx(vegtyp)) then
              sfc_fld%sncovr(i,j) = 1.0 - ( exp(-salp_data*rsnow)
     &                                    - rsnow*exp(-salp_data))
            else
              sfc_fld%sncovr(i,j) = 1.0
            endif
!          print*,snupx(vegtyp2d(i,j)),salp_data,sfc_fld%sncovr(i,j),
!    & '************debug',sheleg(i,j),vegtyp2d(i,j)
          endif
        enddo
       enddo
!

       if (io_task) then
         call sfcio_axdbta(data,iret)
         t2=timef()
         print *,'fixio time ',t2-t1,t1,t2
       endif
!
      return
 5000 print *, ' error in input in fixio'
      stop
      end
      subroutine read_nst_r(nst_fld, nread, cfile,
     &                     global_lats_r, lonsperlar)
!
!***********************************************************************
!
      use namelist_def
      use nstio_module
      use resol_def
      use layout1
      use mpi_def
      use nst_var_esmfmod
      implicit none
!
      type(nst_var_data)       :: nst_fld
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)

!     real (kind=kind_io8) slmsk(lonr,lats_node_r),

      real(kind=kind_ior) buff1(lonr,latr)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      integer nread,i,j,k
      character*(*) cfile
      integer kmsk(lonr,latr)
      real t1,t2,timef
      type(nstio_head) head
      type(nstio_dbta) data
      integer iret

c
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      t1=timef()

      if (io_task) then
        call nstio_srohdc(nread,cfile,head,data,iret)

        print 99,nread,head%fhour,head%idate,
     &     head%lonb,head%latb,head%lsea,head%ivo,iret
99      format(1h ,'in nstio nread=',i3,2x,'hour=',f8.2,3x,'idate=',
     &  4(1x,i4),4x,'lonnst,latnst,lsea,ivsnst,iret=',5i8)

        if(iret.ne.0) goto 5000
        if(head%lonb.ne.lonr) goto 5000
        if(head%latb.ne.latr) goto 5000
        if(head%lsea.ne.lsea) goto 5000

      endif

      kmsk=0
!
!     assign nst_fld(lonr,lats_node_r)
!
      if (io_task) buff1=data%xt
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%xt,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%xs
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%xs,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%xu
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%xu,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%xv
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%xv,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%xz
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%xz,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%zm
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%zm,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%xtts
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%xtts,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%xzts
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%xzts,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%dt_cool
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%dt_cool,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%z_c
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%z_c,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%c_0
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%c_0,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%c_d
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%c_d,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%w_0
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%w_0,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%w_d
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%w_d,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%d_conv
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%d_conv,
     &               global_lats_r,lonsperlar)


      if (io_task) buff1=data%ifd
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%ifd,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%tref
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%tref,
     &               global_lats_r,lonsperlar)

      if (io_task) buff1=data%qrain
      call split2d_r(buff1, buffo,global_lats_r)
      call interpred(1,kmsk,buffo,nst_fld%qrain,
     &               global_lats_r,lonsperlar)

       if (io_task) then
         call nstio_axdbta(data,iret)
         t2=timef()
         print *,'read_nst_r time ',t2-t1,t1,t2
       endif
!
      return
 5000 print *, ' error in input in read_nst_r'
      stop
      end
!
!***********************************************************************
!
