! This program reads wind on grid points, transform them back to
! spectrum space, and then computes psi, chi, vorticity, and divergence
! Author: Saha
! History: Chuang: modified to be used by ncep post and to add dynamical allocation
! April 2010   Moorthi : Updated to use less memory, speedup and implicit none
!
      implicit none
!
      integer, parameter :: komax=100
      character*200 grbwnd,indwnd
      character*200 file,psichifile
      character*10 kyr,kmth
!
      real, allocatable ::  buff(:)
      real, allocatable ::  ui(:,:,:), vi(:,:,:), uo(:,:,:), vo(:,:,:)
      real, allocatable ::  div(:,:,:),  zo(:,:,:)
      real, allocatable ::  psio(:,:,:), so(:,:,:)
!
      integer KPDS(200), KGDS(22), JPDS(200), JGDS(22)
      integer, allocatable :: npress(:)
!
      logical*1, allocatable :: lbms(:,:), lb(:)
      real    :: po(komax), th(komax), pv(komax)
!
      integer :: idim, jdim, kdim, ldim, nt, nl, kl, klb, num_threads
     &,          n, lev, klen, jcap, idrt, ntimes, k, ierr, iret, kskp
     &,          ndata, kpo, kth, kpv, maxdim
      integer :: imax, jmax
      integer :: num_parthds
      data       imax/1760/, jmax/880/
      namelist/nampgb/kpo,po,kth,th,kpv,pv,imax,jmax
!
       read(5,nampgb,iostat=ierr)
       if(ierr == 0)then
         kdim = kpo
	 allocate (npress(kdim))
	 do k=1,kdim
	   npress(k) = nint(po(k))
	 end do  
         if (npress(kdim) == 0 ) then 
           print*,'kdim, npress = ',kdim,npress
           print *,' May be you should use Pa instead of mb for'
     &,            ' pressure?'
         endif
       else
         call abort
       end if	 
       maxdim = imax*jmax
       allocate (lb(maxdim))
       allocate (buff(maxdim))
       call getenv("PGBOUT",grbwnd)
       write(*,*) "grbwnd= ",grbwnd
!
       call getenv("PGIOUT",indwnd)
       write(*,*) "indwnd= ",indwnd
!
       call getenv("psichifile",psichifile)
       write(*,*) "psichifile= ",psichifile
!
!       call getenv("YY",kyr)
!       read(kyr,'(i4)') iyr
!       write(*,*) "year= ",iyr
!
!       call getenv("MM",kmth)
!       read(kmth,'(i2)') imth
!       write(*,*) "month= ",imth
!
       call baopenr(11,grbwnd,ierr)
         if(ierr.ne.0) then
         print *,'error opening file ',grbwnd
         call abort
       endif
       call baopenr(12,indwnd,ierr)
         if(ierr.ne.0) then
         print *,'error opening file ',indwnd
         call abort
       endif
!
       call baopenwt(51,psichifile,ierr)
         if(ierr.ne.0) then
         print *,'error opening file ',psichifile
         call abort
       endif
! get grid dimensions
       jpds = -1
       jgds = -1
       CALL GETGB(11,12,maxdim,0,JPDS,JGDS,
     *            NDATA,KSKP,KPDS,KGDS,LB,buff,IRET) 
       if (iret == 0)then
         idim = kgds(2)
	 jdim = kgds(3)
	 idrt = kgds(1)
	 if(idrt == 0)then
	   jcap = (jdim-3)/2
	 else
	   jcap = jdim-1
	 end if    
	 print*,'idim,jdim idrt,jcap= ',idim,jdim,idrt,jcap
       else
         call abort	 
       end if 	     
       num_threads = num_parthds()
       ldim        = min(kdim,num_threads)
!                                       allocate arrays now
      allocate (ui(idim,jdim,ldim))
      allocate (vi(idim,jdim,ldim))
      allocate (uo(idim,jdim,ldim))
      allocate (vo(idim,jdim,ldim))
      allocate (div(idim,jdim,ldim))
      allocate (zo(idim,jdim,ldim))
      allocate (psio(idim,jdim,ldim))
      allocate (so(idim,jdim,ldim))
      allocate (lbms(idim,jdim))
!     allocate deg(jdim)
                  
!      do j=1,jdim
!      deg(j)=90.0-float(j-1)*2.5
!      enddo
!
      ntimes = (kdim-1)/ldim + 1
      do nt=1,ntimes
        klb = (nt-1)*ldim
        klen = ldim
        if (klb+klen > kdim) klen = kdim - klb
        do kl=1,klen
          nl  = kl + klb
          lev = npress(nl)
!
          N        = -1
          JPDS     = -1
          JPDS(5)  = 33
          JPDS(6)  = 100
          JPDS(7)  = lev
!         JPDS(8)  = mod(IYR-1,100) + 1
!         JPDS(9)  = IMTH
!         JPDS(21) = ((IYR-1)/100) + 1
          JGDS     = -1
          CALL GETGB(11,12,idim*jdim,N,JPDS,JGDS,NDATA,KSKP,KPDS,KGDS,
     &               LBMS,UI(1,1,kl),IRET)
          if(iret.ne.0) then
            print *,' error in GETGB for rc = ',iret,jpds
            call abort
          endif
!
          N        = -1
          JPDS     = -1
          JPDS(5)  = 34
          JPDS(6)  = 100
          JPDS(7)  = lev
!         JPDS(8)  = mod(IYR-1,100) + 1
!         JPDS(9)  = IMTH
!         JPDS(21) = ((IYR-1)/100) + 1
          JGDS     = -1
          CALL GETGB(11,12,idim*jdim,N,JPDS,JGDS,NDATA,KSKP,KPDS,KGDS,
     &               LBMS,VI(1,1,kl),IRET)
          if(iret.ne.0) then
            print *,' error in GETGB for rc = ',iret,jpds
            call abort
          endif
        enddo             ! end of kl loop
!
!       print*,'sample ui,vi=',ui(idim/2,jdim/2),vi(idim/2,jdim/2)
        call sptrunv(0,jcap,idrt,idim,jdim,idrt,idim,jdim,klen,
     &               0,0,0,0,0,0,0,0,ui(1,1,1),vi(1,1,1),
     &              .false.,uo(1,1,1),vo(1,1,1),.false.,div,zo,.true.
     &              ,psio(1,1,1),so(1,1,1))
!
!        call gridav(ui(1:idim,1:jdim,nl),idim,jdim,deg,globu)
!        call gridav(vi(1:idim,1:jdim,nl),idim,jdim,deg,globv)
!        call gridav(po(1:idim,1:jdim,nl),idim,jdim,deg,globp)
!        call gridav(so(1:idim,1:jdim,nl),idim,jdim,deg,globs)
!        print *,iyr,imth,lev,globu,globv,globp,globs
!
        do kl=1,klen
          nl  = kl + klb
          KPDS(5)  = 35
          KPDS(22) = -4
	  KPDS(7)  = npress(nl)
          CALL PUTGB(51,NDATA,KPDS,KGDS,LBMS,SO(1:idim,1:jdim,kl)
     &	  ,IRET)
          if(iret.ne.0) then
            print *,' error in PUTGB for psi for iret ',iret,
     *        (kpds(k),k=5,7)
            call abort
          endif
!
          KPDS(5)  = 36
          KPDS(22) = -4
	  KPDS(7)  = npress(nl)
          CALL PUTGB(51,NDATA,KPDS,KGDS,LBMS,PsiO(1:idim,1:jdim,kl)
     &	  ,IRET)
          if(iret.ne.0) then
            print *,' error in PUTGB for chi for iret ',iret,
     *        (kpds(k),k=5,7)
            call abort
          endif
!
        enddo               ! end of kl loop
      enddo                 ! end of nt loop
!
      stop
      end
