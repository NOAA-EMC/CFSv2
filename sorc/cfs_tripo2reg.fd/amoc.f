      subroutine amoc(v,x,y,z,idim,jdim,kdim,spv0,clat,nreg,vmoc)

! stream function based on V (overturning)
!  v= 3-D meridional current
!  x= longitude coordinate
!  y= latitude  coordinate
!  z= depth     coordinate
! dx= x-grid size
! dy= y-grid size
! dz= z-grid size
! idim= x-dimension
! jdim= y-dimension
! kdim= z-dimension
! nreg= number of regions
! spv0= mask (missing) value
! clat= latitude for tripolar coordinate
! vmoc= outputing meridional overturning circulation for globe, ind-pac, pac, ind, and atl
! Boyin Huang 2010/01/10
! Xingren Wu - 01/22/2010 revised

      implicit none

      integer idim,jdim,kdim,nreg,ims,ime

      real, dimension(idim,jdim,kdim) :: vvel,v
      integer, dimension(jdim,kdim)   :: kyz

      real, dimension(jdim,kdim) :: vyzg,vyzpi,vyza,vyzp,vyzi
      real, dimension(jdim,kdim) :: syzg,syzpi,syza,syzp,syzi
      real, dimension(jdim,kdim) :: ayzg,ayzpi,ayza,ayzp,ayzi
      real, dimension(jdim,kdim,nreg) :: vmoc

      real, dimension(idim) :: x,xv,dx
      real, dimension(jdim) :: y,dy
      real, dimension(kdim) :: z,dz
      real, dimension(idim,jdim) :: dm,v2
      integer, dimension(idim,jdim) :: msk

      logical lrot

      integer i,i0,j,j0,k,nac
      real clat, spv, spv0, pi, rd, yc, yitf, ymin, v0
      data spv/-9999.0/
!
      ime=idim
      if (x(1) .LT. 0.) then
         ime=0
         do i=1,idim
            ime=ime+1
            if (x(i) .GE. 0.) then
               ime=ime-1
               go to 11
            endif
         enddo
      endif
      print *, 'All lons are negative'
      stop
 11   continue
      ims=idim-ime

      xv(:)=x(:)
      do i=1,ims
         x(i)=xv(i+ime)
      enddo
      do i=ims+1,idim
         x(i)=xv(i-ims)+360.
      enddo

      do k=1,kdim
      do j=1,jdim
      do i=1,ims
         v0=v(i+ime,j,k)
         if (abs(v0).ge.abs(spv0))  then
            vvel(i,j,k)=spv
         else
            vvel(i,j,k)=v0
         endif
      enddo
      do i=ims+1,idim
         v0=v(i-ims,j,k)
         if (abs(v0).ge.abs(spv0))  then
            vvel(i,j,k)=spv
         else
            vvel(i,j,k)=v0
         endif
      enddo
      enddo
      enddo
!
      do i=1,idim
         if (x(i).lt.0.) then
             x(i)=x(i)+360.
             print *,'WARNING: Lon is negative @ grid(i): ',i
         endif
      enddo

      do i=2,idim-1
         dx(i)=0.5*(x(i+1)-x(i-1))
      enddo
      dx(1)=0.5*(360.+x(2)-x(idim))
      dx(idim)=0.5*(360.+x(1)-x(idim-1))

      do j=2,jdim-1
         dy(j)=0.5*(y(j+1)-y(j-1))
      enddo
      dy(1)=y(2)-y(1)
      dy(jdim)=y(jdim)-y(jdim-1)

      do k=2,kdim-1
         dz(k)=0.5*(z(k+1)-z(k-1))
      enddo
      dz(1)=z(2)-z(1)
      dz(kdim)=z(kdim)-z(kdim-1)

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      v2(:,:)=vvel(:,:,1)
      call mask(v2,x,y,idim,jdim,msk)

      pi=4. * atan(1.0)
      rd=6.37e6

      do 36 j=1,jdim
      dy(j) = dy(j) * pi/180.*rd
   36 continue

      do 38 j=1,jdim
      do 38 i=1,idim
      dm(i,j)=dx(i)*pi/180.*rd*cos(y(j)*pi/180.)
   38 continue

! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! y_z stream function

      do 500 j=1,jdim				!global
      do 500 k=1,kdim
      vyzg(j,k)=0.0
      kyz (j,k)=1
      nac      =0
      do 510 i=1,idim
      if(vvel(i,j,k).ne.spv .and. &
         (msk(i,j).eq.2 .or. msk(i,j).eq.3 .or. msk(i,j).eq.4) ) then
        vyzg(j,k)=vyzg(j,k)+vvel(i,j,k)*dm(i,j)
        nac=nac+1
      endif
  510 continue
      if(nac.eq.0) then
        kyz (j,k)=0
       ayzg(j,k)=spv
      else
       ayzg(j,k)=vyzg(j,k)/dm(1,j)/float(nac)
      endif
  500 continue
 
      do 515 j=1,jdim
      do 515 k=1,kdim
      syzg(j,k)=0.0
  515 continue

      do 520 j=1,jdim
      syzg(j,1)=+vyzg(j,1)*dz(1)
      do 522 k=2,kdim
      syzg(j,k)=syzg(j,k-1)+vyzg(j,k)*dz(k)       !the '+' from dz: 0 at surface
  522 continue
  520 continue

      do 525 j=1,jdim
      do 525 k=1,kdim
      if(kyz(j,k).ne.0) then
      syzg(j,k)=syzg(j,k)*1.e-6
      else 
      syzg(j,k)=spv
      endif
  525 continue

! ++++++++++++++++++++++++++++++++++++++++++++++
      do 600 j=1,jdim                           !Pacific + Indian O
      do 600 k=1,kdim
      vyzpi(j,k)=0.0
      kyz (j,k)=1
      nac      =0
      do 610 i=1,idim
      if(vvel(i,j,k).ne.spv .and. &
         (msk(i,j).eq.2 .or. msk(i,j).eq.3) ) then
        vyzpi(j,k)=vyzpi(j,k)+vvel(i,j,k)*dm(i,j)
        nac=nac+1
      endif
  610 continue
      if(nac.eq.0) then
        kyz (j,k)=0
       ayzpi(j,k)=spv
      else
       ayzpi(j,k)=vyzpi(j,k)/dm(1,j)/float(nac)
      endif
  600 continue

      do 615 j=1,jdim
      do 615 k=1,kdim
      syzpi(j,k)=0.0
  615 continue
      
      do 620 j=1,jdim
      syzpi(j,1)=+vyzpi(j,1)*dz(1)
      do 622 k=2,kdim
      syzpi(j,k)=syzpi(j,k-1)+vyzpi(j,k)*dz(k)
  622 continue
  620 continue

      do 625 j=1,jdim
      do 625 k=1,kdim
      if(kyz(j,k).ne.0) then
      syzpi(j,k)=syzpi(j,k)*1.e-6
      else
      syzpi(j,k)=spv
      endif
  625 continue

! ++++++++++++++++++++++++++++++++++++++++++++++
      do 700 j=1,jdim                           !Atlantic
      do 700 k=1,kdim
      vyza(j,k)=0.0
      kyz (j,k)=1
      nac      =0
      do 710 i=1,idim
      if(vvel(i,j,k).ne.spv .and. msk(i,j).eq.4) then
        vyza(j,k)=vyza(j,k)+vvel(i,j,k)*dm(i,j)
        nac=nac+1
      endif
  710 continue
      if(nac.eq.0) then
        kyz (j,k)=0
       ayza(j,k)=spv
      else
       ayza(j,k)=vyza(j,k)/dm(1,j)/float(nac)
      endif
  700 continue

      do 715 j=1,jdim
      do 715 k=1,kdim
      syza(j,k)=0.0
  715 continue
  
      do 720 j=1,jdim
      syza(j,1)=+vyza(j,1)*dz(1)
      do 722 k=2,kdim
      syza(j,k)=syza(j,k-1)+vyza(j,k)*dz(k)
  722 continue
  720 continue

      do 725 j=1,jdim
      do 725 k=1,kdim
      if(kyz(j,k).ne.0) then
      syza(j,k)=syza(j,k)*1.e-6
      else
      syza(j,k)=spv
      endif
  725 continue

! ++++++++++++++++++++++++++++++++++++++++++++++
      yitf=-3.0					!ITF latitude
      ymin=90.0
      do j=1,jdim			
      yc=abs(y(j)-yitf)
      if(yc.lt.ymin) then
      ymin=yc
      j0=j
      endif
      enddo
    
      do 1700 j=1,jdim                           !Pacific
      do 1700 k=1,kdim
      vyzp(j,k)=0.0
      kyz (j,k)=1
      nac      =0
      do 1710 i=1,idim
      if(vvel(i,j,k).ne.spv .and. msk(i,j).eq.3) then
        vyzp(j,k)=vyzp(j,k)+vvel(i,j,k)*dm(i,j)
        nac=nac+1
      endif
 1710 continue
      if(y(j).le.0.0) then
        do i=1,idim  
        if(x(i).ge.120.and.x(i).le.140) then 	!longitude for ITF in GODAS
        if(vvel(i,j0,k).ne.spv) then
        vyzp(j,k)=vyzp(j,k)+vvel(i,j0,k)*dm(i,j0)
        endif
        endif
        enddo
      endif

      if(nac.eq.0) then
        kyz (j,k)=0
       ayzp(j,k)=spv
      else
       ayzp(j,k)=vyzp(j,k)/dm(1,j)/float(nac)
      endif
 1700 continue

      do 1715 j=1,jdim
      do 1715 k=1,kdim
      syzp(j,k)=0.0
 1715 continue

      do 1720 j=1,jdim
      syzp(j,1)=+vyzp(j,1)*dz(1)
      do 1722 k=2,kdim
      syzp(j,k)=syzp(j,k-1)+vyzp(j,k)*dz(k)
 1722 continue
 1720 continue

      do 1725 j=1,jdim
      do 1725 k=1,kdim
      if(kyz(j,k).ne.0) then
      syzp(j,k)=syzp(j,k)*1.e-6
      else
      syzp(j,k)=spv
      endif
 1725 continue

! ++++++++++++++++++++++++++++++++++++++++++++++
      do 2700 j=1,jdim                           !Indian
      do 2700 k=1,kdim
      vyzi(j,k)=0.0
      kyz (j,k)=1
      nac      =0
      do 2710 i=1,idim
      if(vvel(i,j,k).ne.spv .and. msk(i,j).eq.2) then
        vyzi(j,k)=vyzi(j,k)+vvel(i,j,k)*dm(i,j)
        nac=nac+1
      endif
 2710 continue
      if(nac.eq.0) then
        kyz (j,k)=0
       ayzi(j,k)=spv
      else
       ayzi(j,k)=vyzi(j,k)/dm(1,j)/float(nac)
      endif
 2700 continue

      do 2715 j=1,jdim
      do 2715 k=1,kdim
      syzi(j,k)=0.0
 2715 continue

      do 2720 j=1,jdim
      syzi(j,1)=+vyzi(j,1)*dz(1)
      do 2722 k=2,kdim
      syzi(j,k)=syzi(j,k-1)+vyzi(j,k)*dz(k)
 2722 continue
 2720 continue

      do 2725 j=1,jdim
      do 2725 k=1,kdim
      if(kyz(j,k).ne.0) then
      syzi(j,k)=syzi(j,k)*1.e-6
      else
      syzi(j,k)=spv
      endif
 2725 continue

      do 2730 k=1,kdim                           !topography
      do j =1,jdim
      if(y(j).lt.-40.0) then
!       syzg(j,k)=spv
       syzpi(j,k)=spv
        syzp(j,k)=spv
        syzi(j,k)=spv
        syza(j,k)=spv
      endif
      if(y(j).gt.clat) then
        syzg(j,k)=spv
       syzpi(j,k)=spv
        syzp(j,k)=spv
        syzi(j,k)=spv
        syza(j,k)=spv
      endif
      enddo
        syzg(jdim,k)=spv
       syzpi(jdim,k)=spv
        syzp(jdim,k)=spv
        syzi(jdim,k)=spv
        syza(jdim,k)=spv
 2730 continue

      do j=1,jdim
      do k=1,kdim
      vmoc(j,k,1)=syzg (j,k)
      vmoc(j,k,2)=syzpi(j,k)
      vmoc(j,k,3)=syzp (j,k)
      vmoc(j,k,4)=syzi (j,k)
      vmoc(j,k,5)=syza (j,k)
      enddo
      enddo

      return
      end subroutine amoc

! calculate model ocean masks

      subroutine mask(sst,rlon,rlat,idim,jdim,msk)

      implicit none

      integer idim,jdim
      integer i,j,k
      real    rln,rl1,rl2

      real sst(idim,jdim)
      real rlat(jdim),rlon(idim)
      integer msk(idim,jdim)

      do 10 i=1,idim
      do 10 j=1,jdim
      msk(i,j)=0
      if(sst(i,j).gt.-8888.) msk(i,j)=1
   10 continue

! indian ocean

      do 20 j=1,jdim
      IF (rlat(j) .GT. 30.) go to 20
      do i=1,idim
        if(rlat(j).le.0.0.and.msk(i,j).eq.1) then
          if(rlon(i).gt.20.0.and.rlon(i).le.135.0) then
          msk(i,j)=2
          endif
        else if(rlat(j).le.30.0.and.msk(i,j).eq.1) then
          if(rlon(i).gt.45.0.and.rlon(i).le.100.0) then
          msk(i,j)=2
          endif
        endif
      enddo
  20  continue

! pacific

      do 30 i=1,idim
      IF (rlon(i) .LE. 100.) go to 30
      IF (rlon(i) .GT. 290.) go to 30
      do j=1,jdim
        if(rlat(j).le.-75.0.and.msk(i,j).eq.1) then
          if(rlon(i).gt.135.0.and.rlon(i).le.270.0) then
          msk(i,j)=3
          endif
        else if(rlat(j).le. 4.0.and.msk(i,j).eq.1) then
          if(rlon(i).gt.100.0.and.rlon(i).le.290.0) then
          msk(i,j)=3
          endif
        else if(rlat(j).le.18.0.and.msk(i,j).eq.1) then
!         rln=260.+(290.-260.)/(4.-18.)*(rlat(j)-18.)
          rln=260.+(18.-rlat(j))*30./14.
          if(rlon(i).gt.100.0.and.rlon(i).le.rln) then
          msk(i,j)=3
          endif
        else if(rlat(j).le.68.0.and.msk(i,j).eq.1) then
          if(rlon(i).gt.100.0.and.rlon(i).le.260.) then
          msk(i,j)=3
          endif
        endif
      enddo
  30  continue

! atlantic

      do 40 j=1,jdim
      do 40 i=1,idim
        if(rlat(j).le.-75.0.and.msk(i,j).eq.1) then
          if(rlon(i).gt.270.0.or.rlon(i).le.20.0) then
          msk(i,j)=4
          endif
        else if(rlat(j).le. 0.0.and.msk(i,j).eq.1) then
          if(rlon(i).gt.290.0.or.rlon(i).le.20.0) then
          msk(i,j)=4
          endif
        else if(rlat(j).le. 4.0.and.msk(i,j).eq.1) then
!         rln=355.+(380.-355.)/(0.-35.)*(rlat(j)-35.)
          rln=355.+(35.-rlat(j))*25./35.
          if(rln.lt.360.) then
            if(rlon(i).gt.290.0.and.rlon(i).le.rln) then
            msk(i,j)=4
            endif
          else
            rln=rln-360.
            if(rlon(i).gt.290.0.or.rlon(i).le.rln) then
            msk(i,j)=4
            endif
          endif
        else if(rlat(j).le.18.0.and.msk(i,j).eq.1) then
!         rl1=260.+(290.-260.)/(4.-18.)*(rlat(j)-18.)
!         rl2=355.+(380.-355.)/(0.-35.)*(rlat(j)-35.)
          rl1=260.+(18.-rlat(j))*30./14.
          rl2=355.+(35.-rlat(j))*25./35.
          if(rl2.lt.360.) then
            if(rlon(i).gt.rl1.and.rlon(i).le.rl2) then
            msk(i,j)=4
            endif
          else
            rl2=rl2-360.
            if(rlon(i).gt.rl1.or.rlon(i).le.rl2) then
            msk(i,j)=4
            endif
          endif
        else if(rlat(j).le.35.0.and.msk(i,j).eq.1) then
!         rl2=355.+(380.-355.)/(0.-35.)*(rlat(j)-35.)
          rl2=355.+(35.-rlat(j))*25./35.
          if(rl2.lt.360.) then
            if(rlon(i).gt.260.and.rlon(i).le.rl2) then
            msk(i,j)=4
            endif
          else
            rl2=rl2-360.
            if(rlon(i).gt.260.or.rlon(i).le.rl2) then
            msk(i,j)=4
            endif
          endif
        else if(rlat(j).le.67.0.and.msk(i,j).eq.1) then
!         rl2=375.+(355.-375.)/(35.-67.)*(rlat(j)-67.)
          rl2=375.+(rlat(j)-67.)*20./32.
          if(rl2.lt.360.) then
            if(rlon(i).gt.260.and.rlon(i).le.rl2) then
            msk(i,j)=4
            endif
          else
            rl2=rl2-360.
            if(rlon(i).gt.260.or.rlon(i).le.rl2) then
            msk(i,j)=4
            endif
          endif
        else if(rlat(j).le.80.0.and.msk(i,j).eq.1) then
!         rl2=340.+(375.-340.)/(67.-80.)*(rlat(j)-80.)
          rl2=340.+(80.-rlat(j))*35./13.
          if(rl2.lt.360.) then
            if(rlon(i).gt.260.and.rlon(i).le.rl2) then
            msk(i,j)=4
            endif
          else
            rl2=rl2-360.
            if(rlon(i).gt.260.or.rlon(i).le.rl2) then
            msk(i,j)=4
            endif
          endif
        endif
  40  continue

! arctic 

      do 50 j=1,jdim
      IF (rlat(j) .LE. 68) go to 50
      do i=1,idim
      if(rlon(j).gt.15.0.and.rlon(j).le.260.0.and.msk(i,j).eq.1) then
          if(rlat(j).gt.68.) then
          msk(i,j)=5
          endif
      elseif(rlon(j).gt.260..and.rlon(j).le.340..and.msk(i,j).eq.1)then
          if(rlat(j).gt.80.) then
          msk(i,j)=5
          endif
      elseif(rlon(j).gt.340..or.rlon(j).le.15..and.msk(i,j).eq.1)then
          rl2=80+(67.-80.)/(375.-340.)*(rlon(i)-340)
          if(rlat(j).gt.rl2) then
          msk(i,j)=5
          endif
      endif
      enddo
  50  continue

      return
      end subroutine mask
