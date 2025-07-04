!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine gblevn12(kbak)

      USE GBLEVN_MODULE

      data rog /29.261/

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      call allobaks ! allocate background arrays

! copy a background time level to 4d arrays

      call fill_ns(iarzs(1,1,kbak)  ,iar12z,imax,jmax,1)
      call fill_ns(iarps(1,1,kbak)  ,iar13p,imax,jmax,1)
      call fill_ns(iarqq(1,1,1,kbak),iar17q,imax,jmax,kmax)
      call fill_ns(iartt(1,1,1,kbak),iar14t,imax,jmax,kmax)
      call fill_ns(iaruu(1,1,1,kbak),iar15u,imax,jmax,kmax)
      call fill_ns(iarvv(1,1,1,kbak),iar16v,imax,jmax,kmax)
      call fill_ns(iarpl(1,1,1,kbak),iarpsl,imax,jmax,kmax)

! compute and store height profiles the gsi way                

      do k=1,kmax 
      do j=1,1
!$omp parallel do private(i,pwgt,tmid,zblo)
      do i=1,imax*jmax
      if(k==1) then
         pwgt=log(iarpl(i,j,k,kbak)/iarps(i,j,kbak))
         tmid=iartt(i,j,k,kbak) ! this is not quite right, but copied from gsi
         zblo=iarzs(i,j,kbak)
         iarzl(i,j,k,kbak)=zblo-rog*tmid*pwgt                       
         iarzl(i,j,k,kbak)=iarzl(i,j,k,kbak)-iarzs(i,j,kbak) ! convert to height above the surface
      else
         pwgt=log(iarpl(i,j,k,kbak)/iarpl(i,j,k-1,kbak))
         tmid=.5*(iartt(i,j,k-1,kbak)+iartt(i,j,k,kbak))
         zblo=iarzl(i,j,k-1,kbak)
         iarzl(i,j,k,kbak)=zblo-rog*tmid*pwgt
      endif
      enddo
      enddo
      enddo

      return
      end
c------------------------------------------------------------------
c------------------------------------------------------------------
      subroutine suterps

      USE GBLEVN_MODULE

      real slat(jmax),wlat(jmax)

      print*,'suterps',idrt,imax,jmax,kmax
      call splat(idrt,jmax,slat,wlat)
      rad2deg=180./acos(-1.)

      if(idrt==0.or.idrt==4) then ! equal grid including poles
         allocate(slats(jmax))
         slats(:)=-asin(slat(:))*rad2deg
         dlat=180./float(jmax-1)
      elseif(idrt==256.or.idrt==4) then ! equal grid not incl poles
         allocate(slats(jmax+2))
         slats(1)=-90.00
         slats(2:jmax+1)=-asin(slat(:))*rad2deg
         slats(jmax+2)=90.00
         dlat=180./float(jmax);jmax=jmax+2
      else
         print*,'in suterps, idrt=',idrt
         call bort('suterps - bad idrt value')
      endif

      return

      print*
      print*,'after suterps'
      print*,idrt,imax,jmax,kmax,dlon,dlat
      do j=1,jmax
      print'(i8,2f8.2)',j,slats(j),(j-1)*dlat-90.
      enddo
      print*
         
      return
      end
c------------------------------------------------------------------
c------------------------------------------------------------------
      subroutine fill_ns(gout,gin,ix,jx,kx)
      USE GBLEVN_MODULE

      real(8)  gin(ix,jx,kx)
      real(4) gout(ix,jx,kx)

      if(idrt==0.or.idrt==4) then
         gout(:,:,:)=gin(:,:,:)
      elseif(idrt==4.or.idrt==256) then                       
         gout(:,2:jx+1,:)=gin(:,1:jx,:)
         do k=1,kx
         gout(1:ix,   1,k)=sum(gout(1:ix,   2,k))/float(ix)
         gout(1:ix,jx+2,k)=sum(gout(1:ix,jx+1,k))/float(ix)
         enddo
      endif

      return
      end
