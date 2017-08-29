!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                                                                              !
!  This program reads ice concentration file for CFS reanalysis                !
!  It contains: getfice, getsst, fice2nc,hsno2nc, FillLand, extrap             !
!                                                                              !
!     Xingren Wu   (Xingren.Wu@noaa.gov)                                       !
!     2007-08-16                                                               !
!     Modified by Moorthi on 20071101
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

      module fice_intp_mode

      implicit none

contains

      subroutine fice2nc(ofile,im,jm,x,y,fi)
      INCLUDE "netcdf.inc"
      character(len=120) :: ofile
      character(len=10)  :: charymd
      integer x_dim,y_dim,time_dim
      integer im,jm,status,ncid
      integer x_id,y_id,time_id,fi_id
      integer vardim(3),startvar(3),countvar(3)
      integer iyr,imth,iday

      real*8 x(im),y(jm),fi(im,jm)
      real*8 time

      print *,'Year,Month,Day (e.g. 1999,12,31):'
      read (5,*) iyr,imth,iday

      write (charymd,'(I4,"-",I2.2,"-",I2.2)') iyr,imth,iday

      status = nf_create(ofile,nf_write,ncid)

      status = nf_def_dim(ncid,'x', im, x_dim)
      status = nf_def_dim(ncid,'y', jm, y_dim)
      status = nf_def_dim(ncid,'time', nf_unlimited, time_dim) 
      
      status = nf_def_var(ncid,'x',nf_float,1,x_dim,x_id)
      status = nf_def_var(ncid,'y',nf_float,1,y_dim,y_id)
      status = nf_def_var(ncid,'time',nf_double,1,time_dim,time_id)

      vardim= (/x_dim,y_dim,time_dim/) 
      status = nf_def_var(ncid,'ICECSFC',nf_double, 3,vardim,fi_id)

      status = nf_put_att_text(ncid,x_id,'long_name',      9, 'longitude')
      status = nf_put_att_text(ncid,x_id,'units',          9, 'degrees_E')
      status = nf_put_att_text(ncid,x_id,'cartesian_axis', 1, 'X')

      status = nf_put_att_text(ncid,y_id,'long_name',      8, 'latitude')
      status = nf_put_att_text(ncid,y_id,'units',          9, 'degrees_N')
      status = nf_put_att_text(ncid,y_id,'cartesian_axis', 1, 'Y')

      status = nf_put_att_text(ncid,time_id,'long_name',    4, 'time')
      status = nf_put_att_text(ncid,time_id,'units',        10, charymd)
      status = nf_put_att_text(ncid,time_id,'cartesian_axis',1, 'T')
      status = nf_put_att_text(ncid,time_id,'calendar_type', 6,   'JULIAN')
      status = nf_put_att_text(ncid,time_id,'calendar',      6,   'JULIAN')
      status = nf_put_att_text(ncid,time_id,'bounds',       11,   'time_bounds')

      status = nf_put_att_text(ncid,fi_id,'long_name',	  17, 'ice concentration')
      status = nf_put_att_text(ncid,fi_id,'units',	   8, 'fraction')
      status = nf_put_att_real(ncid,fi_id,'missing_value', nf_real, 1, -1.e+34)

      status = nf_enddef(ncid)
      
      status = nf_put_vara_double(ncid,x_id,1,im,x)
      status = nf_put_vara_double(ncid,y_id,1,jm,y)
      status = nf_put_vara_double(ncid,time_id,1,1,time)
      
      startvar = (/1,1,1/)
      countvar = (/im,jm,1/)  
      status = nf_put_vara_double(ncid,fi_id,startvar,countvar,fi)
      status=nf_close(ncid)

      return
      end subroutine fice2nc

      subroutine sst2nc(ofile,im,jm,x,y,sst)
      INCLUDE "netcdf.inc"
      character(len=120) :: ofile
      character(len=10)  :: charymd
      integer x_dim,y_dim,time_dim
      integer im,jm,status,ncid
      integer x_id,y_id,time_id,sst_id
      integer vardim(3),startvar(3),countvar(3)
      integer iyr,imth,iday

      real*8 x(im),y(jm),sst(im,jm)
      real*8 time

      print *,'Year,Month,Day (e.g. 1999,12,31):'
      read (5,*) iyr,imth,iday

      write (charymd,'(I4,"-",I2.2,"-",I2.2)') iyr,imth,iday

      status = nf_create(ofile,nf_write,ncid)

      status = nf_def_dim(ncid,'x', im, x_dim)
      status = nf_def_dim(ncid,'y', jm, y_dim)
      status = nf_def_dim(ncid,'time', nf_unlimited, time_dim) 
      
      status = nf_def_var(ncid,'x',nf_float,1,x_dim,x_id)
      status = nf_def_var(ncid,'y',nf_float,1,y_dim,y_id)
      status = nf_def_var(ncid,'time',nf_double,1,time_dim,time_id)

      vardim= (/x_dim,y_dim,time_dim/) 
      status = nf_def_var(ncid,'TMPSFC',nf_double, 3,vardim,sst_id)

      status = nf_put_att_text(ncid,x_id,'long_name',      9, 'longitude')
      status = nf_put_att_text(ncid,x_id,'units',          9, 'degrees_E')
      status = nf_put_att_text(ncid,x_id,'cartesian_axis', 1, 'X')

      status = nf_put_att_text(ncid,y_id,'long_name',      8, 'latitude')
      status = nf_put_att_text(ncid,y_id,'units',          9, 'degrees_N')
      status = nf_put_att_text(ncid,y_id,'cartesian_axis', 1, 'Y')

      status = nf_put_att_text(ncid,time_id,'long_name',    4, 'time')
      status = nf_put_att_text(ncid,time_id,'units',        10, charymd)
      status = nf_put_att_text(ncid,time_id,'cartesian_axis',1, 'T')
      status = nf_put_att_text(ncid,time_id,'calendar_type', 6,   'JULIAN')
      status = nf_put_att_text(ncid,time_id,'calendar',      6,   'JULIAN')
      status = nf_put_att_text(ncid,time_id,'bounds',       11,   'time_bounds')

      status = nf_put_att_text(ncid,sst_id,'long_name',	  23, 'sea surface temperature')
      status = nf_put_att_text(ncid,sst_id,'units',	   1, 'K')
      status = nf_put_att_real(ncid,sst_id,'missing_value', nf_real, 1, -1.e+34)

      status = nf_enddef(ncid)
      
      status = nf_put_vara_double(ncid,x_id,1,im,x)
      status = nf_put_vara_double(ncid,y_id,1,jm,y)
      status = nf_put_vara_double(ncid,time_id,1,1,time)
      
      startvar = (/1,1,1/)
      countvar = (/im,jm,1/)  
      status = nf_put_vara_double(ncid,sst_id,startvar,countvar,sst)
      status=nf_close(ncid)

      return
      end subroutine sst2nc

      subroutine hsno2nc(ofile,im,jm,x,y,hs)
      INCLUDE "netcdf.inc"
      character(len=120) :: ofile
      character(len=10)  :: charymd
      integer x_dim,y_dim,time_dim
      integer im,jm,status,ncid
      integer x_id,y_id,time_id,hs_id
      integer vardim(3),startvar(3),countvar(3)
      integer iyr,imth,iday

      real*8 x(im),y(jm),hs(im,jm)
      real*8 time

      print *,'Year,Month,Day (e.g. 1999,12,31):'
      read (5,*) iyr,imth,iday

      write (charymd,'(I4,"-",I2.2,"-",I2.2)') iyr,imth,iday

      status = nf_create(ofile,nf_write,ncid)

      status = nf_def_dim(ncid,'x', im, x_dim)
      status = nf_def_dim(ncid,'y', jm, y_dim)
      status = nf_def_dim(ncid,'time', nf_unlimited, time_dim) 
      
      status = nf_def_var(ncid,'x',nf_float,1,x_dim,x_id)
      status = nf_def_var(ncid,'y',nf_float,1,y_dim,y_id)
      status = nf_def_var(ncid,'time',nf_double,1,time_dim,time_id)

      vardim= (/x_dim,y_dim,time_dim/) 
      status = nf_def_var(ncid,'HSNO',nf_double, 3,vardim,hs_id)

      status = nf_put_att_text(ncid,x_id,'long_name',      9, 'longitude')
      status = nf_put_att_text(ncid,x_id,'units',          9, 'degrees_E')
      status = nf_put_att_text(ncid,x_id,'cartesian_axis', 1, 'X')

      status = nf_put_att_text(ncid,y_id,'long_name',      8, 'latitude')
      status = nf_put_att_text(ncid,y_id,'units',          9, 'degrees_N')
      status = nf_put_att_text(ncid,y_id,'cartesian_axis', 1, 'Y')

      status = nf_put_att_text(ncid,time_id,'long_name',    4, 'time')
      status = nf_put_att_text(ncid,time_id,'units',        10, charymd)
      status = nf_put_att_text(ncid,time_id,'cartesian_axis',1, 'T')
      status = nf_put_att_text(ncid,time_id,'calendar_type', 6,   'JULIAN')
      status = nf_put_att_text(ncid,time_id,'calendar',      6,   'JULIAN')
      status = nf_put_att_text(ncid,time_id,'bounds',       11,   'time_bounds')

      status = nf_put_att_text(ncid,hs_id,'long_name',	  10, 'snow depth')
      status = nf_put_att_text(ncid,hs_id,'units',	   8, 'mm')
      status = nf_put_att_real(ncid,hs_id,'missing_value', nf_real, 1, -1.e+34)

      status = nf_enddef(ncid)
      
      status = nf_put_vara_double(ncid,x_id,1,im,x)
      status = nf_put_vara_double(ncid,y_id,1,jm,y)
      status = nf_put_vara_double(ncid,time_id,1,1,time)
      
      startvar = (/1,1,1/)
      countvar = (/im,jm,1/)  
      status = nf_put_vara_double(ncid,hs_id,startvar,countvar,hs)
      status=nf_close(ncid)

      return
      end subroutine hsno2nc

      subroutine FillLand(data,im,jm,xmiss)
      integer i,j,im,jm
      real xmiss
      real, dimension(im,jm) :: data
      real, dimension(im+2,jm) :: comb,res,ptd
      integer, dimension(im+2,jm) :: land
      
      do j=1,jm
      do i=1,im
       land(i+1,j)=1
       comb(i+1,j)=data(i,j)
       if(comb(i+1,j) >= xmiss) then
        land(i+1,j)=0
	comb(i+1,j)=0.0
       endif
      enddo
      enddo 
      
      do j=1,jm
       comb(1,j)=data(im+2,j)
       land(1,j)=1
       if(comb(1,j) >= xmiss) then
        land(1,j)=0
	comb(1,j)=0.0
       endif
       comb(im+2,j)=data(1,j)
       land(im+2,j)=1
       if(comb(im+2,j) >= xmiss) then
        land(im+2,j)=0
	comb(im+2,j)=0.0
       endif
      enddo
      call extrap(comb,land,res,ptd,im+2,jm,3000,1.e-9)
      do j=1,jm
      do i=1,im
       data(i,j)=comb(i+1,j)
      enddo
      enddo

      return
      end subroutine FillLand
      
      subroutine extrap(a,land,res,ptd,il,jl,maxscn,crit)
      real, PARAMETER :: c0=0.,p25=0.25
      integer il,jl,maxscn
      integer land(il,jl)
      real, dimension(il,jl) :: a,res,ptd
      real :: crit,relc,resmax
      data relc /0.6/
      save relc
      integer :: i,j,n
      logical done

      n = 0

      do j=1,jl
      do i=1,il
         if (land(i,j) == 0) then
            ptd(i,j) = relc
         else
            ptd(i,j) = c0
         endif
      enddo
      enddo

100   continue
      resmax = c0
      done   = .true.
      n    = n + 1
      do j=2,jl-1
      do i=2,il-1
         res(i,j) = p25*(a(i-1,j)+a(i+1,j)+a(i,j-1)+a(i,j+1))-a(i,j)
      enddo
      enddo
      do j=2,jl-1
      do i=2,il-1
          res(i,j) = res(i,j)*ptd(i,j)
          a(i,j) = a(i,j) + res(i,j)
          if (abs(res(i,j)) .gt. crit) done = .false.
          resmax = MAX(abs(res(i,j)),resmax)
      enddo
      enddo

      do j=1,jl
      do i=1,il
         a(1,j)  = a(il-1,j)
         a(il,j) = a(2,j)
      enddo
      enddo

      if (.not. done .and. n .le. maxscn) go to 100

      return
      end subroutine extrap

      SUBROUTINE GETAREA(KGDS,DLAT,DLON,RSLAT,RNLAT,WLON,ELON,IJORDR,me)
      implicit none
      integer j,me,kgds11
      real f0lon,f0lat,elon,dlon,dlat,rslat,wlon,rnlat
!
!  Get area of the grib record
!
      Integer KGDS(200)
      LOGICAL IJORDR
!
      if (me .eq. 0) then
       WRITE(6,*) ' KGDS( 1-12)=',(KGDS(J),J= 1,12)
       WRITE(6,*) ' KGDS(13-22)=',(KGDS(J),J=13,22)
      endif
!
      IF(KGDS(1).EQ.0) THEN                      !  Lat/Lon grid
!
        if (me .eq. 0) WRITE(6,*) 'LAT/LON GRID'
        DLAT   = FLOAT(KGDS(10)) * 0.001
        DLON   = FLOAT(KGDS( 9)) * 0.001
        F0LON  = FLOAT(KGDS(5))  * 0.001
        F0LAT  = FLOAT(KGDS(4))  * 0.001
        IF (KGDS(7) > KGDS(4))  F0LAT  = FLOAT(KGDS(7))  * 0.001
        KGDS11 = KGDS(11)
        IF(KGDS11.GE.128) THEN
          WLON = F0LON - DLON*(KGDS(2)-1)
          ELON = F0LON
          IF(DLON*KGDS(2).GT.359.99) THEN
            WLON =F0LON - DLON*KGDS(2)
          ENDIF
          DLON   = -DLON
          KGDS11 = KGDS11 - 128
        ELSE
          WLON = F0LON
          ELON = F0LON + DLON*(KGDS(2)-1)
          IF(DLON*KGDS(2).GT.359.99) THEN
            ELON = F0LON + DLON*KGDS(2)
          ENDIF
        ENDIF
        IF(KGDS11.GE.64) THEN
          RNLAT  = F0LAT + DLAT*(KGDS(3)-1)
          RSLAT  = F0LAT
          KGDS11 = KGDS11 - 64
        ELSE
          RNLAT = F0LAT
          RSLAT = F0LAT - DLAT*(KGDS(3)-1)
          DLAT  = -DLAT
        ENDIF
        IF(KGDS11.GE.32) THEN
          IJORDR = .FALSE.
        ELSE
          IJORDR = .TRUE.
        ENDIF

        IF(WLON.GT.180.) WLON = WLON - 360.
        IF(ELON.GT.180.) ELON = ELON - 360.
        WLON  = NINT(WLON*1000.)  * 0.001
        ELON  = NINT(ELON*1000.)  * 0.001
        RSLAT = NINT(RSLAT*1000.) * 0.001
        RNLAT = NINT(RNLAT*1000.) * 0.001
        RETURN
!
      ELSEIF(KGDS(1).EQ.1) THEN                  !  Mercator projection
        WRITE(6,*) 'Mercator GRID'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
!
      ELSEIF(KGDS(1).EQ.2) THEN                  !  Gnomonic projection
        WRITE(6,*) 'Gnomonic GRID'
        WRITE(6,*) 'ERROR!! Gnomonic projection not coded'
        CALL ABORT
!
      ELSEIF(KGDS(1).EQ.3) THEN                  !  Lambert conformal
        WRITE(6,*) 'Lambert conformal'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
      ELSEIF(KGDS(1).EQ.4) THEN                  !  Gaussian grid
!
        if (me .eq. 0) WRITE(6,*) 'Gaussian GRID'
        DLAT   = 99.
        DLON   = FLOAT(KGDS( 9)) / 1000.0
        F0LON  = FLOAT(KGDS(5))  / 1000.0
        F0LAT  = 99.
        KGDS11 = KGDS(11)
        IF(KGDS11.GE.128) THEN
          WLON = F0LON
          ELON = F0LON
          IF(DLON*KGDS(2).GT.359.99) THEN
            WLON = F0LON - DLON*KGDS(2)
          ENDIF
          DLON   = -DLON
          KGDS11 = KGDS11-128
        ELSE
          WLON = F0LON
          ELON = F0LON + DLON*(KGDS(2)-1)
          IF(DLON*KGDS(2).GT.359.99) THEN
            ELON = F0LON + DLON*KGDS(2)
          ENDIF
        ENDIF
        IF(KGDS11.GE.64) THEN
          RNLAT  = 99.
          RSLAT  = 99.
          KGDS11 = KGDS11 - 64
        ELSE
          RNLAT = 99.
          RSLAT = 99.
          DLAT  = -99.
        ENDIF
        IF(KGDS11.GE.32) THEN
          IJORDR = .FALSE.
        ELSE
          IJORDR = .TRUE.
        ENDIF
        RETURN
!
      ELSEIF(KGDS(1).EQ.5) THEN                  !  Polar Strereographic
        WRITE(6,*) 'Polar Stereographic GRID'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
        RETURN
!
      ELSEIF(KGDS(1).EQ.13) THEN                 !  Oblique Lambert conformal
        WRITE(6,*) 'Oblique Lambert conformal GRID'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
!
      ELSEIF(KGDS(1).EQ.50) THEN                 !  Spherical Coefficient
        WRITE(6,*) 'Spherical Coefficient'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
        RETURN
!
      ELSEIF(KGDS(1).EQ.90) THEN                 !  Space view perspective
!                                                  (orthographic grid)
        WRITE(6,*) 'Space view perspective GRID'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
        RETURN
!
      ELSE                                       !  Unknown projection.  Abort.
        WRITE(6,*) 'ERROR!! Unknown map projection'
        WRITE(6,*) 'KGDS(1)=',KGDS(1)
        PRINT *,'ERROR!! Unknown map projection'
        PRINT *,'KGDS(1)=',KGDS(1)
        CALL ABORT
      ENDIF
!
      RETURN
      end subroutine getarea

      end module fice_intp_mode
