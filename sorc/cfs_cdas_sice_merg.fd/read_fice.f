!    2007-10-09  XINGREN WU
!    2007-1031   S. Moorthi - generalized

      program read_fice

      use fice_intp_mode

      implicit none

      integer im,jm

      integer i,j,ierr,irec,nf,jj
      integer iyr,imth,iday,ihr
      character*10 charymdh
      real (kind=8), allocatable :: fi(:,:), xlon(:), ylat(:)

      real (kind=4), allocatable :: fice(:,:)
      real fland
!
      character(len=120) :: ficegrib,ficenc
!
      integer ijdim,iret,lat1,n,ndata, me,lskip,lgrib
      integer, dimension(200) :: JPDS,JGDS,KPDS,KGDS
      real, allocatable :: data4(:,:), cosl(:), wgt(:)
      logical*1, allocatable :: lbms(:,:)
      logical  ijordr
      real dlon, dlat, RSLAT,RNLAT,WLON,ELON, RADI
!
      irec=0
      nf=11
      fland=1.4
!
      read (5,*) iyr,imth,iday,ihr
      write(charymdh,'(I4,I2.2,I2.2,I2.2)') iyr,imth,iday,ihr
      ficegrib='icegrb.gdas.'//charymdh
      call baopenr(nf,ficegrib,ierr)
!
! Sea-ice: JPDS(5)=91 JPDS(6)=102 JPDS(7)=0
!
      me      = 0
      lskip   = -1
      JPDS(:) = -1
      JPDS(5) = 91
      JPDS(6) = 102
      JPDS(7) = 0
      JGDS=-1
      N=-1
!
      call getgbh(nf,0,lskip,jpds,jgds,lgrib,ndata,lskip,kpds,kgds,iret)
      if(me .eq. 0) then
        WRITE(6,*) ' First grib record.'
        WRITE(6,*) ' KPDS( 1-10)=',(KPDS(J),J= 1,10)
        WRITE(6,*) ' KPDS(11-20)=',(KPDS(J),J=11,20)
        WRITE(6,*) ' KPDS(21-  )=',(KPDS(J),J=21,22)
      endif
!
      IF(IRET.NE.0) THEN
        WRITE(6,*) ' Error in GETGBH. IRET: ', iret
        IF (IRET == 99) WRITE(6,*) ' Field not found.'
        CALL ABORT
      ENDIF
!
      im    = kgds(2)
      jm    = kgds(3)
      lskip   = -1
      ijdim = im * jm
      allocate (data4(im,jm), lbms(im,jm))
      allocate (fi(im,jm), fice(im,jm), xlon(im), ylat(jm), cosl(jm), wgt(jm))
!
      CALL GETGB(nf,0,ijdim,N,JPDS,JGDS,NDATA,lskip,KPDS,KGDS,LBMS,data4,IRET)
!
      call GETAREA(KGDS,DLAT,DLON,RSLAT,RNLAT,WLON,ELON,IJORDR,me)
      print *,' dlat=',dlat,' rslat=',rslat,' rnlat=',rnlat
!
! latitude    -  ylat will have latitudes from S to N
!
      if (kgds(1) .eq. 4) then         ! grib file on Gaussian grid
        CALL SPLAT(kgds(1), JM, cosl, wgt)
        RADI = 180.0 / (4.*ATAN(1.))
        if (rnlat .gt. 0.0) then
          DO J=1,JM
            ylat(J) = 90. - ACOS(cosl(JM-J+1)) * RADI
          ENDDO
        else
          DO J=1,JM
            ylat(J) = -90. + ACOS(cosl(J)) * RADI
          ENDDO
        endif
      elseif (kgds(1) .eq. 0) then     ! grib file on lat/lon grid
        DLAT = (RNLAT-RSLAT) / FLOAT(JM-1)
        if (rnlat > 0.0) then
          DO J=1,JM
            ylat(J) = RSLAT + (J-1) * DLAT
          ENDDO
        else
          DO J=1,JM
            ylat(J) = RNLAT - (J-1) * DLAT
          ENDDO
        endif
      else                           ! grib file on some other grid
        call abort
      endif
! longitude
      dlon = 360.0 / im
      DO I=1,IM
        xlon(I) = WLON + (I-1)*DLON
      ENDDO
!
      print *,' xlon=',xlon
      print *,' ylat=',ylat
!
      lat1 = kgds(4)
      if(lat1 > 0) then
        do j=jm,1,-1
          jj = jm-j+1
          do i=1,im
            fice(i,j) = data4(i,jj)
          enddo
       enddo
      else
        fice = data4
      endif

      do j=1,jm
        if (abs(ylat(j)) < 30.0) then
          do i=1,im
            fice(i,j) = 0.0
          enddo
        endif
      enddo

      call FillLand(fice,im,jm,fland)

      fi=fice
      ficenc='fice.nc'

      call fice2nc(ficenc,im,jm,xlon,ylat,fi)

      stop
      end
