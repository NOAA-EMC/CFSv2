!----------------------------------------------------------------------
!----------------------------------------------------------------------
      SUBROUTINE GBLEVN10netc(IUNITF,IDATEP,IM,JM,kbak) 
                                                   
      USE GBLEVN_MODULE
      use sigio_module
      use netcdf

!     IMPLICIT NONE

      INTEGER IUNITF, IDATEP, IM, JM, kbak
      REAL, PARAMETER :: PI180 = .0174532
      REAL, PARAMETER :: ROG   = 29.261
      REAL, PARAMETER :: tzero = 273.15
      REAL :: ZBLO,TMID,PWGT
      REAL :: p,q,t,virt,esph

      INTEGER*4 IDRTNEMS
      INTEGER*4 IDVC,IDSL,SFCPRESS_ID,THERMODYN_ID
      INTEGER*4 NRET,NRET1,NRETS,IMJM,KM4,IDVM,NTRAC
      INTEGER*4 JCAP,I,J,K,KK,L,IRET,imx,jmx,ii,nvcoord
      INTEGER*4 NFDAY,NFHOUR,NFMINUTE,NFSECONDN,NFSECONDD,IDATGS_COR
      integer nk,nlev

      CHARACTER*6  COORD(3)
      CHARACTER*20 CFILE

      DATA COORD /'SIGMA ','HYBRID','GENHYB'/


      INTEGER yyyy,mm,dd,hh
      INTEGER*4 ncid
      integer*4  error, id_var, dimid, len
      integer*4  km, lm,n, nargs
      REAL(KIND=8),PARAMETER:: CON_RV=4.6150E+2,CON_RD=2.8705E+2,FV=CON_RV/CON_RD-1.,ONER=1.0,QMIN=1.0E-10         
      INTEGER*4, PARAMETER :: TEN=10


      INTEGER(4) JCAP4,IDVC4,DIMZ4,K4,IM4,JM4,KINDREAL,KINDINT
      INTEGER(4) idsl4,idvm4,kr
      REAL(8) tfac, time
      REAL(4),ALLOCATABLE :: VCOORD4(:,:,:),CPI(:),ak(:),bk(:)
      REAL,ALLOCATABLE :: temp(:,:), temp3d(:,:,:)
      REAL, ALLOCATABLE   :: DPRS(:,:)
      character (len = 80) :: attone
      INTEGER nt1, nt2
      character(len=10) :: dim_nam, grid
      CHARACTER*20 gfname

      INTEGER(4) IDATE(8),JDATE(8)
      REAL(4) FHOUR,RINC(5)

!----------------------------------------------------------------------
!--INITIALLY DEVELOPED BY HANG LEI BASED ON GBLEVN10 FOR NETCDF INPUT
!----------------------------------------------------------------------

!  GET VALID-TIME DATE OF NETCDF INPUT FILE, ALSO READ HEADERS
!  -----------------------------------------------------------

       WRITE(gfname,'("fort.",I2.2)') IUNITF
       print*,'opening ',gfname

!  open the file and find the dimension of the arrays

       call check( nf90_open(gfname,NF90_NOWRITE,ncid),"opening PL file")
       call getdim(ncid,"grid_xt",im)
       call getdim(ncid,"grid_yt",jm)
       call getdim(ncid,"pfull  ",km)
       call getdim(ncid,"phalf  ",lm)

       print*
       print*, "im,jm,kmi,lm:",im,jm,km,lm
       imax = im
       jmax = jm
       kmax = km
       lmax = lm

       imjm=im*jm

!  VALID DATES MUST MATCH
!  ----------------------

      call getatt(ncid,"time","units",attone); print*,trim(attone)
      read(attone,'(12x,i4,x,i2,x,i2,x,i2)') yyyy,mm,dd,hh

      IF(attone(1:5) .NE. "hours") THEN
         print*, "checkout the time unit, not hourly",attone
      ELSE
         print*, "base time", yyyy,mm,dd,hh
         print*
      ENDIF

      error=nf90_inq_varid(ncid,"time", id_var)
      error=nf90_get_var(ncid, id_var, time)
      fhour=time

      idate = 0
      idate(1)=yyyy
      idate(2)=mm
      idate(3)=dd
      idate(5)=hh

      rinc=0.0
      rinc(2)=fhour

      CALL W3MOVDAT(RINC,IDATE,JDATE)

      PRINT 1, FHOUR,(IDATE(II),II=1,3),IDATE(5),(JDATE(II),II=1,3),JDATE(5)

 1    FORMAT(' --> GBLEVENTS: GLOBAL NEMSIO FILE: HERE IS A ',F5.1,' HOUR FORECAST FROM ',I5.4,3I3.2,' VALID AT ',I5.4,3I3.2)

      IDATGS_COR = (JDATE(1) * 1000000) + (JDATE(2) * 10000) + (JDATE(3) * 100) + JDATE(5)
      print*,IDATGS_COR,idatep

      error=nf90_get_att(ncid, NF90_GLOBAL, "grid", grid)
      IF (grid == "gaussian")THEN
        IDRTnems=4
      ENDIF

      NVCOORD=2     !for idvc=2, nvcoord=2: hybrid interface a and b
      IDVC=2        !(1 for sigma and 2 for hybrid)
      IDSL=1        !(1 for phillips or 2 for mean)
      idvm=1
      jcap = -9999

      SFCPRESS_ID  = MOD(IDVM,TEN)
      THERMODYN_ID = MOD(IDVM/TEN,TEN)

      IF(IDVC == 3 .AND. THERMODYN_ID == 3) THEN
         KMAXS = (NTRAC+1)*KMAX + 2
      ELSE
         KMAXS = 2*KMAX + 2
         NTRAC = 1
      ENDIF

!  DEFINE THE OTHER RESOLUTION PARAMETERS
!  --------------------------------------

      DLAT  = 180./(JMAX-1)
      DLON  = 360./IMAX

      PRINT 2, JCAP,IMAX,JMAX,KMAX,kmaxs,DLAT,DLON,idvc
 2    FORMAT(/' --> GBLEVENTS: GLOBAL MODEL SPECS: T',I5,' ',I5,' x ',I5,' GRID WITH ',I3,' LEVELS ',I3,' SCALARS -------> ',F5.2,' X ',F5.2,&
             //' VERT. ', $ 'COORD ID IS: ',I0)

      print *,'sfcpress_id=',sfcpress_id,' thermodyn_id=',thermodyn_id,' ntrac=',ntrac
      print *
      print *,' idvc=',idvc,' idsl=',idsl,' idvm=',idvm,' nvcoord=',nvcoord
      print *

!  read the hybrid level definition coefficients ak and bk
!  -------------------------------------------------------

      allocate(ak(km+1))
      allocate(bk(km+1))

      error=nf90_get_att(ncid, NF90_GLOBAL, "ak", ak)
      error=nf90_get_att(ncid, NF90_GLOBAL, "bk", bk)

      ak(:)=ak(:)*.001

      if(kbak==1) call prttime('read dims & atts')

!***********************************************************************
!**********  read netcdf file
!***********************************************************************

      CALL ALLOBAKs ! aloocates the nbak background arrays

      allocate (dprs(imjm,kmax))
      allocate (temp(im,jm))

      error=nf90_inq_varid(ncid, 'hgtsfc', id_var)
      error=nf90_get_var(ncid, id_var, temp)
      call gblevn11(imax,jmax,temp)
      iarzs(:,:,kbak)=temp(:,:)
      write(6,111)'hgtsfc', 1,minval(temp),maxval(temp)
      

      error=nf90_inq_varid(ncid, 'pressfc', id_var)
      error=nf90_get_var(ncid, id_var, temp)
      call gblevn11(imax,jmax,temp)
      iarps(:,:,kbak)=temp(:,:)*0.01
      write(6,111)'pressfc',1,minval(temp),maxval(temp)

      if(kbak==1) call prttime('read surface')

      deallocate(temp)

      allocate(temp3d(im,jm,km))
      error=nf90_inq_varid(ncid, 'tmp', id_var)
      error=nf90_get_var(ncid, id_var, temp3d)
         DO K4=1,km
         kr=km+1-k4
         call gblevn11(imax,jmax,temp3d(:,:,kr))          
         iartt(:,:,K4,kbak)=temp3d(:,:,kr)
         !write(6,111)'tmp',k4,minval(iartt(:,:,k4,kbak)),maxval(iartt(:,:,k4,kbak))
      ENDDO

      error=nf90_inq_varid(ncid, 'ugrd', id_var)
      error=nf90_get_var(ncid, id_var, temp3d)
      DO K4=1,km
         kr=km+1-k4
         call gblevn11(imax,jmax,temp3d(:,:,kr))
         iaruu(:,:,k4,kbak)=temp3d(:,:,kr)
         !write(6,111)'ugrd',k4,minval(iaruu(:,:,k4,kbak)),maxval(iaruu(:,:,k4,kbak))
      ENDDO

      error=nf90_inq_varid(ncid, 'vgrd', id_var)
      error=nf90_get_var(ncid, id_var, temp3d)
      DO K4=1,km
         kr=km+1-k4
         call gblevn11(imax,jmax,temp3d(:,:,kr))
         iarvv(:,:,k4,kbak)=temp3d(:,:,kr)
         !write(6,111)'vgrd',k4,minval(iarvv(:,:,k4,kbak)),maxval(iarvv(:,:,k4,kbak))
      ENDDO

      error=nf90_inq_varid(ncid, 'spfh', id_var)
      error=nf90_get_var(ncid, id_var, temp3d)
      DO K4=1,km
         kr=km+1-k4
         call gblevn11(imax,jmax,temp3d(:,:,kr))
         iarqq(:,:,k4,kbak)=max(0.0,temp3d(:,:,kr)*1.e6)
         !write(6,111)'spfh',k4,minval(iarqq(:,:,k4,kbak)),maxval(iarqq(:,:,k4,kbak))
      ENDDO

 111  format(a8,1x,i3,1x,2(g13.6,1x))

      deallocate(temp3d)

      if(kbak==1) call prttime('read upper air')

!***********************************************************************
!**********  make temperatures virtual
!***********************************************************************
!     1) ESVP(T     ) - SATURATION WATER VAPOR PRESSURE FROM T
!     2) ERLH(P,R,T ) - WATER VAPOR PRESSURE FROM P,R,T
!     3) EMIX(P,W   ) - WATER VAPOR PRESSURE FROM P,W
!     4) ESPH(P,Q   ) - WATER VAPOR PRESSURE FROM P,Q
!     5) EDEW(DP    ) - WATER VAPOR PRESSURE FROM DP
!     6) RELH(P,E,T ) - RELATIVE HUMIDITY FROM P,E,T
!     7) WMIX(P,E   ) - MIXING RATIO FROM P,E
!     8) QSPH(P,E   ) - SPECIFIC HUMIDITY FROM P,E
!     9) DPAL(E     ) - DEW POINT FROM E   (ALGEBRAIC)
!    10) VIRT(P,E,TS) - VIRTUAL  TEMPERATURE FROM P,E,TS
!    11) SENT(P,E,TV) - SENSIBLE TEMPERATURE FROM P,E,TV
!***********************************************************************

!***********************************************************************
!**********  compute mid layer pressures and heights
!***********************************************************************

! use gsm routine to derive mid layer pressures

      call hyb2pres(imjm,kmax,ak,bk,iarps(1,1,kbak),iarpl(1,1,1,kbak),dprs)
      if(kbak==1) call prttime('hyb2pres')

! compute and store height profiles the gsi way

      do k=1,kmax
      do j=1,jmax
      do i=1,imax

      ! make temps virtual
      p=iarpl(i,j,k,kbak)
      q=iarqq(i,j,k,kbak)*1.e-6
      t=iartt(i,j,k,kbak)-tzero
      iartt(i,j,k,kbak)=virt(p,esph(p,q),t)+tzero

      ! create height profiles
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

!!test
      if ((kbak==1) .and. (i==imax/2) .and. (j==jmax/2)) then
         if (k==1) then
            write(6,122) kbak,i,j,iarzs(i,j,kbak)
 122        format('kbak,i,j=',3(i6,1x),' zsfc=',g13.6)
         endif
         write(6,123) k,p,iarzl(i,j,k,kbak),t,q
 123     format('k= ',i3,' p,z,t,q= ',4(g14.6,1x))
      endif
!!test

      enddo
      enddo
      enddo

      if(kbak==1) call prttime('zprofiles,etc')

      !print*
      !print*,iarpl(1,1,:,1)
      !print*
      !print*,iarzl(1,1,:,1)
      !print*
      !call bort('end of test')


!***********************************************************************
!**********  normal exit
!***********************************************************************

      ! don't forget to deallocate !

      deallocate (ak,bk,dprs)

      RETURN
      END
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine getdim(ncid,dimname,dim)

      use netcdf

      character(*)  dimname
      integer       dim,dimid

      call check( nf90_inq_dimid(ncid,dimname,dimid)          ,"getting "//dimname//" dimid") 
      call check( nf90_inquire_dimension(ncid,dimid,len = dim),"getting "//dimname//" dim")

      end subroutine getdim
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine getatt(ncid,varname,attname,attout)

      use netcdf

      character(*)    varname,attname,attout
      !!character(80) varname
      integer         idvarR,len

      call check( nf90_inq_varid(ncid, varname, idvar)                    ,"getting "//varname//" idvar")
      call check( nf90_inquire_attribute(ncid, idvar, attname, len=len)   ,"getting "//attname//" dim")
      call check( nf90_get_att(ncid, idvar, attname, attout)              ,"getting "//attout//" dim")

      !error=nf90_inq_varid(ncid, "time", id_var)
      !error=nf90_inquire_attribute(ncid, id_var, "units", len=len)
      !error=nf90_get_att(ncid, id_var, "units", attone)


      end subroutine getatt
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine check(status, loc)

         use netcdf

         integer, intent(in) :: status
         character(len=*), intent(in) :: loc

         if(status /= NF90_NOERR) then
            write (*,*) "Error at ", loc
            write (*,*) NF90_STRERROR(status)
         end if

      end subroutine check
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

