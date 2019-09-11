C***********************************************************************
C***********************************************************************
      SUBROUTINE SELECTFILE(IUNITF,INPTYP)
      USE GBLEVN_MODULE
      USE SIGIO_MODULE
      USE SIGIO_R_MODULE

      IMPLICIT NONE

      INTEGER*4 IRET,IRET1,IUNIT
      TYPE(SIGIO_HEAD) :: HEAD
      CHARACTER*20 CFILE

      INTEGER IUNITF(2), INPTYP

      IUNIT = IUNITF(1)
      INPTYP = 1
         WRITE(CFILE,'("fort.",2I4.2)') IUNITF

         CALL SIGIO_RROPEN(IUNIT,CFILE,IRET)
         print *,'HLW IRET=', IRET
         CALL SIGIO_RRHEAD(IUNIT,HEAD,IRET1)
         print *,'HLW cfile=',cfile,'sig openrrhead,iret=',IRET,IRET1  !HL

       IF(IRET1.NE.0) THEN
          INPTYP = 2
       ENDIF

      RETURN

      END
C***********************************************************************
C***********************************************************************
      SUBROUTINE GBLEVN10nems(IUNITF,IDATEP,IM,JM,kbak) 
                                                    
      USE GBLEVN_MODULE
      use nemsio_module, only: nemsio_getheadvar, nemsio_gfile, 
     .                         nemsio_init, nemsio_open, 
     .                         nemsio_getfilehead,nemsio_close,
     .                         NEMSIO_READRECVw34             
      USE SIGIO_MODULE   
      USE SIGIO_R_MODULE


      IMPLICIT NONE
      INTEGER IUNITF(2), IDATEP, IM, JM, kbak
      REAL, PARAMETER :: PI180 = .0174532
      REAL, PARAMETER :: ROG   = 29.261
      REAL, PARAMETER :: tzero = 273.15
      REAL :: ZBLO,TMID,PWGT
      REAL :: p,q,t,virt,esph

      INTEGER*4 IDRTNEMS
      INTEGER*4 IDVC,IDSL,SFCPRESS_ID,THERMODYN_ID
      INTEGER*4 NRET,NRET1,NRETS,IMJM,KM4,IDVM,NTRAC
      INTEGER*4 JCAP,I,J,K,KK,L,IRET
      INTEGER*4 IDATE(7),NFDAY,NFHOUR,NFMINUTE,NFSECONDN,NFSECONDD
      integer nk,nlev

      CHARACTER*6  COORD(3)
      CHARACTER*20 CFILE

      DATA COORD /'SIGMA ','HYBRID','GENHYB'/

      REAL, ALLOCATABLE   :: CPI(:)
      REAL, ALLOCATABLE   :: DPRS(:,:),AK5(:),BK5(:)
      REAL, ALLOCATABLE   :: TMP(:)
      real ak5_in,bk5_in

      TYPE(NEMSIO_GFILE)  :: GFILE

C***********************************************************************
C***********************************************************************

      IMAX  = IM
      JMAX  = JM

C  GET VALID-TIME DATE OF SIGMA OR HYBRID FILE(S), ALSO READ HEADERS
C  -----------------------------------------------------------------
!
      CALL NEMSIO_INIT(NRET)     !HL

      WRITE(CFILE,'("fort.",I2.2)') IUNITF(1)

      print *,' cfile=',cfile

      CALL NEMSIO_OPEN(GFILE,trim(CFILE),'read',iret=nret)
      print *,' cfile=',cfile,'nemsio open,iret=',nret
      if(nret/=0) call bort('NEMSIO_OPEN')

      IDRTNEMS=IDRT

      CALL NEMSIO_GETFILEHEAD(GFILE,
     .  nfday=nfday,
     .  nfhour=nfhour,
     .  nfminute=nfminute,
     .  IDATE=idate,
     .  JCAP=JCAP,
     .  DIMX=IMAX,
     .  DIMY=jmax,
     .  DIMZ=KMAX, 
     .  IDVC=IDVC,
     .  IDSL=IDSL,
     .  IDVM=IDVM,        
     .  NTRAC=NTRAC,
     .  IDRT=IDRTNEMS,
     .  IRET=NRET)

      IF(NRET.NE.0) call bort('NEMSIO_GETFILEHEAD1')

      IMJM=IMAX*JMAX
      SFCPRESS_ID  = MOD(IDVM,10)
      THERMODYN_ID = MOD(IDVM/10,10)

      print *,idate 
      print *,nfday,nfhour,nfminute
      print *,imjm,SFCPRESS_ID,THERMODYN_ID
      print *,jcap,imax,jmax,kmax,idvc,idsl,idvm,ntrac,idrtnems,idrt

      ALLOCATE(CPI(NTRAC+1))
      CALL NEMSIO_GETHEADVAR(GFILE,'CPI',CPI,iret=NRET)
!!    IF(NRET.NE.0) call bort('cpi')
      if (nret/=0) then
         write(6,*)'***WARNING***  cpi not present'
      endif
      print*,cpi

      IF(THERMODYN_ID==3 .AND. IDVC==3) then
         call bort('gblenv10:id=3:thermodyn=3')
      endif

C  VALID DATES MUST MATCH
C  ----------------------

C     IF(IDATEP.NE.IDATGS_COR)  GO TO 901

C  EXTRACT HEADER INFO
C  -------------------

      DLAT  = 180./(JMAX-1)
      DLON  = 360./IMAX

      print*
      PRINT 2, nfhour,JCAP,KMAX,ntrac,DLAT,DLON,COORD(IDVC)
    2 FORMAT(/' --> GBLEVENTS: GLOBAL MODEL SPECS: F',i3.3,' T',I5,' ',I3,
     .' LEVELS ',I3,' SCALARS -------> ',F5.2,' X ',F5.2,' VERT. ',
     .'COORD: ',A)
      print*

!***********************************************************************
!**********  read nemsio file
!***********************************************************************

      CALL ALLOBAKs ! aloocates the nbak background arrays

      allocate (ak5(kmax+1),bk5(kmax+1))
      allocate (dprs(imjm,kmax))
      allocate (tmp(imjm))

      CALL NEMSIO_GETHEADVAR(gfile,'AK5',ak5,iret=iret)
      CALL NEMSIO_GETHEADVAR(gfile,'BK5',bk5,iret=iret)
!!      if(iret/=0) call bort('ak5 bk5')
      if (iret/=0) then
         write(6,*)'***WARNING*** unable to extract ak5,bk5 from nemsio'
         open(17,form='formatted')
         read(17,*) nk,nlev
         write(6,*)'nk=',nk,' nlev=',nlev
         do k=1,kmax+1
            read(17,*) ak5_in,bk5_in
            kk = (kmax+1)-(k-1)
            ak5(kk) = ak5_in * 0.001
            bk5(kk) = bk5_in
            write(6,*) kk,ak5(kk),bk5(kk)
         end do
         close(17)
      endif


      CALL NEMSIO_READRECVw34(GFILE,'hgt','sfc',1,tmp,iret=nret)
      CALL GBLEVN11(IMAX,JMAX,tmp)                 
      print *,'zsfc,',maxval(tmp),minval(tmp),nret
      iarzs(:,:,kbak)=reshape(tmp,(/imax,jmax/))
      if(nret/=0) call bort('reading nems')
      print*,iarzs(1,1,1),iarzs(1,jmax,1)


      CALL NEMSIO_READRECVw34(GFILE,'pres','sfc',1,tmp,iret=nret)
      CALL GBLEVN11(IMAX,JMAX,tmp)                 
      print *,'psfc,',maxval(tmp),minval(tmp),nret
      iarps(:,:,kbak)=reshape(tmp,(/imax,jmax/))*.01
      if(nret/=0) call bort('reading nems')
      print*,iarps(1,1,1),iarps(1,jmax,1)

      Do K=1,KMAX; kk=kmax-k+1

      CALL NEMSIO_readrecvw34(GFILE,'tmp','mid layer',K,tmp,
     .iret=nret)
      CALL GBLEVN11(IMAX,JMAX,tmp)                 
      if(k==1) print *,'temp,k=',k,maxval(tmp),minval(tmp),nret
      IARTT(:,:,k,kbak)=reshape(tmp,(/imax,jmax/))
      if(nret/=0) call bort('reading nems')

      CALL NEMSIO_readrecvw34(GFILE,'ugrd','mid layer',K,tmp,
     .iret=nret)
      CALL GBLEVN11(IMAX,JMAX,tmp)                 
      if(k==1) print *,'ugrd,k=',k,maxval(tmp),minval(tmp),nret
      IARUU(:,:,k,kbak)=reshape(tmp,(/imax,jmax/))
      if(nret/=0) call bort('reading nems')

      CALL NEMSIO_readrecvw34(GFILE,'vgrd','mid layer',K,tmp,
     .iret=nret)
      CALL GBLEVN11(IMAX,JMAX,tmp)                 
      if(k==1) print *,'vgrd,k=',k,maxval(tmp),minval(tmp),nret
      IARVV(:,:,k,kbak)=reshape(tmp,(/imax,jmax/))
      if(nret/=0) call bort('reading nems')

      CALL NEMSIO_readrecvw34(GFILE,'spfh','mid layer',K,tmp,
     .iret=nret)
      CALL GBLEVN11(IMAX,JMAX,tmp)                 
      if(k==1) print *,'spfh,k=',k,maxval(tmp),minval(tmp),nret
      IARQQ(:,:,k,kbak)=reshape(tmp,(/imax,jmax/))*1.e6
      if(nret/=0) call bort('reading nems')

      do i=1,imax
      do j=1,jmax
      iarqq(i,j,k,kbak)=max(iarqq(i,j,k,kbak),0.)
      enddo
      enddo

      ENDDO

      if(kbak==1) call prttime('nemsio')

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

      call hyb2pres(imjm,kmax,ak5,bk5,
     . iarps(1,1,kbak),iarpl(1,1,1,kbak),dprs)
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
 123     format('k= ',i2,' p,z,t,q= ',4(g13.6,1x))
      endif
!!test

      enddo
      enddo
      enddo

      if(kbak==1) call prttime('zprofile') 

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

      deallocate (ak5,bk5,dprs,tmp)

      CALL NEMSIO_close(gfile)  
      RETURN
      END
!***********************************************************************
!**********  subroutine to compute mid layer pressures
!***********************************************************************
      subroutine hyb2pres(imjm,levs,ak5,bk5,psmb,prsl,dprs)

      implicit none

      integer, parameter :: kind_args = 4

      real(kind=kind_args),parameter :: con_rd     =2.8705e+2      ! gas constant air    (J/kg/K)
      real(kind=kind_args),parameter :: con_cp     =1.0046e+3      ! spec heat air @p    (J/kg/K)
      real(kind=kind_args),parameter :: con_rocp   =con_rd/con_cp
      real(kind=kind_args),parameter :: rk         =con_rd/con_cp
      real(kind=kind_args),parameter :: rk1 = rk + 1.0, rkr = 1.0/rk
      real(kind=kind_args),parameter :: R1000=1000.0, R10=10., PT01=0.01

      real(kind=kind_args) tem,pressfc(imjm)
      real(kind=kind_args) ppi(imjm,levs+1),ppik(imjm,levs+1)
      real(kind=kind_args) psmb(imjm),ak5(levs+1),bk5(levs+1)
      real(kind=kind_args) prsl(imjm,levs),dprs(imjm,levs)

      integer imjm,levs
      integer i,k

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! interface pressures defined by ak and bk

      do k=1,levs+1
      do i=1,imjm
      ppi(i,levs+2-k) = ak5(k)+bk5(k)*psmb(i)*.1 ! psmb=surface pres in mb
      enddo
      enddo

! exner function for level 1

      ppik(:,1) = (ppi(:,1)*pt01) ** rk

! mid later pressures using exner function interpolation bottom to top

      do k=1,levs
      do i=1,imjm   
      ppik(i,k+1) = (ppi(i,k+1)*pt01) ** rk
      tem         = rk1 * (ppi(i,k) - ppi(i,k+1))
      ppik(i,k)   = (ppik(i,k)*ppi(i,k)-ppik(i,k+1)*ppi(i,k+1))/tem

      ! pres and dprs returned in hPa

      prsl(i,k)   = r1000 * ppik(i,k) ** rkr 
      dprs(i,k)   = r10   * (ppi(i,k) - ppi(i,k+1))

      enddo
      enddo

      !print*,prsl(1,:)
      !call bort('test')

      return
      end
