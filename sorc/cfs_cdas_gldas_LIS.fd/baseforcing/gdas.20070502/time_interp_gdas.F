!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!
!	MODIFIED FOR GDAS T382 FORCING
!	JESSE 20051031
!
#include <misc.h>
!BOP
! !ROUTINE: time_interp_gdas
!
! !DESCRIPTION:
!  Opens, reads, and interpolates GDAS forcing.  
!
!    TIME1 = most recent past data\\
!    TIME2 = nearest future data \\
!
!  The strategy for missing data is to go backwards up to 10 days to get
!  forcing at the same time of day.
!
! \subsection{Core Functions of time$_-$interp$_-$gdas}
!  \begin{description}
!  \item[zterp]  
!   Performs zenith angle-based temporal interpolation
!  \end{description}
!
! !REVISION HISTORY:
!  1  Oct 1999: Jared Entin; Initial code
!  25 Oct 1999: Jared Entin; Significant F90 Revision
!  11 Apr 2000: Brian Cosgrove; Fixed name construction error 
!               in Subroutine ETA6HRFILE 
!  27 Apr 2000: Brian Cosgrove; Added correction for use of old shortwave
!               data with opposite sign convention from recent shortwave data.
!               Added capability to use time averaged shortwave & longwave data
!               Altered times which are passed into ZTERP--used to be GMT1 
!               and GMT2, now they are LDAS%ETATIME1 and LDAS%ETATIME2
!  30 Nov 2000: Jon Radakovich; Initial code based on geteta.f
!  17 Apr 2001: Jon Gottschalck; A few changes to allow model init.  
!  13 Aug 2001: Urszula Jambor; Introduced missing data replacement.     
!   5 Nov 2001: Urszula Jambor; Reset tiny negative SW values to zero. 
! !INTERFACE:
subroutine time_interp_gdas()
! !USES:
  use lisdrv_module, only :lis, grid  
  use baseforcing_module, only: glbdata1, glbdata2
  use time_manager
!  use time_module, only : tick, time2date
  use grid_spmdMod
  use spmdMod
  use gdasdomain_module, only : gdasdrv
!EOP
  implicit none
!==== Local Variables=======================
  integer :: ier
  integer :: c,r,f,zdoy,idoy,iyr,imo,ida,ihr,imn,its,iss
  integer :: bdoy,byr,bmo
  integer :: bda,bhr,bmn
  real*8 :: btime,inittime
  real :: wt1,wt2,czb,cze,czm,gmt1,gmt2
  real :: zw1,zw2
  integer :: igmt1, igmt2, fcst1, fcst2
  integer :: nstep
  integer, parameter :: c1=0
!BOC
  if(masterproc) then 
!3.1
     nstep = get_nstep(lis%t)
     if (nstep .eq. 0) then
!     if ( get_nstep().eq.0 ) then
        lis%f%nforce = gdasdrv%nmif
     else
        lis%f%nforce = lis%f%nf
     endif
  endif
#if(defined SPMD)
  call MPI_BCAST(gdasdrv%gdastime1,1,MPI_REAL8,0, & 
       MPI_COMM_WORLD,ier)
  call MPI_BCAST(gdasdrv%gdastime2,1,MPI_REAL8,0, & 
       MPI_COMM_WORLD,ier)
  call MPI_BCAST(lis%t%time,1,MPI_REAL8,0, & 
       MPI_COMM_WORLD,ier)
  call MPI_BCAST(lis%t%gmt,1,MPI_REAL,0, & 
       MPI_COMM_WORLD,ier)
  call MPI_BCAST(lis%f%nforce,1,MPI_INTEGER,0, & 
       MPI_COMM_WORLD,ier)
  call MPI_BCAST(lis%f%F00_flag,1,MPI_INTEGER,0, & 
       MPI_COMM_WORLD,ier)
  call MPI_BCAST(lis%f%F06_flag,1,MPI_INTEGER,0, & 
       MPI_COMM_WORLD,ier)
#endif
  btime=gdasdrv%gdastime1
  call time2date(btime,bdoy,gmt1,byr,bmo,bda,bhr,bmn)
  btime=gdasdrv%gdastime2
  call time2date(btime,bdoy,gmt2,byr,bmo,bda,bhr,bmn)

  wt1 = (gdasdrv%gdastime2-lis%t%time) / & 
       (gdasdrv%gdastime2-gdasdrv%gdastime1)
  wt2 = 1.0 - wt1
  zw1 = 0.
  zw2 = 0.
  igmt1 = gmt1
  igmt2 = gmt2
  fcst1 = modulo(igmt1,6)
  fcst2 = modulo(igmt2,6)
  if ( fcst2 .EQ. 0 ) fcst2 = 6

  WRITE(*,'(1X,A,(A,I3))') "J---TIME_INTERP_GDAS",&
       " NFORCE=", lis%f%nforce
  WRITE(*,'(1X,A,3(A,F8.3))') "J---TIME_INTERP_GDAS",&
       " BTIME=",GMT1," ETIME=",GMT2," MTIME=",lis%t%gmt

  do f=1,lis%f%nforce
     if ( f == 3 ) then						 	!SWDN 
        do c = 1, gdi(iam)
           zdoy = lis%t%doy
!           print*, 'f0,f6',lis%f%F00_flag,lis%f%F06_flag
!           call zterp( 0, grid(c)%lat, grid(c)%lon, gmt1, gmt2, &	!AVE 
!           call zterp( 1, grid(c)%lat, grid(c)%lon, gmt1, gmt2, &	!INSTANTANEOUS
!                lis%t%gmt,zdoy,zw1,zw2,czb,cze,czm,lis)
           
           if( c == c1 ) &
              WRITE(88,'(1X,A,8(A,F8.3))') "J---TIME_INTERP_GDAS",&
                   " GMT1 =",gmt1," GMT2 =",gmt2," GMT =",lis%t%gmt,&
                   " CZB  =",czb, " CZE  =",cze, " CZM =",czm,&
                   " ZW1  =",zw1, " ZW2  =",zw2
           
           ! grid(c)%forcing(f) = zw1 * glbdata2(f,c)			!AVE
           ! grid(c)%forcing(f) = zw1*glbdata1(f,c) + zw2*glbdata2(f,c)	!INSTANTANEOUS
           grid(c)%forcing(f) = wt1*glbdata1(f,c) + wt2*glbdata2(f,c)	!INSTANTANEOUS
 
           if (grid(c)%forcing(f) < 0) then
              print *, '2 warning!!!  SW radiation is negative!!'
              print *, 'sw=', grid(c)%forcing(f), '... negative'
              print *, 'gdas2=', glbdata2(f,c)
              call endrun
           end if
           
           if (grid(c)%forcing(f).gt.1367) then
              grid(c)%forcing(f)=glbdata2(f,c)
           endif
        end do

     else if ( f==4 )  then 						!LWDN 
        do c = 1, gdi(iam)
           ! grid(c)%forcing(f) = glbdata2(f,c)            		!AVE
           grid(c)%forcing(f) = wt1*glbdata1(f,c) + wt2*glbdata2(f,c)	!INSTANTANEOUS
        end do
        
!     else if ( (f==8) .or. (f==9 ) ) then 				!PRATE
! forcing(9) is now gfrac
     else if ( (f==8) ) then 		   				!PRATE
        do c = 1, gdi(iam)
           ! grid(c)%forcing(f) = glbdata2(f,c)                 	!AVE
           grid(c)%forcing(f) = glbdata2(f,c)*fcst2 - glbdata1(f,c)*fcst1 !ACCUMULATION
           if ( grid(c)%forcing(f) .LT. 0.0 ) grid(c)%forcing(f) = 0.0
        end do
 
     else if ( f==15 ) then                                             !SNEQV
        do c = 1, gdi(iam)
           grid(c)%forcing(f) = wt1*glbdata1(f,c) + wt2*glbdata2(f,c)   !INSTANTANEOUS
           grid(c)%forcing(f) = grid(c)%forcing(f) * 0.001              ![mm] -> [m]
           if ( grid(c)%forcing(f) .LT. 0.0 ) grid(c)%forcing(f) = 0.0
        end do

     else
        do c = 1, gdi(iam)
           grid(c)%forcing(f) = wt1*glbdata1(f,c) + wt2*glbdata2(f,c)   !INSTANTANEOUS
        end do

     end if

!<<<J ON SCREEN OUTPUT, JESSE 20070504

       IF ( c==c1 ) then
        IF ( F==1 ) then
 !       WRITE(*,'(1X,A,A,I8,2(A,F8.3))') "J---TIME_INTERP_GDAS",&
 !            " GDI  =",gdi(iam),&
 !            " LAT1 =",grid(1)%lat,  " LAT2 =",grid(gdi(iam))%lat
 !       WRITE(*,'(1X,A,A,I8,2(A,F8.3))') "J---TIME_INTERP_GDAS",&
 !            " GDI  =",gdi(iam),&
 !            " LON1 =",grid(1)%lon,  " LON2 =",grid(gdi(iam))%lon
        WRITE(*,'(1X,A,A,I8,2(A,F8.3))') "J---TIME_INTERP_GDAS",&
             " GRID =",c1,&
             " LAT  =",grid(c1)%lat, " LON  =",grid(c1)%lon
        WRITE(*,'(1X,A,3(A,F8.3))') "J---TIME_INTERP_GDAS",&
             " T2M1 =",glbdata1(f,c1)," T2M2 =",glbdata2(f,c1),&
             " T2M  =",grid(c1)%forcing(f)
        ENDIF
        IF ( F==3 ) &
        WRITE(*,'(1X,A,3(A,F8.3))') "J---TIME_INTERP_GDAS",&
             " SWDN1=",glbdata1(f,c1)," SWDN2=",glbdata2(f,c1),&
             " SWDN =",grid(c1)%forcing(f)
        IF ( F==4 ) &
        WRITE(*,'(1X,A,3(A,F8.3))') "J---TIME_INTERP_GDAS",&
             " LWDN1=",glbdata1(f,c1)," LWDN2=",glbdata2(f,c1),&
             " LWDN =",grid(c1)%forcing(f) 
        IF ( F==8 ) then
        WRITE(*,'(1X,A,2(A,F8.3),2(A,I8))') "J---TIME_INTERP_GDAS",&
             " PRAT1=",glbdata1(f,c1)*3600.,&
             " PRAT2=",glbdata2(f,c1)*3600.,&
             " FCST1=",fcst1,&
             " FCST2=",fcst2
        WRITE(*,'(1X,A,3(A,F8.3))') "J---TIME_INTERP_GDAS",&
             " PACC1=",glbdata1(f,c1)*3600.*fcst1,&
             " PACC2=",glbdata2(f,c1)*3600.*fcst2,&
             " PRAT =",grid(c1)%forcing(f)*3600.
        ENDIF
       ENDIF
!J>>>

  end do
  return
!EOC    
end subroutine time_interp_gdas
