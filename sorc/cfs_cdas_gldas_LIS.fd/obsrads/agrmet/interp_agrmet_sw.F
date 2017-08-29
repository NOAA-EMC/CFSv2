!-------------------------------------------------------------------------
! NASA Goddard Space Flight Center Land Information System (LIS) V3.0
! Released May 2004
!
! See SOFTWARE DISTRIBUTION POLICY for software distribution policies
!
! The LIS source code and documentation are in the public domain,
! available without fee for educational, research, non-commercial and
! commercial purposes.  Users may distribute the binary or source
! code to third parties provided this statement appears on all copies and
! that no charge is made for such copies.
!
! NASA GSFC MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE
! SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED AS IS WITHOUT EXPRESS OR
! IMPLIED WARRANTY.  NEITHER NASA GSFC NOR THE US GOVERNMENT SHALL BE
! LIABLE FOR ANY DAMAGES SUFFERED BY THE USER OF THIS SOFTWARE.
!
! See COPYRIGHT.TXT for copyright details.
!
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: interp_agrmet_sw.F90
!  
! !DESCRIPTION:
!  Opens, reads, and interpolates AGRMET shortwave radiation forcing 
!
! !REVISION HISTORY:
!  26 Jun 2001: Urszula Jambor; Initial code, based on Jesse Meng's 
!               RTNEPH2LATLON.F code.
!  08 Feb 2002: Urszula Jambor; Modified declarations of arrays 
!               dependant on domain & resolution to allocatable.
!               Pass in values for latmax.
!  11 Dec 2002: Urszula Jambor; Added 1/2 & 1 degree resolution GDS arrays
! 
! !INTERFACE:
subroutine interp_agrmet_sw( nameSH, outdata, ferror )
! !USES:
  use lisdrv_module,only : lis,gindex 
  use agrmetdomain_module, only : rlat,rlon,w11,w12,w21,w22,n11,n12,n21,& 
       n22, mi, mo
  use lis_openfileMod
  use lis_indices_module
  implicit none
! !ARGUMENTS:
  character*80 :: nameSH  
  real :: outdata(lis%d%ngrid)  
  integer :: ferror              
!EOP
  integer, parameter :: nagrc = 1440, nagrr=600
  integer :: openerrN=0, openerrS=0 !set to non-zero if error found
  integer :: readerrN=0, readerrS=0 !set to non-zero if error found

  integer :: ip, i,j,count
  integer :: ipopt(20)
  integer :: gridDesci(22)
  integer :: gridDesco(50)
  integer, parameter :: km=1
  integer :: ibi(km)
  logical*1, allocatable :: li1(:)   
  integer :: no                  !ipolates returns no=latmax*lonmax
  integer :: iret
  real, allocatable :: pdata1(:,:)
  real, allocatable :: pdata(:)
  real, allocatable :: ldata(:)
  integer :: ibo
  logical*1, allocatable :: lo1(:)
  integer :: kgdsi(200), kgdso(200)
  integer misave
  logical :: file_exists
  integer :: c1 = 0
!BOC
  print*,"J---INTERP_AGRMET_SW"
  allocate(pdata1(1440,600))
  openerrS = 0
  readerrS = 0
  misave = mi 
  mi = mi + 120*1440
  print*, "mi = ", mi
  mo = lis_nc_working*lis_nr_working
  print*, "mo = ", mo
  allocate(pdata(mi))
  allocate(li1(mi))
  allocate(ldata(lis_nc_working*lis_nr_working))
  allocate(lo1(lis_nc_working*lis_nr_working))

  pdata = -9999.
  ldata = -9999.

  print*, 'Reading AGRMET file : ',nameSH
  inquire (file=nameSH, exist=file_exists) 
  if ( file_exists ) then 
     call lis_open_file(11, file=nameSH, form='unformatted',script='getagrmet_sw.pl')
     read(11, iostat=readerrS) pdata1
     close(11)
  else
     openerrS = 1
  endif

  if ((openerrS+readerrS) > 0) then
     ferror = 0
     print*, 'AGRMET file problem: ', nameSH, openerrS, readerrS
     do i=1,lis%d%ngrid
        outdata(i) = lis%d%udef
     end do
  else
     ferror = 1
     ibi = 1
     count = 120 *1440
     li1 = .false.
     do j=1,nagrr
        do i=1,nagrc
           if( pdata1(i,j) .GE. 0. ) pdata(count+i) = pdata1(i,j)
        enddo
        count = count+nagrc
     enddo

     do i=1,mi
        if(pdata(i).lt.0.0) then
           li1(i) = .false.
        else
           li1(i) = .true.
        endif
     enddo

     gridDesco = 0
     gridDesco = lis%d%gridDesc

!     call bilinear_interp(gridDesco,ibi,li1,pdata,ibo,lo1,ldata,mi,mo,&
!          rlat,rlon,w11,w12,w21,w22,n11,n12,n21,n22,iret)
     
!-------------------------------------------------------------------------
! JESSE 20041228 USE IPOLATES
!-------------------------------------------------------------------------

  ip = 0
  ipopt = 0

  write(*,*) "mi = ", mi
  write(*,*) "mo = ", mo

  kgdsi = 0
  kgdsi(1) = 0
  kgdsi(2) = 1440 
  kgdsi(3) = 720
  kgdsi(4) = -89875
  kgdsi(5) = -179875
  kgdsi(6) = 128
  kgdsi(7) = 89875
  kgdsi(8) = 179875
  kgdsi(9) = 250
  kgdsi(10)= 250
  kgdsi(11)= 64 
  kgdsi(20)= 255

  write(*,'(11I7)') kgdsi(1:11)

  kgdso = 0
  do i = 1, 10
     if( i.EQ.4 .OR. i.EQ.7 .OR. i.EQ.8 .OR. i.EQ.9 ) then
       kgdso(i) = int(lis%d%gridDesc(i)*1000)
     else
       kgdso(i) = int(lis%d%gridDesc(i))
     endif
  enddo
  kgdso(11) = 0 
  kgdso(20) = 255
  write(*,'(11I7)') kgdso(1:11)

  print*,"BEFORE IPOLATES, ldata(",c1,")=",ldata(c1)
  iret = 0
  call ipolates (ip,ipopt,kgdsi,kgdso,mi,mo, &
       km,ibi,li1,pdata,no,rlat,rlon,ibo,lo1,ldata,iret)
  print*,"AFTER  IPOLATES, ldata(",c1,")=",ldata(c1)
  print*,"C1 LAT/LON =", rlat(c1), rlon(c1)
!-------------------------------------------------------------------------

     if(iret .NE. 0) then
        print*, "IPOLATES ERROR!! PROGRAM STOP!!"
        call exit(iret)
     end if
     count = 0
     do j=lis_nr_working,1,-1
        do i=1,lis_nc_working
           if(gindex(i,j).ne. -1) then
              outdata(gindex(i,j)) = ldata(i+count)
           endif
        enddo
        count = count+lis_nc_working
     enddo
  endif
  deallocate(pdata)
  deallocate(li1)
  deallocate(ldata)
  deallocate(lo1)
  deallocate(pdata1)

  mi = misave
  print*,"DONE interp_agrmet_sw"
!EOC
end subroutine interp_agrmet_sw






