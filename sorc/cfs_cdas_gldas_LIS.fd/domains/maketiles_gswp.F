!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
#include "absoft.h"
!BOP
!
! !ROUTINE: maketiles_gswp.F90
!
! !DESCRIPTION:
!  This primary goal of this routine is to determine tile space for 
!  GSWP data sets
!
! !REVISION HISTORY:
! 23Feb04, Sujay Kumar : Intial Specification
!
! !INTERFACE:
subroutine maketiles_gswp()
! !USES:
  use lisdrv_module, only: lis, grid, glbgindex, tile
  use grid_module
  use spmdMod
!EOP
  IMPLICIT NONE
  real, allocatable :: mask(:,:)
  real, allocatable :: vegclass(:,:)
  real,allocatable :: fgrd(:,:)
  character*8 :: cslat,cnlat
  character*8 :: cwlon,celon
  character*100 :: gswp_comm
  integer :: glnc,glnr
  integer :: line1, line2
  real :: lat,lon
  real :: tmp
! for writing dominant veg types..
  real, allocatable :: domveg(:,:)
!=== Local Variables =====================================================
  INTEGER :: ppp,cc,C,R,T,I,J,count  ! Loop counters
  real :: isum
  INTEGER :: KVEG, J2, LANDNVEG
  REAL :: TPGRID
  REAL    :: RSUM          !Temporary vegetation processing variable
  REAL    :: FVT(LIS%P%NT)  !Temporary vegetation processing variable
  REAL    :: MAX           !Temporary vegetation processing variable
  INTEGER :: NCHP          !Number of tiles use for array size
  
  integer :: ierr
  integer :: gnc, gnr
  integer :: cindex, rindex  
!=== End Variable Definition =============================================
!BOC
  if ( masterproc ) then
     if(lis%d%gridDesc(42) > lis%d%lnc .or. &
          lis%d%gridDesc(43) > lis%d%lnr)  then !using a subdomain
        gnc = lis%d%gridDesc(42)
        gnr = lis%d%gridDesc(43)
     else
        gnc = lis%d%lnc
        gnr = lis%d%lnr
     endif
     lis%d%gnc = gnc
     lis%d%gnr = gnr
     
     allocate(mask(lis%d%lnc, lis%d%lnr), stat=ierr)
     call check_error(ierr,'Error allocating mask.',iam)
     
     allocate(vegclass(lis%d%lnc, lis%d%lnr), stat=ierr)
     call check_error(ierr,'Error allocating vegclass.',iam)

     allocate(fgrd(lis%d%lnc, lis%d%lnr), stat=ierr)
     call check_error(ierr,'Error allocating vegclass.',iam)
     
     tmp = lis%d%gridDesc(4)/1000.0
     write(cslat, '(f8.2)') tmp
     tmp = lis%d%gridDesc(7)/1000.0
     write(cnlat, '(f8.2)') tmp
     tmp = lis%d%gridDesc(5)/1000.0
     write(cwlon, '(f8.2)') tmp
     tmp = lis%d%gridDesc(8)/1000.0
     write(celon, '(f8.2)') tmp

     print*,'MSG: maketiles -- Reading landmask (',iam,')'
     gswp_comm = "./gswp_scripts/getmask.sh "// & 
          cslat//" "//cnlat//" "//cwlon//" "//celon
     print*, 'command : ',gswp_comm
     call system(gswp_comm)
     lis%p%mfile = './gswp_scripts/mask.bin'
     open(30,file=lis%p%mfile,form='unformatted',status='old')
     read(30) mask
     close(30)
     print*,'MSG: maketiles -- Done reading ',trim(lis%p%mfile), & 
          ' (',iam,')'

     print*,'MSG: maketiles -- Reading vegclass (',iam,')'
     call system("./gswp_scripts/getvegclass_umd.sh "// & 
          cslat//" "//cnlat//" "//cwlon//" "//celon)
     lis%p%mfile = './gswp_scripts/vegclass.bin'
     open(30,file=lis%p%mfile,form='unformatted',status='old')
     read(30) vegclass
     close(30)
     print*,'MSG: maketiles -- Done reading ',trim(lis%p%mfile), & 
          ' (',iam,')'

     print*,'MSG: maketiles -- Reading classfrac (',iam,')'
     call system("./gswp_scripts/getvegfrac.sh "// & 
          cslat//" "//cnlat//" "//cwlon//" "//celon)
     lis%p%vfile = './gswp_scripts/classfrac.bin'
     open(30,file=lis%p%vfile,form='unformatted',status='old')
     read(30) fgrd
     close(30)
     print*,'MSG: maketiles -- Done reading ',trim(lis%p%vfile), & 
          ' (',iam,')'
     
!----------------------------------------------------------------------
!  Make Tile Space
!----------------------------------------------------------------------
      lis%d%glbnch=0
      do r=1,lis%d%lnr      
         do c=1,lis%d%lnc   
            if(mask(c,r).gt.0.99.and. & 
                 mask(c,r).lt.3.01)then !we have land
               lis%d%glbnch=lis%d%glbnch+1 
            endif
         enddo
      enddo
      
      print*, 'DBG: maketiles -- glbnch',lis%d%glbnch,' (',iam,')'
      allocate(tile(lis%d%glbnch))

      lis%d%glbngrid=0
      do r=1,lis%d%lnr
         do c=1,lis%d%lnc
            if(mask(c,r).gt.0.99 .and. & 
                 mask(c,r).lt.3.01) then
               lis%d%glbngrid=lis%d%glbngrid+1
            endif
         enddo
      enddo
      count = 1
      print*, 'DBG: maketiles1 -- glbnch',lis%d%glbnch,' (',iam,')'
      allocate(grid(lis%d%glbngrid))
      allocate(glbgindex(lis%d%lnc, lis%d%lnr))
      print*, 'DBG: maketiles2 -- glbnch',lis%d%glbnch,' (',iam,')'
      line1 = (lis%d%gridDesc(4)-lis%d%gridDesc(44))/lis%d%gridDesc(9) + 1
      line2 = (lis%d%gridDesc(5)-lis%d%gridDesc(45))/lis%d%gridDesc(10) + 1
      do r=1,lis%d%lnr
         do c=1,lis%d%lnc
            glbgindex(c,r) = -1
            if(mask(c,r).gt.0.99 .and. & 
                 mask(c,r).lt.3.01) then
               glnc = line2+c-1
               glnr = line1+r-1
               lat = -59.5+(glnr-1)*1.0
               lon = -179.5 + (glnc-1)*1.0
               grid(count)%lat = lat
               grid(count)%lon = lon
               grid(count)%fgrd = fgrd(c,r)
               glbgindex(c,r) = count
               count = count+1
            endif
         enddo
      enddo
      print*, 'DBG: maketiles3 -- glbnch',lis%d%glbnch,' (',iam,')'
!--------------------------------------------------------------------
!   For writing dominant Vegetation types
!--------------------------------------------------------------------
      if(lis%o%wparam .eq.1) then 
         allocate(domveg(lis%d%lnc,lis%d%lnr))
         domveg = -9999.0
      endif
      count = 0
      do r=1,lis%d%lnr  
         do c=1,lis%d%lnc
            if(mask(c,r).gt.0.99.and. & 
                 mask(c,r).lt.3.01)then 
!               if(fgrd(c,r).gt.0.0)then
                  count = count+1
                  tile(count)%row=r    
                  tile(count)%col=c    
                  tile(count)%index = glbgindex(c,r)
                  tile(count)%vegt= vegclass(c,r)
                  if(lis%o%wparam.eq.1) then 
                     domveg(c,r) = vegclass(c,r)
                  endif
                  tile(count)%fgrd=fgrd(c,r)
!               endif
            endif
         enddo
      enddo
      if(lis%o%wparam.eq.1) then 
         open(32,file="domvegtype.bin",form='unformatted')
         write(32) domveg
         close(32)
         deallocate(domveg)
      endif
      deallocate(mask,stat=ierr)
      deallocate(vegclass,stat=ierr)
      deallocate(fgrd, stat=ierr)
      call check_error(ierr,'Error allocating glbfgrd',iam)
      call absoft_release_cache()
      
      WRITE(*,*) 'MSG: maketiles -- Actual Number of Tiles:', & 
           LIS%D%GLBNCH,' (',iam,')'
      WRITE(*,*)
      
      WRITE(*,*) 'MSG: maketiles -- Size of Grid Dimension:', & 
           lis%d%glbngrid,' (',iam,')'
      
   endif
   print*,'MSG: maketiles -- done',' (',iam,')'   
   return
!EOC
 end subroutine maketiles_gswp
 
