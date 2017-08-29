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
! !ROUTINE: glbprecip_cmap.F90
!
! !DESCRIPTION:
!  Includes reading routines for global CMAP precipitation product
!  Used instead of GDAS/GEOS precipitation forcing
!
! !REVISION HISTORY:
!  17 Jul 2001: Jon Gottschalck; Initial code
!  04 Feb 2002: Jon Gottschalck; Added necessary code to use global precip
!               observations with domain 3 (2x2.5)
!  30 Jul 2002: Jon Gottschalck; Added code to use Huffman and Persiann precip data
! !INTERFACE:
subroutine glbprecip_cmap( fname, ferror_cmap, filehr)
! !USES:
  use lisdrv_module, only : lis, gindex      
  use obsprecipforcing_module, only: obsprecip
  use cmapdomain_module, only : cmapdrv
  implicit none
! !ARGUMENTS:
  character(len=80)  :: fname(2)           ! Filename variable for datafile
  integer            :: ferror_cmap
  integer            :: filehr
!EOP

  integer            :: i,j,ios,iret,jret  ! Loop indicies and error flags

!  real, allocatable  :: precip_regrid(:,:) ! Interpolated precipitation array (N-S)
  real, allocatable  :: precip_cmap(:,:)   ! Interpolated precipitation array (S-N)

  integer            :: ngdas
  real, allocatable  :: gdasin(:)          ! GDAS 1d
  logical*1,allocatable  :: lg(:)          ! GDAS lb
  real, allocatable  :: precip_gdas(:,:)   ! GDAS 6 hour  precipitation array (S-N)
  real, allocatable  :: w(:,:)             ! Weighting mask of obs to GDAS (S-N)

  integer            :: ncmap
  integer            :: jj,lugb(2),lugi,kf,kpds(200),kgds(200),k,gridDesccmap(200),jpds(200),jgds(200)
  real               :: ism,udef
  real, allocatable  :: cmap2d(:,:)
  real, allocatable  :: cmapin(:)
  logical*1,allocatable  :: lb(:)

  integer            :: index  
  logical            :: file_exists
!=== End Variable Definition =======================
!BOC
  print*,"J---glbprecip_cmap()"

  obsprecip = 1.0
  allocate (precip_cmap(lis%d%lnc,lis%d%lnr))
  precip_cmap = -1.0    
  allocate (precip_gdas(lis%d%lnc,lis%d%lnr))
  precip_gdas = -1.0
  allocate (w(lis%d%lnc,lis%d%lnr))

!------------------------------------------------------------------------    
! Set necessary parameters for call to interp_gdas    
!------------------------------------------------------------------------    
  ism     = 0
  udef    = lis%d%udef	    
  jj      = 0

  cmapdrv%ncold=lis%d%lnc
  cmapdrv%nrold=lis%d%lnr
  ncmap = cmapdrv%ncold*cmapdrv%nrold
  allocate(cmap2d(cmapdrv%ncold, cmapdrv%nrold))
  allocate(cmapin(ncmap))
  allocate(lb(ncmap)) 

  ngdas = lis%d%lnc*lis%d%lnr
  allocate (gdasin(ngdas))
  allocate (lg(ngdas))

  lugb(1) = 31
  iret = 0
  inquire (file=fname(1), exist=file_exists)
  if ( file_exists ) then
     call baopenr (lugb(1),fname(1),iret)
  else
     iret = 1
  endif
  print*, fname(1), "iret =", iret

  lugb(2) = 32
  jret = 0
  inquire (file=fname(2), exist=file_exists)
  if ( file_exists ) then
     call baopenr (lugb(2),fname(2),jret)
  else
     jret = 1
  endif
  print*, fname(2), "jret =", jret

  if (iret+jret == 0 ) then

! READ AND INTERP CMAP

  print*, "reading precip_cmap ", fname(1)

  lugi    = 0
  jj      = 0
  jpds    = -1
  jpds(5) = 59
  jpds(6) = 1
  jpds(7) = 0
  jgds    = 0
  iret    = 0

     call getgb (lugb(1),lugi,ncmap,jj,jpds,jgds,kf,k,kpds,&
          kgds,lb,cmapin,iret)
     iret = 0
     call baclose(lugb(1),iret)

   !  call interp_cmap(kpds,ncmap,cmapin,lb,lis%d%gridDesc, &
   !       lis%d%lnc,lis%d%lnr,precip_cmap)

     k = 1
     do j = lis%d%lnr, 1, -1
        do i = 1, lis%d%lnc
           precip_cmap(i,j) = cmapin(k) !map to 90S-90N
           k = k + 1
        end do
     end do

! READ GDAS PRECIP

  print*,"reading precip_gdas ", fname(2)

  lugi    = 0
  jj      = 0
  jpds    = -1
  jpds(5) = 59
  jpds(6) = 1
  jpds(7) = 0
  jgds    = 0
  iret    = 0

     call getgb (lugb(2),lugi,ngdas,jj,jpds,jgds,kf,k,kpds,&
          kgds,lg,gdasin,iret)
     iret = 0
     call baclose(lugb(2),iret)

     k = 1
     do j = lis%d%lnr, 1, -1
        do i = 1, lis%d%lnc
           precip_gdas(i,j) = gdasin(k) !map to 90S-90N
           k = k + 1
        end do
     end do

! READ WEIGHTING MASK

  open(40,file='./FIX/weight_gage_gdas.bin', &
          form='unformatted',access='direct',&
          recl=lis%d%lnc*lis%d%lnr*4,status='unknown')
  read(40,rec=1) w
  close(40)
!  open(50,file='w.bin', &
!          form='unformatted',access='direct',&
!          recl=lis%d%lnc*lis%d%lnr*4,status='unknown')
!  write(50,rec=1) w
!  close(50)


     do j = 1,lis%d%lnr
        do i = 1,lis%d%lnc
           if (precip_cmap(i,j) .ne. -1.0) then
              index = gindex(i,j)
              if(index .ne. -1) then 
                 if( precip_gdas(i,j) .GE. 1.E-6 ) then
                   precip_cmap(i,j) = w(i,j)*precip_cmap(i,j) + (1.-w(i,j))*precip_gdas(i,j)
                   obsprecip(index) = precip_cmap(i,j) / precip_gdas(i,j)
                 else
                   obsprecip(index) = 1.0
                 endif
              endif
           endif
        enddo
     enddo

     ferror_cmap = 1
     print*, "Obtained CMAP CPC precipitation data "
!     print*, fname(1)
!     print*, fname(2)
  else
     ferror_cmap = 0
     print*, "Missing CMAP CPC precipitation data "
!     print*, fname(1)
!     print*, fname(2)
  endif
  deallocate(precip_cmap)
  deallocate(lb)
  deallocate(cmapin)
  deallocate(cmap2d)
  deallocate(precip_gdas)
  deallocate(lg)
  deallocate(gdasin)
  deallocate(w)
  print*,"DONE glbprecip_cmap()"
  !EOC 
end subroutine glbprecip_cmap

