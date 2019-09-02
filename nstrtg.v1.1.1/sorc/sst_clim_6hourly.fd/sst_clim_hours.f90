program nstrtg
!
!$$$  main program documentation block
! Abstract:  Generate a RTG-like SST analysis file at a lat/lon grids
!           (resolution defined by namelist variables)
!           
! Notes of the procudure to generate RTG-like SST file from NSST Tf analysis :
!       1. Read namelist variables
!       2. Get lat/lon for the target grids (ny) (nx)
!       3. Read surface mask at the target grids (nx,ny)
!       4. Get 6-hourly GFS NSST foundation temperature analysis (T1534 Gaussian grids, at prsent))
!       5. Get sea ice analysis at target grids (interpolated when necessary)
!       6. Modify the mask with sea ice info
!       7. get tf_clm at the analysis time & at target resolution from monthly climatology (nx,ny)
!       8. get sal_clm at the analysis time & at target resolution from monthly climatology (nx,ny)
!       9. interpolate tf_nst (Gaussian lat/lon) to tf (lat/lon)
!      10. set tf to be SST climatology for the missing water grids
!      11. get water temperature for grids with positive sea ice fraction 
!      12. fill the land grids temperature with known water grids temperatures
!      13. Write tf as grib format w/o bitmap
!     
! Created by Xu Li, Mar., 2019

!$$$

 use set_para, only: init_setup

 implicit none

 integer, parameter :: sfcflag=0,miss_fill=0
 real,    parameter :: bmiss=-999.0, tzero=273.16,tfrozen=271.20
!
! Arrays at Gaussian grids
!
!
! Arrays at target grids lat/lon or Gaussian, (nx,ny), (nx) or (ny)
 real,    allocatable, dimension(:,:) :: tf_clm              ! SST Climatology at target grids and valid time (nx,ny)
 real,    allocatable, dimension(:)   :: xlats               ! latitudes of target grids (ny)
 real,    allocatable, dimension(:)   :: xlons               ! longitudes of target grids (nx)
 integer, allocatable, dimension(:,:) :: mask                ! mask at target grids (0 = water, 1 = land, 2 = sea ice) (nx,ny)
!
! Arrays target grids (nx*ny)
!
 real,    allocatable, dimension(:)   :: tf_clm_ij           ! SST clim at target grids (nx*ny)
 real,    allocatable, dimension(:)   :: xlats_ij            ! latitudes of target grids (nx*ny)
 real,    allocatable, dimension(:)   :: xlons_ij            ! longitudes of target grids (nx*ny)
 real,    allocatable, dimension(:)   :: slatx,wlatx         ! latitudes of target grids (nx*ny)
 integer, allocatable, dimension(:)   :: masks_ij            ! mask at target grids (0 = water or sea ice , 1 = land) (nx*ny)

!input/output data file names
 character (len=6),  parameter :: fin_sst_clm='sstclm'        ! SST climatology 
 character (len=13), parameter :: fout_sst_clim='sst_clim_hours'


 integer :: lensfc,mon1,mon2
 integer :: iy,im,id,ih,i,j,k,ij
 integer :: nxc,nyc
 integer :: nx,ny
 real    :: wei1,wei2,dsearch,x0,y0,dres
 character (len=10) :: catime
 namelist/setup/catime,lgaus,nx,ny

 call init_setup
!
!Read namelist
!
 read(5,setup)

 read(catime(1:10),'(i4,3i2)') iy,im,id,ih

 allocate( tf_clm(nx,ny),mask(nx,ny) )
 allocate( mask_ij(nx*ny),tf_clm_ij(nx*ny))
 allocate( xlats(ny),xlons(nx),xlats_ij(nx*ny),xlons_ij(nx*ny) )
 allocate( slatx(ny),wlatx(ny) )
!
! get xlats & xlons for lat/lon or Gaussian
!
  dres = 180.0/real(ny)
  if ( lgaus ) then
    call splat(4,ny,slatx,wlatx)
    xlats = 180.0/acos(-1.0)*asin(slatx(ny:1:-1))
    do i = 1, nx
       xlons(i)=real(i-1)*dres
    end do
  else

     y0 = 0.5*dres-90.0
     x0 = 0.5*dres

! Get lat_sst & lon_sst
     do j = 1, ny
        xlats(j) = y0 + real(j-1)*dres
     enddo

     do i = 1, nx
        xlons(i) = (x0 + real(i-1)*dres)
     enddo
  endif
!
! Get 1-dimentional xlats and xlons for global grids (nx*ny)
!
 ij = 0
 do j = 1, ny
    do i = 1, nx
       ij = ij + 1
       xlats_ij(ij) = xlats(j)
       xlons_ij(ij) = xlons(i)
    enddo
 enddo
!
! read target mask (nx,ny): mask = 0 for ocean; mask = 1 for land
!
! Get dimensions of sfcanl, allocate arrays and read NSST tf
!
 write(*,'(a,8I5)') 'iy,im,id,ih,ny_nst,nx_nst,ny,nx : ',iy,im,id,ih,ny,nx
!
! get tf_clm at the analysis time & at target resolution from monthly climatology (nx,ny)
!
  call get_tf_clm(mask_ij,xlats_ij,xlons_ij,ny,nx,iy,im,id,ih,dsearch,miss_fill,bmiss,tf_clm_ij)

  tf_clm(:,:) = reshape (tf_clm_ij, (/nx,ny/) )
!
! write out tf as grib1 and grib2 format
!
 maskflag=0      ! no bitmap
 call togrib(fout_tf_grb,tf,mask,maskflag,nx,ny,iy,im,id,ih)

 maskflag=1      ! with bitmap
 call togrib(fout_tf_grb_awips,tf,mask,maskflag,nx,ny,iy,im,id,ih)

 call w3tage('nsst_tf_to_rtg')

 write(*,*) 'All Done'

end program nstrtg 

