subroutine get_sal_clm(mask_ij,xlats_ij,xlons_ij,ny,nx,iy,im,id,ih,dsearch,miss_fill,bmiss,sal_clm)
!
! Abstract: get Salinity climatology at the valid time (atime) and target resolution
! (nx,ny)
!
! input & output
!
 real,    dimension(nx*ny), intent(in)  :: xlats_ij   ! latitudes of target grids (nx*ny)
 real,    dimension(nx*ny), intent(in)  :: xlons_ij   ! latitudes of target grids (nx*ny)
 integer, dimension(nx*ny), intent(in)  :: mask_ij    ! mask at target grids (0 =water, 1 = land, 2 = sea ice) (nx*ny)
 real,    dimension(nx,ny), intent(out) :: sal_clm    ! Salinity climatology valid at atime (nx,ny)
 real,    intent(in) :: dsearch,bmiss
 integer, intent(in) :: iy,im,id,ih,nx,ny,miss_fill
! local declare
 real,    allocatable, dimension(:,:)   :: sal_clm0   ! Salinity climatology at the valid time
 integer, allocatable, dimension(:,:)   :: cmask      ! surface mask of Salinity climatology (set to zero = water everywhere)
 real,    allocatable, dimension(:)     :: cxlats     ! latitudes of Salinity Climatology
 real,    allocatable, dimension(:)     :: cxlons     ! longitudes of Salinity Climatology


 real, dimension(nx*ny)  :: sal_clm_ij  ! Salinity climatology at target grids (nx*ny)
 integer :: nxc,nyc,mon1,mon2,sfcflag
 character (len=6), parameter :: fin_sal_clm='salclm' ! Salinity climatology file name
!
! get which two months used and their weights from atime
!
 call get_tim_wei(iy,im,id,ih,mon1,mon2,wei1,wei2)
!
! get the dimensions of the Salinity climatology & allocate the related arrays
!
 call get_dim_nc(fin_sal_clm,nyc,nxc)
 allocate( sal_clm0(nxc,nyc),cmask(nxc,nyc),cxlats(nyc),cxlons(nxc) )
!
! get sal_clm at the analysis time from monthly climatology & cxlats, cxlons
!
 call get_sal_clm_ta(sal_clm0,cxlats,cxlons,nyc,nxc,mon1,mon2,wei1,wei2)
!
! get mask at 1 degree based on if salinity value (valid or not)
!
 do j = 1, nyc
    do i = 1, nxc
       if ( sal_clm0(i,j) < 5.0 .or. sal_clm0(i,j) > 50.0 ) then  ! non-water
          cmask(i,j) = 1
       else                                                       ! water (ocean, lake,river)
          cmask(i,j) = 0
       endif
    enddo
 enddo
!
! get sal_clm (nx by ny lat/lon) valid at atime
!
 if ( nx == nxc .and. ny == nyc ) then
    sal_clm(:,:) = sal_clm0(:,:)
    write(*,'(a,2F9.3)') 'same dimensions, sal_clm, min : ',minval(sal_clm),maxval(sal_clm)
 else
    write(*,'(a,4I8)') 'different dimensions,nx,ny,nxc,nyc : ',nx,ny,nxc,nyc
    sfcflag=0
    call lalo_to_tile(sal_clm0,   cmask,   cxlats,  cxlons,  nyc, nxc, &
                      sal_clm_ij, mask_ij, xlats_ij,xlons_ij,ny,  nx,  &
                      sfcflag,2500.0,miss_fill,32.0)
    write(*,'(a,2F9.3)') 'done with lalo_to_tile for sal_clm, min : ',minval(sal_clm_ij),maxval(sal_clm_ij)

    sal_clm(:,:) = reshape (sal_clm_ij, (/nx,ny/) )
 endif

end subroutine get_sal_clm

subroutine read_woa05_sal_nc(filename,sal,xlats,xlons,nlat,nlon,itime)
! abstract: read woa05 salinity monthly climatology  (netcdf)
  use netcdf
  implicit none
  
  ! This is the name of the data file we will read.
  character (len=6),             intent(in)  :: filename
  integer,                       intent(in)  :: nlat,nlon
  integer,                       intent(in)  :: itime
  real,    dimension(nlat),      intent(out) :: xlats
  real,    dimension(nlon),      intent(out) :: xlons
  real,    dimension(nlon,nlat), intent(out) :: sal
! Local variables
  integer :: ncid,ntime

  integer, parameter :: ndims = 4
  character (len = *), parameter :: lat_name = "LAT"
  character (len = *), parameter :: lon_name = "LON"
  character (len = *), parameter :: z_name = "DEPTH"
  character (len = *), parameter :: t_name = "TIME"
  character (len = *), parameter :: sal_name="SALT"
  integer :: no_fill,fill_value
  integer :: time_varid,lon_varid, lat_varid, z_varid, sal_varid

  ! The start and count arrays will tell the netCDF library where to read our data.
  integer, dimension(ndims) :: start, count

  character (len = *), parameter :: units = "units"
  character (len = *), parameter :: sal_units = "psu" 
                                  ! PSU (Practical SalinitUnit). 1 PSU = 1g/kg
  character (len = *), parameter :: time_units = "day_of_year"
  character (len = *), parameter :: lat_units = "degrees_north"
  character (len = *), parameter :: lon_units = "degrees_east"
  character (len = *), parameter :: z_units = "downward"

  integer :: missv
! Loop indices
  integer :: i,j

! Open the file. 
  call nc_check( nf90_open(filename, nf90_nowrite, ncid) )

! Get the varids of time, latitude, longitude & depth coordinate variables.
  call nc_check( nf90_inq_varid(ncid, "TIME",  time_varid) )
  call nc_check( nf90_inq_varid(ncid, "LAT",   lat_varid) )
  call nc_check( nf90_inq_varid(ncid, "LON",   lon_varid) )
  call nc_check( nf90_inq_varid(ncid, "DEPTH", z_varid) )

! Read the time, latitude and longitude data.
! call nc_check( nf90_get_var(ncid, time_varid, ntime) )
  call nc_check( nf90_get_var(ncid, lat_varid,  xlats) )
  call nc_check( nf90_get_var(ncid, lon_varid,  xlons) )

! Get the varids of the sal netCDF variables.
  call nc_check( nf90_inq_varid(ncid, "SALT",sal_varid) )

! Read 1 record of nlat*nlon values, starting at the beginning 
! of the record (the (1, 1, 1, rec) element in the netCDF file).
  start = (/ 1, 1, 1, itime /)
  count = (/ nlon, nlat, 1, 1 /)

  write(*,*) 'WOA05 itime : ',itime
! Read the sal data from the file, one record at a time.
  call nc_check( nf90_get_var(ncid, sal_varid, sal, start, count) )

! Close the file. This frees up any internal netCDF resources
! associated with the file.
  call nc_check( nf90_close(ncid) )

! If we got this far, everything worked as expected. Yipee! 
  print *,"*** SUCCESS reading file ", filename, "!"

end subroutine read_woa05_sal_nc

subroutine get_dim_nc(filename,nlat,nlon)
! abstract: get dimensions of sal array
  use netcdf
  implicit none
  
  character (len=6), intent(in)  :: filename
  integer,           intent(out) :: nlat,nlon
! Local variables
  integer :: ncid
  integer :: LatDimID,LonDimID

! Open the file. 
  call nc_check( nf90_open(filename, nf90_nowrite, ncid) )

! Get dimensions 
  call nc_check( nf90_inq_dimid(ncid,"LAT",LatDimID) )
  call nc_check( nf90_inq_dimid(ncid,"LON",LonDimID) )
  call nc_check( nf90_inquire_dimension(ncid,LatDimID,len=nlat) )
  call nc_check( nf90_inquire_dimension(ncid,LonDimID,len=nlon) )

  write(*,'(a,1x,a6,2I8)') 'get_dim_nc, file, nlat, nlon : ',filename,nlat,nlon

! Close the file. This frees up any internal netCDF resources
! associated with the file.
  call nc_check( nf90_close(ncid) )

! If we got this far, everything worked as expected. Yipee! 
  print *,"*** SUCCESS get dimensions from nc file ", filename, "!"

end subroutine get_dim_nc

subroutine get_sal_clm_ta(sal_clm_ta,xlats,xlons,nlat,nlon,mon1,mon2,wei1,wei2)
!$$$
! Abstract:  get Tf/SST climatology at analysis time
! Created by Xu Li, March, 2019

 implicit none

! Input
 integer, intent(in) :: nlat,nlon,mon1,mon2
 real,    intent(in) :: wei1,wei2
! Output
 real, dimension(nlon,nlat), intent(out)   :: sal_clm_ta
 real, dimension(nlon),      intent(inout) :: xlons
 real, dimension(nlat),      intent(inout) :: xlats

!input/output data file names
 character (len=6),  parameter :: fin_sal_clm='salclm'

! Local declare
 real, dimension(nlon,nlat) :: sal_clm1,sal_clm2
 integer :: i,j

!
! read in RTG SST climatology without bitmap (surface mask) for mon1 and mon2
!
  call read_woa05_sal_nc(trim(fin_sal_clm),sal_clm1,xlats,xlons,nlat,nlon,mon1)
  call read_woa05_sal_nc(trim(fin_sal_clm),sal_clm2,xlats,xlons,nlat,nlon,mon2)
!
!  sal_clim at the analysis time
!
   do j = 1, nlat
      do i = 1, nlon
         if ( abs(sal_clm1(i,j)) < 50.0 .and. abs(sal_clm2(i,j)) < 50.0 ) then
            sal_clm_ta(i,j) = wei1*sal_clm1(i,j)+wei2*sal_clm2(i,j)
         elseif ( abs(sal_clm1(i,j)) < 50.0 .and. abs(sal_clm2(i,j)) > 50.0 ) then
            sal_clm_ta(i,j) = sal_clm1(i,j)
         elseif ( abs(sal_clm1(i,j)) > 50.0 .and. abs(sal_clm2(i,j)) < 50.0 ) then
            sal_clm_ta(i,j) = sal_clm2(i,j)
         endif
      enddo
   enddo

 end subroutine get_sal_clm_ta

