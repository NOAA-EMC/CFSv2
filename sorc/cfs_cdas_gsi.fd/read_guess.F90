subroutine read_guess(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_guess          read/compute various guess fields
!   prgmmr: parrish          org: np22                date: 1994-02-11
!
! abstract:  This routine performs various functions, all related in one
!            way or the other to the model guess.  Note that this routine
!            is for the global mode of the gsi.  Separate read_guess type
!            routines exist for the regional gsi.
!
!            Functions performed in this routine include the following
!              a) read atmospheric guess bias correction fields (optional)
!              b) read atmospheric guess fields (optionally update with bias correction)
!              c) read surface guess fields
!              d) compute average ozone at each level                           
!
!
! program history log:
!   1994-02-11  parrish
!   1998-04-03  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-16  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-12-15  treadon - remove variable mype from call load_geop_hgt
!   2005-01-27  treadon - replace inguesfc with rdgesfc
!   2005-02-23  wu - setup for qoption=2 and output stats of RH
!   2005-03-07  dee - support gmao model interface
!   2005-03-30  treadon - reformat code (cosmetic changes only)
!   2005-05-27  parrish - add call get_derivatives
!   2005-06-27  guo     - support of GMAO gridded fields
!   2005-07-28  guo     - added sfc component for GMAO grided fields
!   2005-09-29  kleist  - get derivatives of surface terrain for Jc term
!   2005-11-21  kleist  - expand calls to new genqsat and calctends
!   2005-11-21  derber  - modify qoption =1 to work consistently with =2
!   2005-11-29  derber - remove external iteration dependent calculations
!   2005-11-29  derber - add ozmz calculation                             
!   2005-12-09  guo     - remove GMAO derivative computation code.  Use
!                         unified NCEP compact_diff procedures.
!   2006-01-10  treadon - consolidate all read*guess calls into this routine
!   2006-02-02  treadon - load 3d pressure guess pressure and geopotential 
!                         height grids
!   2006-02-03  derber  - modify to increase reproducibility (ozmz)
!   2006-03-13  treadon - increase filename to 24 characters
!   2006-04-14  treadon - replace call read_gfsatm for bias with read_bias
!   2006-06-08  zhang,b - change "biascor>0" to "biascor>=0" for debug purpose
!   2006-07-28  derber  - include sensible temperature
!   2006-07-31  kleist  - use ges_ps instead of lnps
!   2006-09-28  treadon - add sfc_rough and load_fact10
!   2006-12-04  todling - merged NCEP & GMAO bias correction schemes
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
!   2007-05-30  h.liu   - remove ozmz
!   2008-12-06  todling - some clean; generalized update biasa
!   2009-01-28  todling - remove original GMAO interface
!   2010-03-06  parrish - add option to read ozone from gfs
!   2010-03-15  parrish - add flag regional_ozone to turn on ozone in regional analysis
!   2010-03-31  treadon - replace read_gfsatm with read_gfs
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use jfunc, only: biascor,bcoption
  use guess_grids, only:  nfldsig,ges_tv,ges_q,ges_tsen,load_prsges,load_geop_hgt
  use m_gsiBiases,only : correct_bias,nbc
  use m_gsiBiases,only : bias_q,bias_tv,bias_cwmr,bias_oz,bias_ps,&
       bias_vor,bias_div,bias_tskin,bias_u,bias_v
  use gsi_io, only: read_bias
  use gridmod, only: lat2,lon2
  use gridmod, only: nsig
  use gridmod, only: wrf_mass_regional,wrf_nmm_regional,&
       twodvar_regional,netcdf,regional,nems_nmmb_regional,use_gfs_ozone,regional_ozone

  use constants, only: izero,ione,zero,one,fv
  use ncepgfs_io, only: read_gfs

  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  character(24) filename
  integer(i_kind) i,j,k,it,iret_bias

  real(r_kind),dimension(lat2,lon2):: work

!-----------------------------------------------------------------------------------
! Certain functions are only done once --> on the first outer iteration. 
! One-time functions include
!    a) read atmospheric guess fields (optionally add bias correction)
!    b) read surface guess fields
!

!    Handle regional interfaces
     if (regional)then
        if (wrf_nmm_regional) then
           if(netcdf) then
              call read_wrf_nmm_netcdf_guess(mype)
           else
              call read_wrf_nmm_binary_guess(mype)
           end if
        else if (wrf_mass_regional) then
           if(netcdf) then
              call read_wrf_mass_netcdf_guess(mype)
           else
              call read_wrf_mass_binary_guess(mype)
           end if
        else if(twodvar_regional) then
           call read_2d_guess(mype)
        else if (nems_nmmb_regional) then
           call read_nems_nmmb_guess(mype)
        end if
     

!    Otherwise, handle global interface (ie, NCEP GFS)
     else

!       If requested, read bias correction fields
        iret_bias=izero
        if (biascor >= zero) then
           filename='biascor_in'
           call read_bias(filename,mype,nbc,work,bias_ps,bias_tskin,&
                bias_vor,bias_div,bias_u,bias_v,bias_tv,bias_q,&
                bias_cwmr,bias_oz,iret_bias)
        endif
        
!       Read atmospheric fields
#ifndef HAVE_ESMF
        call read_gfs(mype)
#endif

!    End of non-GMAO global interfaces
     endif
        
! If doing SBC, apply bias correction ...

  if(biascor>=zero .and. iret_bias==izero .and. bcoption==ione ) call correct_bias()

! Get sensible temperature (after bias correction's been applied)

  do it=1,nfldsig
      do k=1,nsig
         do j=1,lon2
            do i=1,lat2
               ges_tsen(i,j,k,it)= ges_tv(i,j,k,it)/(one+fv*max(zero,ges_q(i,j,k,it)))
            end do
         end do
      end do
  end do

! Load 3d subdomain pressure arrays from the guess fields
  call load_prsges

! Compute 3d subdomain geopotential heights from the guess fields
  call load_geop_hgt

!  If this is a regional run and ozone is desired from the gfs model, bring it in here:
  if(regional.and.use_gfs_ozone.and.regional_ozone) call read_gfs_ozone_for_regional
  
  return
end subroutine read_guess
