!#define VERBOSE
subroutine gsisub(mype,init_pass,last_pass)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gsisub                  high level driver for gridpoint 
!                                      statistical interpolation
!   prgmmr: parrish          org: np22                date: 1992-09-16
!
! abstract: This subroutine embodies the main driver for the gsi analysis
!           code.  This routine 
!           a) get analysis time and vertical coordinate information
!           b) generate grid information
!           c) initialize arrays containing information regarding use
!              of radiance, precipitation rate, and ozone observations
!           d) decomponses the domain into subdomains 
!           e) perform various initializations (subdomain variables, 
!              mpi communications, random number for pcp assimilation)
!           f) if (.not.regional) iniitialize spectral <--> grid transforms
!           g) call outer/inner loop driver
!           h) deallocate arrays
!
! program history log:
!   1991-xx-xx  parrish/derber
!   1991-12-10  parrish/derber   fixed coding error in near sfc anal
!   1992-09-14  derber           improved version of global analysis
!   1998-05-15  weiyu yang       mpp version
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-10-31  kleist, d. add capability to run hybrid or sigma
!   2003-12-22  derber, j. comments
!   2004-06-21  treadon - update documentation
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-12-29  treadon - replace wrf_nmm_regional and wrf_mass_regional logicals
!                         with regional
!   2005-02-16  xu li   - add sub destroy_sst_an
!   2005-03-07  dee     - support gmao model interface
!   2005-04-18  treadon - remove destroy_sst_an
!   2005-05-27  pondeca - bypass radinfo_ and pcpinfo_write when twodvar_regional=.t.!                         
!   2005-05-25  guo     - added interfaces to handle GMAO first guess gridded fields
!   2005-07-25  treadon - remove redundant call to gengrid_vars
!   2005-09-08  derber - modify to use input group time window and simplify data set handling
!   2005-10-18  treadon - remove obs_load and dload, move deter_subdomain before read_obs
!   2005-11-04  guo     - moved call to gengrid_vars to ncep initialization part
!   2005-11-28  derber - move read_obs to glbsoi
!   2005-12-09  guo     - minor comment change
!   2006-01-10  treadon - consolidate query guess file code in gesinfo
!   2006-04-20  kistler - moved conv_read from read_obs here to parallel other *info modules
!   2006-04-21  parrish - changes for new processing of level 2 radar wind data
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
!   2007-10-03  todling - add observer call
!   2009-01-28  todling - update observer calling procedure 
!   2009-08-19  guo     - #ifdef out destroy_gesfinfo() call for multi-pass observer.
!
!   input argument list:
!     mype - mpi task id
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  use obsmod, only: iadate,lobserver
  use observermod, only: observer_init,observer_run,observer_finalize
  use gridmod, only: twodvar_regional,regional,&
       create_grid_vars,create_mapping,init_subdomain_vars,&
       destroy_mapping,destroy_grid_vars
  use gridmod, only: wrf_mass_regional,wrf_nmm_regional,nems_nmmb_regional
  use mpimod, only: npe,mpi_comm_world,ierror
  use radinfo, only: radinfo_read
  use pcpinfo, only: pcpinfo_read,create_pcp_random,&
       destroy_pcp_random
  use convinfo, only: convinfo_read
  use ozinfo, only: ozinfo_read
  use read_l2bufr_mod, only: radar_bufr_read_all
  use oneobmod, only: oneobtest,oneobmakebufr
#ifndef HAVE_ESMF
  use guess_grids, only: destroy_gesfinfo
#endif

  use mpeu_util, only: die,tell

  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype
  logical        ,intent(in) :: init_pass
  logical        ,intent(in) :: last_pass

#ifdef VERBOSE
  call tell('gsisub','entered ..')
  call tell('gsisub','init_pass =',init_pass)
  call tell('gsisub','last_pass =',last_pass)
  call tell('gsisub','iadate(1)=',iadate(1))
  call tell('gsisub','iadate(2)=',iadate(2))
  call tell('gsisub','iadate(3)=',iadate(3))
  call tell('gsisub','iadate(4)=',iadate(4))
  call tell('gsisub','iadate(5)=',iadate(5))
#endif

#ifndef HAVE_ESMF

! Allocate grid arrays.
  call create_grid_vars


! Get date, grid, and other information from model guess files
  call gesinfo(mype)

! If single ob test, create prep.bufr file with single ob in it
  if (oneobtest) then
     if(mype==0)call oneobmakebufr
     call mpi_barrier(mpi_comm_world,ierror)
  end if

! Create analysis subdomains and initialize subdomain variables
  call create_mapping(npe)
  call deter_subdomain(mype)
  call init_subdomain_vars

#endif /* !HAVE_ESMF */

! Process any level 2 bufr format land doppler radar winds and create radar wind superob file
  if(wrf_nmm_regional.or.wrf_mass_regional.or.nems_nmmb_regional) call radar_bufr_read_all(npe,mype)

! Read info files for assimilation of various obs
  if (init_pass) then
     if (.not.twodvar_regional) then
        call radinfo_read
        call ozinfo_read(mype)
        call pcpinfo_read(mype)
     endif
     call convinfo_read(mype)
#ifdef VERBOSE
     call tell('gsisub','returned from convinfo_read()')
#endif
  endif

#ifndef HAVE_ESMF
! Set communicators between subdomain and global/horizontal slabs
  call init_commvars(mype)
#endif /* HAVE_ESMF */

! Compute random number for precipitation forward model.  
  if(init_pass) then
     call create_pcp_random(iadate,mype)
#ifdef VERBOSE
     call tell('gsisub','returned from create_pcp_random()')
#endif
  endif

! Complete setup and execute external and internal minimization loops
#ifdef VERBOSE
  call tell('gsisub','lobserver=',lobserver)
#endif
  if (lobserver) then
    if(init_pass) call observer_init()
#ifdef VERBOSE
    call tell('gsisub','calling observer_run()')
#endif
    call observer_run(init_pass=init_pass,last_pass=last_pass)
#ifdef VERBOSE
    call tell('gsisub','returned from observer_run()')
#endif
    if(last_pass) call observer_finalize()
#ifndef HAVE_ESMF
      call destroy_gesfinfo()	! paired with gesinfo()
#endif
  else
     call glbsoi(mype)
  endif

  
  if(last_pass) then
!    Deallocate arrays
     call destroy_pcp_random
#ifndef HAVE_ESMF
     call destroy_mapping
     call destroy_grid_vars
#endif /* HAVE_ESMF */
  endif

#ifdef VERBOSE
  call tell('gsisub','exiting ..')
#endif
! End of gsi driver routine
  return
end subroutine gsisub
