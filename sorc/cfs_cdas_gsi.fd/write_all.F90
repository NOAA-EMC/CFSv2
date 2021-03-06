!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  write_all --- Write various output files
!
! !INTERFACE:
!

subroutine write_all(increment,mype)

! !USES:

  use kinds, only: i_kind
  
  use mpimod, only: npe

  use gridmod, only: regional

  use constants, only: izero, ione, zero
  
  use jfunc, only: biascor
  
  use guess_grids, only: ntguessig
  use guess_grids, only: ges_z

  use m_gsibiases, only: bias_tv, bias_q, bias_oz, bias_cwmr, bias_tskin
  use m_gsibiases, only: bias_ps, bias_vor, bias_div, bias_u, bias_v
  use m_gsibiases ,only : nbc

  use gsi_io, only: write_bias

  use regional_io, only: write_regional_analysis

  use ncepgfs_io, only: write_gfs
  
  implicit none

! !INPUT PARAMETERS:

  logical        , intent(in   ) :: increment  ! when .t., write out w/ increment filenames
  integer(i_kind), intent(in   ) :: mype       ! task number

! !OUTPUT PARAMETERS:

! !DESCRIPTION: This routine writes various output files at the end of 
!           a gsi run.  Output files written by this routine include
!    \begin{itemize}
!          \item updated radiance bias correction coefficients
!          \item updated precipitation bias correction coefficients
!          \item regional analysis grid (grid space)
!          \item global atmospheric analysis (spectral coefficients)
!          \item global surface analysis (grid space)
!          \item global bias correction fields (spectral coefficients)
!          \item analysis increment (grid space)
!    \end{itemize}
!
!           Not all of the above files are written in any given gsi
!           run.  Creation of output files is controlled by various
!           flags as indicated in the code below
!
! !REVISION HISTORY:
!
!   1990-10-10  parrish
!   1998-07-10  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-15  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue; added intent/only's
!   2004-11-30  parrish - modify regional calls for netcdf and binary output
!   2004-12-29  treadon - repackage regional analysis write 
!   2005-02-09  kleist  - remove open and close for iosfc
!   2005-03-10  treadon - remove iadate and igdate
!   2005-05-27  pondeca - bypass radinfo_ and pcpinfo_write when twodvar_regional=.t.
!   2005-06-27  guo     - added interface to GMAO gridded fields
!   2005-07-25  treadon - remove "use m_checksums,only : checksums_show" since not used
!   2005-12-09  guo     - comments added
!   2006-01-10  treadon - use ncepgfs_io module
!   2006-03-13  treadon - increase filename to 24 characters
!   2006-04-14  treadon - replace call write_gfsatm for bias with write_bias
!   2006-06-08  zhang,b - change "biascor>0" to "biascor>=0" for debug purpose
!   2006-07-31  kleist  - use ges_ps instead of ln(ps)
!   2006-12-04  todling - merge in GMAO bias correction changes
!   2007-07-20  todling - move solution write-out call to glbsoi
!   2007-11-12  todling - move write of sat/pcp bias to glbsoi
!   2009-01-28  todling - move ESMF if from glbsoi to this routine
!                       - remove original GMAO interface
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
! !AUTHOR: parrish          org: np22                date: 1990-10-10
!
!EOP
!-------------------------------------------------------------------------

#ifndef HAVE_ESMF
! Declare local variables
  character(24):: filename
  integer(i_kind) mype_atm,mype_bias,mype_sfc,iret_bias
  
!********************************************************************


! Regional output
  if (regional) call write_regional_analysis(mype)


! Global output
  if(.not.regional) then

!    NCEP GFS interface

!    Write atmospheric and surface analysis
     mype_atm=izero
     mype_sfc=npe/2
     call write_gfs(increment,mype,mype_atm,mype_sfc)

!    Write file bias correction     
     if(biascor >= zero)then
        filename='biascor_out'
        mype_bias=npe-ione
        call write_bias(filename,mype,mype_bias,nbc,&
             ges_z(1,1,ntguessig),bias_ps,bias_tskin,&
             bias_vor,bias_div,bias_u,bias_v,bias_tv,&
             bias_q,bias_cwmr,bias_oz,iret_bias)
     endif

! End of global block
  end if

#endif /* HAVE_ESMF */

  return
end subroutine write_all
