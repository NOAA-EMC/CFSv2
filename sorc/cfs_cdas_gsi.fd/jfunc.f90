module jfunc
!$$$   module documentation block
!                .      .    .                                       .
! module:    jfunc
!   prgmmr: treadon          org: np23                date: 2003-11-24
!
! abstract: module containing variables used in inner loop minimzation
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2004-10-06  kleist, create separate control vector for u,v
!   2004-10-15  parrish, add outer iteration variable for nonlinear qc
!   2004-12-23  treadon - add logical flags first and last
!   2005-02-23  wu - add qoption, dqdt,dqdrh,dqdp and varq for norm RH
!   2005-03-28  wu - replace mlath with mlat             
!   2005-06-03  parrish - add logical switch_on_derivatives
!   2005-09-29  kleist - add pointers for time derivatives
!   2005-11-21  kleist - expand time deriv pointers for tracer tendencies
!   2005-11-29  derber - fix bug in restart
!   2006-02-02  treadon - remove prsi_oz (use ges_prsi array)
!   2006-08-30  zhang,b - add bias correction control parameter
!   2007-03-13  derber - remove qsinv2 and add rhgues
!   2007-04-13  tremolet - use control vectors
!   2008-05-22  guo, j - merge GMAO MERRA changes with NCEP 2008-04-22
!                      - defer GMAO diurnal bias correction changes.
!   2008-12-01  todling - bring in Tremolet's changes
!   2009-06-01  pondeca,sato - add tsensible initialization. used in 2dvar mode only
!   2009-06-01  pondeca - add lgschmidt initalization. this variable controls the B-norm
!                         re-orthogonalization of the gradx vectors in 2dvar mode
!   2010-02-20  parrish - add change to get correct nval_len when using hybrid ensemble with dual resolution.
!   2010-02-20  zhu     - add nrf_levb and nrf_leve
!   2010-03-23  derber  - remove rhgues (not used)
!   2010-03-25  zhu     - add pointer_state
!
! Subroutines Included:
!   sub init_jfunc           - set defaults for cost function variables
!   sub create_jfunc         - allocate cost function arrays 
!   sub destroy_jfunc        - deallocate cost function arrays
!   anav_info                - control variables information
!   sub read_guess_solution  - read guess solution
!   sub write_guess_solution - write guess solution
!   sub strip2               - strip off halo from subdomain arrays
!   sub set_pointer          - set location indices for components of vectors
!
! remarks: variable definitions below
!   def first      - logical flag = .true. on first outer iteration
!   def last       - logical flag = .true. following last outer iteration
!   def switch_on_derivatives - .t. = generate horizontal derivatives
!   def iout_iter  - output file number for iteration information
!   def miter      - number of outer iterations
!   def qoption    - option of q analysis variable; 1:q/qsatg 2:norm RH
!   def iguess     - flag for guess solution
!   def biascor    - background error bias correction coefficient 
!   def bcoption   - 0=ibc (no bias correction to bkg); 1= sbc(original implementation)
!   def diurnalbc  - 1= diurnal bias; 0= persistent bias
!   def niter      - number of inner interations (for each other iter.)
!   def niter_no_qc- number of inner interations without nonlinear qc (for each outer iter.)
!   def jiter      - outer iteration counter
!   def jiterstart - first outloop iteration number
!   def jiterend   - last outloop iteration number
!   def iter       - do loop iteration integer
!   def nclen      - length of control (x,y) vectors
!   def nvals_levs - number of 2d (x/y) state-vector variables
!   def nvals_len  - number of 2d state-vector variables * subdomain size (with buffer)
!   def nval_levs  - number of 2d (x/y) control-vector variables
!   def nval_len   - number of 2d control-vector variables * subdomain size (with buffer)
!   def nstsm      - starting point for streamfunction in control vector for comm.
!                    from here on down, without buffer points
!   def nvpsm      - starting point for velocity pot. in control vector for comm.
!   def npsm       - starting point for ln(ps) in control vector for comm.
!   def ntsm       - starting point for temperature in control vector for comm.
!   def nqsm       - starting point for moisture in control vector for comm.
!   def nozsm      - starting point for ozone in control vector for comm.
!   def nsstsm     - starting point for sst in control vector for comm.
!   def nsltsm     - starting point for skin/land temp. in control vector for comm.
!   def nsitsm     - starting point for skin/ice temp. in control vector for comm.
!   def ncwsm      - starting point for cloud water in control vector for comm.
!   def nst2       - starting point for streamfunction in control vector for comm.
!                    from here on down, including buffer points
!   def nvp2       - starting point for velocity pot. in control vector for comm.
!   def np2        - starting point for ln(ps) in control vector for comm.
!   def nt2        - starting point for temperature in control vector for comm.
!   def nq2        - starting point for moisture in control vector for comm.
!   def noz2       - starting point for ozone in control vector for comm.
!   def nsst2      - starting point for sst in control vector for comm.
!   def nslt2      - starting point for skin/land temp. in control vector for comm.
!   def nsit2      - starting point for skin/ice temp. in control vector for comm.
!   def ncw2       - starting point for cloud water in control vector for comm.
!   def l_foto     - option for foto
!   def print_diag_pcg - option for turning on GMAO diagnostics in pcgsoi
!   def tsensible  - option to use sensible temperature as the control variable. applicable
!                    to the 2dvar mode only
!   def lgschmidt  - option to re-biorthogonalyze the gradx and grady vectors during the
!                    inner iteration using the modified gram-schmidt method. useful for
!                    estimating the analysis error via the projection method. 
!
!   def nrf        - total number of control variables
!   def nrf2       - total number of 2D control variables
!   def nrf3       - total number of 3D control variables
!   def nrf2_loc   - location of 2D control variables
!   def nrf3_loc   - location of 3D control variables
!   def nrf_tracer - location of tracer variables
!   def nrf_3d     - indicator of 2D/3D control variables
!   def nrf_levb   - starting level of 2D/3D control variables
!   def nrf_leve   - ending level of 2D/3D control variables
!   def ntracer    - total number of tracer variables
!   def nrft       - total number of time tendencies for upper level control variables
!   def nrft_      - order of time tendencies for 3d control variables
!   def nvars      - total number of 2d & 3d variables

! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use control_vectors 
  use state_vectors
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_jfunc
  public :: create_jfunc
  public :: destroy_jfunc
  public :: read_guess_solution
  public :: write_guess_solution
  public :: strip2
  public :: set_pointer
  public :: pointer_state
! set passed variables to public
  public :: nrclen,npclen,nsclen,qoption,varq,nval_lenz,dqdrh,dqdt,dqdp,tendsflag,tsensible
  public :: switch_on_derivatives,qgues,qsatg,jiterend,jiterstart,jiter,iter,niter,miter
  public :: diurnalbc,bcoption,biascor,nval2d,dhat_dt,xhat_dt,l_foto,xhatsave,first
  public :: factqmax,factqmin,last,yhatsave,nvals_len,nval_levs,iout_iter,nclen
  public :: niter_no_qc,print_diag_pcg,lgschmidt,penorig,gnormorig,iguess
  public :: nst2,nvp2,np2,nt2,nq2,noz2,nsst2,nslt2,nsit2,ncw2
  public :: nstsm,nvpsm,npsm,ntsm,nqsm,nozsm,nsstsm,nsltsm,nsitsm,ncwsm

  logical first,last,switch_on_derivatives,tendsflag,l_foto,print_diag_pcg,tsensible,lgschmidt
  integer(i_kind) iout_iter,miter,iguess,nclen,qoption
  integer(i_kind) jiter,jiterstart,jiterend,iter
  integer(i_kind) nvals_len,nvals_levs
  integer(i_kind) nval_len,nval_lenz,nval_levs
  integer(i_kind) nstsm,nvpsm,npsm,ntsm,nqsm,nozsm,nsstsm,nsltsm,nsitsm,ncwsm
  integer(i_kind) nst2,nvp2,np2,nt2,nq2,noz2,nsst2,nslt2,nsit2,ncw2
  integer(i_kind) nclen1,nclen2,nrclen,nsclen,npclen
  integer(i_kind) nval2d,nclenz

  integer(i_kind),dimension(0:50):: niter,niter_no_qc
  real(r_kind) factqmax,factqmin,gnormorig,penorig,biascor,diurnalbc
  integer(i_kind) bcoption
  real(r_kind),allocatable,dimension(:,:,:):: qsatg,qgues,dqdt,dqdrh,dqdp 
  real(r_kind),allocatable,dimension(:,:):: varq
  type(control_vector),save :: xhatsave,yhatsave
  type(state_vector),save ::xhat_dt,dhat_dt

contains

  subroutine init_jfunc
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_jfunc
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: initialize cost function variables to defaults
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2004-12-23  treadon - initialize first and last
!   2005-06-03  parrish - initialize switch_on_derivatives
!   2005-10-27  kleist  - initialize tendency flag
!   2006-08-30  zhang,b - initialize bias correction scheme
!   2008-05-12  safford - rm unused uses
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: izero,ione,zero, one
    implicit none
    integer(i_kind) i

    first = .true.
    last  = .false.
    switch_on_derivatives=.false.
    tendsflag=.false.
    l_foto=.false.
    print_diag_pcg=.false.
    tsensible=.false.
    lgschmidt=.false.

    factqmin=one
    factqmax=one
    iout_iter=220_i_kind
    miter=ione
    qoption=ione
    do i=0,50
       niter(i)=izero
       niter_no_qc(i)=1000000_i_kind
    end do
    jiterstart=ione
    jiterend=ione
    jiter=jiterstart
    biascor=-one        ! bias multiplicative coefficient
    diurnalbc=izero     ! 1= diurnal bias; 0= persistent bias
    bcoption=ione       ! 0=ibc; 1=sbc
    nclen=ione
    nclenz=ione

    penorig=zero
    gnormorig=zero

! iguess = -1  do not use guess file
! iguess =  0  write only guess file
! iguess =  1  read and write guess file
! iguess =  2  read only guess file

    iguess=ione

    return
  end subroutine init_jfunc


  subroutine create_jfunc(mlat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_jfunc
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: allocate memory for cost function variables
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2004-07-28  treadon - simplify subroutine argument list
!   2005-03-28  wu - replace mlath with mlat, modify dim of varq 
!   2005-06-15  treadon - remove "use guess_grids"
!   2008-05-12  safford - rm unused uses
!
!   input argument list:
!    mlat
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: ione,zero
    use gridmod, only: lat2,lon2,nsig
    implicit none

    integer(i_kind),intent(in   ) :: mlat

    integer(i_kind) i,j,k

    call allocate_cv(xhatsave)
    call allocate_cv(yhatsave)
    allocate(qsatg(lat2,lon2,nsig),&
         dqdt(lat2,lon2,nsig),dqdrh(lat2,lon2,nsig),&
         varq(ione:mlat,ione:nsig),dqdp(lat2,lon2,nsig),&
         qgues(lat2,lon2,nsig))

    xhatsave=zero
    yhatsave=zero

    do k=ione,nsig
       do j=1,mlat
          varq(j,k)=zero
       end do
    end do

    do k=1,nsig
       do j=1,lon2
          do i=1,lat2
             qsatg(i,j,k)=zero
             dqdt(i,j,k)=zero
             dqdrh(i,j,k)=zero
             dqdp(i,j,k)=zero
             qgues(i,j,k)=zero
          end do
       end do
    end do

    return
  end subroutine create_jfunc
    
  subroutine destroy_jfunc
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_jfunc
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: deallocate memory from cost function variables
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    call deallocate_cv(xhatsave)
    call deallocate_cv(yhatsave)
    deallocate(varq)
    deallocate(dqdt,dqdrh,dqdp,qsatg,qgues)

    return
  end subroutine destroy_jfunc

  subroutine read_guess_solution(dirx,diry,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_guess_solution
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: read in guess solution
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2005-05-05  treadon - read guess solution from 4-byte reals
!   2008-05-12  safford - rm unused uses and vars
!
!   input argument list:
!     mype   - mpi task id
!
!   output argument list:
!     dirx,diry
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_single
    use constants, only: izero,ione
    use mpimod, only: ierror, mpi_comm_world,mpi_real4
    use gridmod, only: nlat,nlon,nsig,itotsub,ltosi_s,ltosj_s,&
         displs_s,ijn_s,latlon11,iglobal
    use obsmod, only: iadate
    implicit none

    integer(i_kind)     ,intent(in   ) :: mype
    type(control_vector),intent(  out) :: dirx,diry

    integer(i_kind) i,k,mm1,myper,kk,i1,i2
    integer(i_kind) nlatg,nlong,nsigg
    integer(i_kind),dimension(5):: iadateg
    real(r_single),dimension(max(iglobal,itotsub)):: fieldx,fieldy
    real(r_single),dimension(nlat,nlon):: xhatsave_g,yhatsave_g
    real(r_single),dimension(nclen):: xhatsave_r4,yhatsave_r4
    
    jiterstart = ione
    mm1=mype+ione
    myper=izero

! Open unit to guess solution.  Read header.  If no header, file is
! empty and exit routine
    open(12,file='gesfile_in',form='unformatted')
    iadateg=izero
    nlatg=izero
    nlong=izero
    nsigg=izero
    read(12,end=1234)iadateg,nlatg,nlong,nsigg
    if(iadate(ione) == iadateg(ione) .and. iadate(2_i_kind) == iadate(2_i_kind) .and. &
       iadate(3_i_kind) == iadateg(3_i_kind) .and. iadate(4_i_kind) == iadateg(4_i_kind) .and. &
       iadate(5_i_kind) == iadateg(5_i_kind) .and. nlat == nlatg .and. &
       nlon == nlong .and. nsig == nsigg) then
       if(mype == izero) write(6,*)'READ_GUESS_SOLUTION:  read guess solution for ',&
                     iadateg,nlatg,nlong,nsigg
       jiterstart=izero
         
! Let all tasks read gesfile_in to pick up bias correction (second read)

! Loop to read input guess fields.  After reading in each field & level,
! scatter the grid to the appropriate location in the xhat and yhatsave
! arrays.
       do k=ione,nval_levs
          read(12,end=1236) xhatsave_g,yhatsave_g
          do kk=1,itotsub
             i1=ltosi_s(kk); i2=ltosj_s(kk)
             fieldx(kk)=xhatsave_g(i1,i2)
             fieldy(kk)=yhatsave_g(i1,i2)
          end do
          i=(k-ione)*latlon11 + ione
          call mpi_scatterv(fieldx,ijn_s,displs_s,mpi_real4,&
                   xhatsave_r4(i),ijn_s(mm1),mpi_real4,myper,mpi_comm_world,ierror)
          call mpi_scatterv(fieldy,ijn_s,displs_s,mpi_real4,&
                   yhatsave_r4(i),ijn_s(mm1),mpi_real4,myper,mpi_comm_world,ierror)
       end do  !end do over nval_levs

!      Read radiance and precipitation bias correction terms
       read(12,end=1236) (xhatsave_r4(i),i=nclen1+ione,nclen),(yhatsave_r4(i),i=nclen1+ione,nclen)
       do i=1,nclen
          dirx%values(i)=real(xhatsave_r4(i),r_kind)
          diry%values(i)=real(yhatsave_r4(i),r_kind)
       end do

    else
       if(mype == izero) then
          write(6,*) 'READ_GUESS_SOLUTION:  INCOMPATABLE GUESS FILE, gesfile_in'
          write(6,*) 'READ_GUESS_SOLUTION:  iguess,iadate,iadateg=',iguess,iadate,iadateg
          write(6,*) 'READ_GUESS_SOLUTION:  nlat,nlatg,nlon,nlong,nsig,nsigg=',&
                      nlat,nlatg,nlon,nlong,nsig,nsigg
       end if
    endif
    close(12)
    return

! The guess file is empty.  Do not return an error code but print a message to
! standard out.
1234 continue
    if(mype == izero) then
       write(6,*) 'READ_GUESS_SOLUTION:  NO GUESS FILE, gesfile_in'
       write(6,*) 'READ_GUESS_SOLUTION:  iguess,iadate,iadateg=',iguess,iadate,iadateg
       write(6,*) 'READ_GUESS_SOLUTION:  nlat,nlatg,nlon,nlong,nsig,nsigg=',&
                   nlat,nlatg,nlon,nlong,nsig,nsigg
    end if
    close(12)
    return

! Error contition reading level or bias correction data.  Set error flag and
! return to the calling program.
1236 continue
    if (mype==izero) write(6,*) 'READ_GUESS_SOLUTION:  ERROR in reading guess'
    close(12)
    call stop2(76)

    return
  end subroutine read_guess_solution
  
  subroutine write_guess_solution(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_guess_solution
!   prgmmr: treadon          org: np23               date:  2003-11-24
!
! abstract: write out guess solution (not from spectral forecast)
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2005-03-10  treadon - remove iadate from calling list, access via obsmod
!   2005-05-05  treadon - write guess solution using 4-byte reals
!   2008-05-12  safford - rm unused uses
!   2008-12-13  todling - strip2 called w/ consistent interface
!
!   input argument list:
!     mype   - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_single
    use mpimod, only: ierror, mpi_comm_world, mpi_real4
    use gridmod, only: ijn,latlon11,displs_g,ltosj,ltosi,nsig,&
         nlat,nlon,lat1,lon1,itotsub,iglobal
    use obsmod, only: iadate
    use constants, only: izero,ione,zero
    implicit none

    integer(i_kind),intent(in   ) :: mype

    integer(i_kind) i,j,k,mm1,mypew,kk,i1,i2,ie,is
    real(r_single),dimension(lat1,lon1,2):: field
    real(r_single),dimension(max(iglobal,itotsub)):: fieldx,fieldy
    real(r_single),dimension(nlat,nlon):: xhatsave_g,yhatsave_g
    real(r_single),dimension(nrclen):: xhatsave4,yhatsave4

    mm1=mype+ione
    mypew=izero
    
! Write header record to output file
    if (mype==mypew) then
       open(51,file='gesfile_out',form='unformatted')
       write(51) iadate,nlat,nlon,nsig
    endif

! Loop over levels.  Gather guess solution and write to output
    do k=ione,nval_levs
       ie=(k-ione)*latlon11 + ione
       is=ie+latlon11
       call strip2(xhatsave%values(ie:is),yhatsave%values(ie:is),field)
       call mpi_gatherv(field(1,1,1),ijn(mm1),mpi_real4,&
            fieldx,ijn,displs_g,mpi_real4,mypew,&
            mpi_comm_world,ierror)
       call mpi_gatherv(field(1,1,2),ijn(mm1),mpi_real4,&
            fieldy,ijn,displs_g,mpi_real4,mypew,&
            mpi_comm_world,ierror)

! Transfer to global arrays
       do j=1,nlon
          do i=1,nlat
             xhatsave_g(i,j)=zero
             yhatsave_g(i,j)=zero
          end do
       end do
       do kk=1,iglobal
          i1=ltosi(kk); i2=ltosj(kk)
          xhatsave_g(i1,i2)=fieldx(kk)
          yhatsave_g(i1,i2)=fieldy(kk)
       end do

! Write level record
       if (mype==mypew) write(51) xhatsave_g,yhatsave_g
    end do  !end do over nval_levs

! Write radiance and precipitation bias correction terms to output file
    if (mype==mypew) then
       do i=1,nrclen
          xhatsave4(i)=xhatsave%values(nclen1+i)
          yhatsave4(i)=yhatsave%values(nclen1+i)
       end do
       write(51) (xhatsave4(i),i=1,nrclen),(yhatsave4(i),i=1,nrclen)
       close(51)
       write(6,*)'WRITE_GUESS_SOLUTION:  write guess solution for ',&
                  iadate,nlat,nlon,nsig
    endif

    return
  end subroutine write_guess_solution

    subroutine strip2(field_in1,field_in2,field_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strip2
!   prgmmr: treadon          org: np23                date: 2003-11-24
!
! abstract: strip off halo from two subdomain arrays & combine into
!           single output array
!
! program history log:
!   2003-11-24  treadon
!   2004-05-18  kleist, documentation
!   2008-05-12  safford - rm unused uses
!
!   input argument list:
!     field_in1 - subdomain field one with halo
!     field_in2 - subdomain field two with halo
!
!   output argument list:
!     field_out - combined subdomain fields with halo stripped
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_single
    use constants, only: ione
    use gridmod, only: lat1,lon1,lat2,lon2
    implicit none

    real(r_single),dimension(lat1,lon1,2),intent(  out) :: field_out
    real(r_kind)  ,dimension(lat2,lon2)  ,intent(in   ) :: field_in1,field_in2

    integer(i_kind) i,j,jp1


    do j=1,lon1
       jp1 = j+ione
       do i=1,lat1
          field_out(i,j,1)=field_in1(i+ione,jp1)
          field_out(i,j,2)=field_in2(i+ione,jp1)
       end do
    end do

    return
  end subroutine strip2

  subroutine set_pointer
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set_pointer
!   prgmmr: treadon          org: np23                date: 2004-07-28
!
! abstract: Set length of control vector and other control 
!           vector constants
!
! program history log:
!   2004-07-28  treadon
!   2006-04-21  kleist - include pointers for more time tendency arrays
!   2008-12-04  todling - increase number of 3d fields from 6 to 8 
!   2009-09-16  parrish - add hybrid_ensemble connection in call to setup_control_vectors
!   2010-03-01  zhu     - add nrf_levb and nrf_leve, generalize nval_levs
!                       - generalize vector starting points such as nvpsm, nst2, and others
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: izero,ione
    use gridmod, only: lat1,lon1,latlon11,latlon1n,nsig,lat2,lon2
    use gridmod, only: nnnn1o,regional,nlat,nlon
    use radinfo, only: npred,jpch_rad
    use pcpinfo, only: npredp,npcptype
    use gsi_4dvar, only: nsubwin, lsqrtb
    use bias_predictors, only: setup_predictors
    use hybrid_ensemble_parameters, only: l_hyb_ens,n_ens,generate_ens,grd_ens
    use control_vectors, only: nrf,nrf2,nrf3,nrf_3d,nrf_levb,nrf_leve,nrf_var,nrf2_sst
    implicit none

    integer(i_kind) ii,nx,ny,mr,nr,nf,n,klevb,kleve,ngrid
    character(len=5) cvar

    nvals_levs=8*nsig+2+ione        ! +1 for extra level in p3d
    nvals_len=nvals_levs*latlon11

    nval_levs=nrf3*nsig+nrf2
    nval_len=nval_levs*latlon11
    if(l_hyb_ens) then
       nval_len=nval_levs*latlon11+n_ens*nsig*grd_ens%latlon11
    end if
    nsclen=npred*jpch_rad
    npclen=npredp*npcptype
    nclen=nsubwin*nval_len+nsclen+npclen
    nrclen=nsclen+npclen
    nclen1=nclen-nrclen
    nclen2=nclen1+nsclen
  
    if(lsqrtb.or.(l_hyb_ens.and.generate_ens)) then
       if(regional) then
          nval2d=nlat*nlon*3
       else
!           following lifted from subroutine create_berror_vars in module berror.f90
!            inserted because create_berror_vars called after this routine
          nx=nlon*3/2
          nx=nx/2*2
          ny=nlat*8/9
          ny=ny/2*2
          if(mod(nlat,2)/=izero)ny=ny+ione
          mr=izero
          nr=nlat/4
          nf=nr
          nval2d=(ny*nx + 2*(2*nf+ione)*(2*nf+ione))*3
       end if
       nval_lenz=nval2d*nnnn1o
       nclenz=nsubwin*nval_lenz+nsclen+npclen
    else
       nval2d=latlon11
    end if

    klevb=ione
    if (nrf_3d(1)) then
       kleve=klevb+nsig-ione
    else
       kleve=klevb
    end if
    nrf_levb(1)=klevb
    nrf_leve(1)=kleve
    do n=2,nrf 
       klevb=nrf_leve(n-1)+ione
       if (nrf_3d(n)) then 
          kleve=klevb+nsig-ione
       else
          kleve=klevb
       end if 
       nrf_levb(n)=klevb
       nrf_leve(n)=kleve
    end do

!   For new mpi communication, define vector starting points
!   for each variable type using the subdomains size without 
!   buffer points
!   For new mpi communication, define vector starting points
!   for each variable type using the subdomains size without
!   buffer points
    ii=izero
    do n=1,nrf
       if (nrf_3d(n)) then
          ngrid=lat1*lon1*nsig
       else
          ngrid=lat1*lon1
       end if

       cvar=nrf_var(n)
       select case(cvar)
          case('sf','SF')
             nstsm=ii+ione
          case('vp','VP')
             nvpsm=ii+ione
          case('t','T')
             ntsm=ii+ione
          case('q','Q')
             nqsm=ii+ione
          case('oz','OZ')
             nozsm=ii+ione
          case('cw','CW')
             ncwsm=ii+ione   
          case('ps','PS')
             npsm=ii+ione
          case('sst','SST')
             nsstsm=ii+ione
          case default
             write(6,*) 'allocate_cv: ERROR, unrecognized control variable ',cvar
             call stop2(100)
       end select
      ii=ii+ngrid
    end do
    if (nrf2_sst>izero) then
       nsltsm=ii+ione
       nsitsm=nsltsm+(lat1*lon1)
    end if      


!   Define vector starting points for subdomains which include
!   buffer points
    ii=izero
    do n=1,nrf
       if (nrf_3d(n)) then
          ngrid=latlon1n
       else
          ngrid=latlon11
       end if

       cvar=nrf_var(n)
       select case(cvar)
          case('sf','SF')
             nst2=ii+ione
          case('vp','VP')
             nvp2=ii+ione
          case('t','T')
             nt2=ii+ione
          case('q','Q')
             nq2=ii+ione
          case('oz','OZ')
             noz2=ii+ione
          case('cw','CW')
             ncw2=ii+ione
          case('ps','PS')
             np2=ii+ione
          case('sst','SST')
             nsst2=ii+ione
          case default
             write(6,*) 'allocate_cv: ERROR, unrecognized control variable ',cvar
             call stop2(100)
       end select
       ii=ii+ngrid
    end do
    if (nrf2_sst>izero) then
       nslt2=ii+ione
       nsit2=nslt2+latlon11
    end if


    if (lsqrtb) then
       CALL setup_control_vectors(nsig,lat2,lon2,latlon11,latlon1n, &
                                & nsclen,npclen,nclenz,nsubwin,nval_lenz,lsqrtb)
    else
       if(l_hyb_ens) then
          CALL setup_control_vectors(nsig,lat2,lon2,latlon11,latlon1n, &
                                 & nsclen,npclen,nclen,nsubwin,nval_len,lsqrtb,n_ens)
       else
          CALL setup_control_vectors(nsig,lat2,lon2,latlon11,latlon1n, &
                                 & nsclen,npclen,nclen,nsubwin,nval_len,lsqrtb)
       end if
    endif
    CALL setup_state_vectors(latlon11,latlon1n,nvals_len,lat2,lon2,nsig)
    CALL setup_predictors(nrclen,nsclen,npclen)

  end subroutine set_pointer

subroutine pointer_state(yst,u,v,t,tsen,q,oz,cw,p3d,p,sst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pointer_state
!   prgmmr:
!
! abstract:
!
! program history log:
!   2010-03-25  zhu
!
!   input argument list:
!
!   output argument list:
!    yst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use constants, only: izero
  use control_vectors, only: nrf3_oz,nrf3_cw,nrf2_sst
  use mpimod, only: mype
  use state_vectors
  implicit none
  type(state_vector), intent(in) :: yst
  real(r_kind),dimension(:),pointer,optional,intent(out) :: t,tsen,q,u,v,oz,cw
  real(r_kind),dimension(:),pointer,optional,intent(out) :: sst,p
  real(r_kind),dimension(:),pointer,optional,intent(out) :: p3d

  if (present(u   )) u   => yst%u
  if (present(v   )) v   => yst%v
  if (present(t   )) t   => yst%t
  if (present(tsen)) tsen => yst%tsen
  if (present(q   )) q   => yst%q
  if (present(oz  )) then
     if (nrf3_oz>izero) then
        oz  => yst%oz
     else
        if (mype==izero) write(6,*) 'OZ is not a control variable'
     end if
  end if
  if (present(cw  )) then
     if (nrf3_cw>izero) then
        cw  => yst%cw
     else
        if (mype==izero) write(6,*) 'CW is not a control variable'
     end if
  end if
  if (present(p3d )) p3d => yst%p3d
  if (present(sst )) then
     if (nrf2_sst>izero) then
        sst => yst%sst
     else
        if (mype==izero) write(6,*) 'SST is not a control variable'
     end if
  end if

  return
end subroutine pointer_state

end module jfunc
