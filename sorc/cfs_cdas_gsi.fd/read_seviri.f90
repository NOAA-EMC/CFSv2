subroutine read_seviri(mype,val_sev,ithin,rmesh,jsatid,&
     gstime,infile,lunout,obstype,nread,ndata,nodata,twind,sis, &
     mype_root,mype_sub,npe_sub,mpi_comm_sub)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_seviri                  read seviri bufr data
!   prgmmr: liu, haixia             org: np23                date: 2009-08-10
!
! abstract:  This routine reads BUFR format SEVIRI 1b radiance (brightness
!            temperature) files, which are bufrized from the NESDIS 1b data.  Optionally, the
!            data are thinned to a specified resolution using simple
!            quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2009-08-10  hliu
!
!   input argument list:
!     mype     - mpi task id
!     val_sev  - weighting factor applied to super obs
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
!   output argument list:
!     nread    - number of BUFR SEVIRI 1b observations read
!     ndata    - number of BUFR SEVIRI 1b profiles retained for further processing
!     nodata   - number of BUFR SEVIRI 1b observations retained for further processing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
            checkob,finalcheck,score_crit
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,txy2ll,tll2xy,rlats,rlons
  use constants, only: deg2rad,izero,ione,zero,rad2deg,r60inv
  use obsmod, only: offtime_data
  use radinfo, only: iuse_rad,jpch_rad,nusis
  use gsi_4dvar, only: iadatebgn,iadateend,l4dvar,iwinbgn,winlen
  implicit none

! Declare passed variables
  character(len=*),intent(in):: infile,obstype,jsatid
  character(len=*),intent(in):: sis
  integer(i_kind),intent(in):: mype,lunout,ithin
  integer(i_kind),intent(inout):: ndata,nodata
  integer(i_kind),intent(inout):: nread
  real(r_kind),intent(in):: rmesh,gstime,twind
  real(r_kind),intent(inout):: val_sev
  integer(i_kind),intent(in) :: mype_root
  integer(i_kind),intent(in) :: mype_sub
  integer(i_kind),intent(in) :: npe_sub
  integer(i_kind),intent(in) :: mpi_comm_sub

! Declare local parameters
  integer(i_kind),parameter:: maxinfo=33_i_kind
  real(r_kind),parameter:: r70=70.0_r_kind
  real(r_kind),parameter:: r65=65.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=50.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind
  real(r_kind),parameter:: bmiss = 1.0e11_r_kind

! Declare local variables
  logical outside,iuse,assim,clrsky,allsky

  character(8) subset,subcsr,subasr
  character(80):: hdrsevi             ! seviri header

  integer(i_kind) nchanl,ilath,ilonh,ilzah,iszah,irec,isub,next
  integer(i_kind) nmind,lnbufr,idate,ilat,ilon,nhdr,nchn,ncld,nbrst,jj
  integer(i_kind) ireadmg,ireadsb,iret,nele,itt
  integer(i_kind) itx,i,k,isflg,kidsat,n,iscan,idomsfc
  integer(i_kind) idate5(5)

  real(r_kind) dg2ew,sstime,tdiff,t4dv,sfcr
  real(r_kind) dlon,dlat,timedif,crit1,dist1
  real(r_kind) dlon_earth,dlat_earth
  real(r_kind) pred
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:3):: ts
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind),allocatable,dimension(:,:):: data_all

  real(r_kind),allocatable,dimension(:):: hdr                        !  seviri imager header
  real(r_kind),allocatable,dimension(:,:):: datasev1,datasev2        !  seviri imager data
  real(r_kind) rclrsky

  real(r_kind) disterr,disterrmax,dlon00,dlat00
  integer(i_kind) ntest


!**************************************************************************
! Initialize variables
  lnbufr = 10_i_kind
  disterrmax=zero
  ntest=izero
  dg2ew = r360*deg2rad

  ilon=3_i_kind
  ilat=4_i_kind

! HLIU: NEED TO confirm
  rlndsea(0) = zero
  rlndsea(1) = 15._r_kind
  rlndsea(2) = 10._r_kind
  rlndsea(3) = 15._r_kind
  rlndsea(4) = 30._r_kind

  nread=izero
  ndata=izero
  nodata=izero
  nchanl=8_i_kind              ! the channel number

  ilath=8_i_kind               ! the position of latitude in the header
  ilonh=9_i_kind               ! the position of longitude in the header
  ilzah=10_i_kind              ! satellite zenith angle
  iszah=11_i_kind              ! solar zenith angle
  subcsr='NC021043'            ! sub message
  subasr='NC021042'            ! sub message

! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.

  assim=.false.
  search: do i=1,jpch_rad
     if ((trim(nusis(i))==trim(sis)) .and. (iuse_rad(i)>izero)) then
        assim=.true.
        exit search
     endif
  end do search
  if (.not.assim) val_sev=zero

! Make thinning grids
  call makegrids(rmesh,ithin)

! Open bufr file.
  call closbf(lnbufr)
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)
  call readmg(lnbufr,subset,idate,iret)

! Check the data set
  if( iret/=izero) then
     write(6,*) 'READ_SEVIRI: SKIP PROCESSING OF SEVIRI FILE'
     write(6,*) 'infile=', lnbufr, infile
     go to 900
  endif

  clrsky=.false.
  allsky=.false.
  if(subset == subcsr) then
     clrsky=.true.
  elseif(subset == subasr) then
     allsky=.true.
  else
     write(6,*) 'READ_SEVIRI: SKIP PROCESSING OF SEVIRI FILE'
     write(6,*) 'infile=', lnbufr, infile,' subset=', subset
     go to 900
  endif

! Set BUFR string based on seviri data set
  if (clrsky) then
     hdrsevi='SAID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA SOZA'
     nhdr=11_i_kind
     nchn=12_i_kind
     ncld=nchn
     nbrst=nchn
  else if (allsky) then
     hdrsevi='SAID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH'
     nhdr=9_i_kind
     nchn=11_i_kind
     ncld=2_i_kind
     nbrst=nchn*6                ! channel dependent: all, clear, cloudy, low, middle and high clouds
  endif
  allocate(datasev1(ione,ncld))     ! not channel dependent
  allocate(datasev2(ione,nbrst))    ! channel dependent: all, clear, cloudy, low, middle and high clouds
  allocate(hdr(nhdr))


! Check the data time
  write(6,*)'READ_SEVIRI: bufr file date is ',idate,infile
  IF (idate<iadatebgn.OR.idate>iadateend) THEN
     if(offtime_data) then
       write(6,*)'***READ_SEVIRI analysis and data file date differ, but use anyway'
     else
       write(6,*)'***READ_SEVIRI ERROR*** ',&
          'incompatable analysis and observation date/time'
     end if
     write(6,*)'Analysis start  :',iadatebgn
     write(6,*)'Analysis end    :',iadateend
     write(6,*)'Observation time:',idate
     if(.not.offtime_data) go to 900
  ENDIF

! Allocate arrays to hold all data for given satellite
  nele=maxinfo+nchanl
  allocate(data_all(nele,itxmax))

  next=izero

!  Reopen unit to bufr file
  call closbf(lnbufr)
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)

! Big loop over bufr file
  do while (ireadmg(lnbufr,subset,idate) >= izero)
     next=next+ione
     if(next == npe_sub)next=izero     
     if(next /= mype_sub)cycle

     read_loop: do while (ireadsb(lnbufr) == izero)

!       Read through each record
        call ufbint(lnbufr,hdr,nhdr,ione,iret,hdrsevi)
        if(jsatid == 'm08') kidsat = 55_i_kind
        if(jsatid == 'm09') kidsat = 56_i_kind
        if(jsatid == 'm10') kidsat = 57_i_kind
        if(hdr(1) /= kidsat) cycle read_loop

        call ufbrep(lnbufr,datasev1,ione,ncld,iret,'NCLDMNT')
        call ufbrep(lnbufr,datasev2,ione,nbrst,iret,'TMBRST')

        nread=nread+nchanl
 
        rclrsky=bmiss
        do n=1,ncld
          if(datasev1(1,n)>= zero .and. datasev1(1,n) <= 100.0_r_kind ) then
            rclrsky=datasev1(1,n)
!           first QC filter out data with less clear sky fraction
            if ( rclrsky < r70 ) cycle read_loop
          end if
        end do

       if (clrsky) then     ! asr bufr has no sza
!         remove the obs whose satellite zenith angles larger than 65 degree
          if ( hdr(ilzah) > r65 ) cycle read_loop
       end if

!       Compare relative obs time with window.  If obs 
!       falls outside of window, don't use this obs
        idate5(1) = hdr(2)     !year
        idate5(2) = hdr(3)     ! month
        idate5(3) = hdr(4)     ! day
        idate5(4) = hdr(5)     ! hours
        idate5(5) = hdr(6)     ! minutes
        call w3fs21(idate5,nmind)
        t4dv = (real((nmind-iwinbgn),r_kind) + real(hdr(7),r_kind)*r60inv)*r60inv
        if (l4dvar) then
          if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
          sstime = real(nmind,r_kind) + real(hdr(7),r_kind)*r60inv
          tdiff=(sstime-gstime)*r60inv
          if (abs(tdiff)>twind) cycle read_loop
        endif

!       Convert obs location from degrees to radians
        if (hdr(ilonh)>=r360) hdr(ilonh)=hdr(ilonh)-r360
        if (hdr(ilonh)< zero) hdr(ilonh)=hdr(ilonh)+r360

        dlon_earth=hdr(ilonh)*deg2rad
        dlat_earth=hdr(ilath)*deg2rad

!       If regional, map obs lat,lon to rotated grid.
        if(regional)then

!          Convert to rotated coordinate.  dlon centered on 180 (pi), 
!          so always positive for limited area
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)

           if(diagnostic_reg) then
              call txy2ll(dlon,dlat,dlon00,dlat00)
              ntest=ntest+ione
              disterr=acos(sin(dlat_earth)*sin(dlat00)+cos(dlat_earth)*cos(dlat00)* &
                   (sin(dlon_earth)*sin(dlon00)+cos(dlon_earth)*cos(dlon00)))*rad2deg
              disterrmax=max(disterrmax,disterr)
           end if

!          Check to see if in domain.  outside=.true. if dlon_earth,
!          dlat_earth outside domain, =.false. if inside
           if(outside) cycle read_loop

!       Global case
        else
           dlon=dlon_earth
           dlat=dlat_earth
           call grdcrd(dlat,ione,rlats,nlat,ione)
           call grdcrd(dlon,ione,rlons,nlon,ione)
        endif

        if (l4dvar) then
          crit1=0.01_r_kind
        else
          timedif = 6.0_r_kind*abs(tdiff)        ! range:  0 to 18
          crit1=0.01_r_kind+timedif
        endif
        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
        if(.not. iuse)cycle read_loop

!       Locate the observation on the analysis grid.  Get sst and land/sea/ice
!       mask.  

!       isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed                         


        call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
            ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)


        crit1=crit1+rlndsea(isflg)
!       call checkob(dist1,crit1,itx,iuse)
!       if(.not. iuse)cycle read_loop

!       Set common predictor parameters

!test
        pred=zero
!test        
!       Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"

        crit1 = crit1+pred  
        call finalcheck(dist1,crit1,itx,iuse)

        if(.not. iuse)cycle read_loop
  
        iscan = nint(hdr(ilzah))+1.001_r_kind ! integer scan position HLIU check this
 
!       Transfer information to work array
        data_all( 1,itx) = hdr(1)                     ! satellite id
        data_all( 2,itx) = t4dv                       ! analysis relative time
        data_all( 3,itx) = dlon                       ! grid relative longitude
        data_all( 4,itx) = dlat                       ! grid relative latitude
        data_all( 5,itx) = hdr(ilzah)*deg2rad         ! satellite zenith angle (radians)
        data_all( 6,itx) = bmiss                      ! satellite azimuth angle (radians)
        data_all( 7,itx) = rclrsky                    ! clear sky amount
        data_all( 8,itx) = iscan                      ! integer scan position
        data_all( 9,itx) = hdr(iszah)                 ! solar zenith angle
        data_all(10,itx) = bmiss                      ! solar azimuth angle
        data_all(11,itx) = sfcpct(0)                  ! sea percentage of
        data_all(12,itx) = sfcpct(1)                  ! land percentage
        data_all(13,itx) = sfcpct(2)                  ! sea ice percentage
        data_all(14,itx) = sfcpct(3)                  ! snow percentage
        data_all(15,itx)= ts(0)                       ! ocean skin temperature
        data_all(16,itx)= ts(1)                       ! land skin temperature
        data_all(17,itx)= ts(2)                       ! ice skin temperature
        data_all(18,itx)= ts(3)                       ! snow skin temperature
        data_all(19,itx)= tsavg                       ! average skin temperature
        data_all(20,itx)= vty                         ! vegetation type
        data_all(21,itx)= vfr                         ! vegetation fraction
        data_all(22,itx)= sty                         ! soil type
        data_all(23,itx)= stp                         ! soil temperature
        data_all(24,itx)= sm                          ! soil moisture
        data_all(25,itx)= sn                          ! snow depth
        data_all(26,itx)= zz                          ! surface height
        data_all(27,itx)= idomsfc + 0.001             ! dominate surface type
        data_all(28,itx)= sfcr                        ! surface roughness
        data_all(29,itx)= ff10                        ! ten meter wind factor
        data_all(30,itx) = dlon_earth*rad2deg         ! earth relative longitude (degrees)
        data_all(31,itx) = dlat_earth*rad2deg         ! earth relative latitude (degrees)

        data_all(32,itx) = val_sev
        data_all(33,itx) = itt

        do k=1,nchanl
           if (clrsky) then
              data_all(k+maxinfo,itx)=datasev2(1,k+3)     ! for chn 4,5,6,7,8,9,10,11
           else if (allsky) then
              jj=(k+2)*6+1
              data_all(k+maxinfo,itx)=datasev2(1,jj)     ! all-sky radiance for chn 4,5,6,7,8,9,10,11
           end if
        end do

!    End of satellite read block
     enddo read_loop
  enddo
  call closbf(lnbufr)

  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
       nele,itxmax,nread,ndata,data_all,score_crit)

! If no observations read, jump to end of routine.

! Allow single task to check for bad obs, update superobs sum,
! and write out data to scratch file for further processing.
  if (mype_sub==mype_root.and.ndata>0) then

  do n=1,ndata
    do k=1,nchanl
        if(data_all(k+maxinfo,n) > tbmin .and. &
           data_all(k+maxinfo,n) < tbmax)nodata=nodata+ione
    end do
    itt=nint(data_all(maxinfo,n))
    super_val(itt)=super_val(itt)+val_sev
  end do

! Write retained data to local file
 write(lunout) obstype,sis,maxinfo,nchanl,ilat,ilon
 write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)

  endif

! Deallocate local arrays
 deallocate(data_all)

! Deallocate satthin arrays
900 continue
 call destroygrids

! Print data counts
  write(6,9000) infile,sis,nread,rmesh,ndata
9000 format(' READ_SEVIRI:  infile=',a10,&
          '   sis=',a20,&
          '   nread=',i10, &
          '   rmesh=',f7.3,'   ndata=',i10)

 if(diagnostic_reg.and.ntest>izero) write(6,*)'READ_SEVIRI:  ',&
      'mype,ntest,disterrmax=',mype,ntest,disterrmax

! End of routine
 return
end subroutine read_seviri
