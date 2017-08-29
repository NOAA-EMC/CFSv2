!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM:  GODAS_EDITPRF
!   PRGMMR: David W. Behringer  ORG: NP23        DATE: 2003-07-23
!
! ABSTRACT:  Perform quality control on subsurface temperature data
!   for use in Global Ocean Data Assimilation System
!
! PROGRAM HISTORY LOG:
! 2003-07-23  David W. Behringer
! 2005-10-06  Diane C. Stokes - discard profiles in Gulf of Mexico.
!                 Recent deployment of frequently reporting profiling 
!                 buoys in this region is overwhelming analysis.  
!                 This is temporary until a more suitable and generic 
!                 fix is available.
! 2005-11-01  David W. Behringer - profiles in the Gulf of Mexico are once
!                 again allowed.  A modification to the program avePrfDly.f
!                 identifies platforms that are reporting too frequently
!                 and super-obs them.  The editing in editPrf.f has also
!                 been tuned.  A second method for fixing near surface
!                 spikes and a method for removing deep inversions have
!                 been added.
! 2010-04-23  David W. Behringer - The reference profile is now taken from 
!                 the 3D gridded WOA-5 climatology from NODC.  A histogram 
!                 of differences from the reference profile is used to aid 
!                 in editing of observed peofiles.
!
! USAGE:
!   INPUT FILES:
!     UNIT 11  - TEMPERATURE PROFILE DATA IN IEEE
!              - WOA-5 CLIMATOLOGY FILE IN NETCDF
!
!   OUTPUT FILES:
!     UNIT 51  - QC'D TEMPERATURE PROFILE DATA IN IEEE
!     UNIT 06  - UNIT 6 (STANDARD PRINTFILE)
!
!   WORK FILES:  (INCLUDING SCRATCH FILES)
!     UNIT 80  - SCRATCH FILE FOR GODAS PROFILE DATA
!     UNIT 81  - SCRATCH FILE FOR GODAS - OBS PROFILE DATA
!
!   SUBPROGRAMS CALLED FROM PROGRAM: (LIST ALL CALLED FROM ANYWHERE IN CODES)
!     UNIQUE:    - indx, zintrp
!     LIBRARY:
!       W3LIB    - w3tagb, w3tage, errexit
!       NETCDF   - nf_open, nf_inq_varid, nf_get_var_real, nf_inq_var,
!                  nf_get_vara_real, nf_close, nf_strerror
!
!   SUBPROGRAMS CALLED FROM MAIN: (LIST ALL CALLED FROM MAIN)
!     UNIQUE:    - indx, zintrp
!     LIBRARY:
!       W3LIB    - w3tagb, w3tage, errexit
!       NETCDF   - nf_open, nf_inq_varid, nf_get_var_real, nf_inq_var,
!                  nf_get_vara_real, nf_close, nf_strerror
!
!   EXIT STATES:
!     COND =   0 - SUCCESSFUL RUN
!     COND =  10 - NETCDF ERROR
!     COND =  11 - ERROR OPENING UNIT 11
!     COND =  12 - ERROR READING UNIT 11
!
! REMARKS AND IMPORTANT LOCAL VARIABLES:
!     Requires a WOA climatology file (WOA.nc) in netCDF format
!
! ATTRIBUTES:  (LIST MACHINES FOR WHICH CODE IS USED AND CHECKED OUT)
!
!   MACHINE:  IBM SP
!   LANGUAGE: F90
!
!
!$$$
      program editPrf
!
!   "buoy"  profiles have dtyp = TR or TA or BU
!   "bathy" profiles have dtyp = BA
!   "tesac" profiles have dtyp = TE
!
      include 'netcdf.inc'
!
      integer imx,jmx,kmx,tmx
      parameter (imx=360,jmx=180,kmx=33,tmx=14)
      parameter (mfll=14,nhst=20)
      integer ii,jj,kk,ncid,status
      integer ndims, nvars, natts, idunlm
      integer dsiz(20), vtype, nvdm, vdm(10), nvatts
      integer start(4), count(4)
      character*20 dname, vname, dbgs
!
      real xt(imx), yt(jmx), zt(kmx), temp(kmx), std(kmx)
      integer msk(imx,jmx)
!
      character csign*8,ukey*1,sid*2,dtyp*2,qkey*1,msign*8
      real, allocatable, dimension(:) :: pt, pz, wz, wt, tti, dtdz, wti, sdi, wsdi, dt
      real, allocatable, dimension(:) :: hst
      integer, allocatable, dimension(:) :: chst, ihdt, idtdz
      integer neg, pos
!     real pt(10), pz(10), tti(10), dt(10)
      logical noerr, ok, despk, spke, mooring, bathy, triton
      logical indocn, soocn, medi, safr, gom, etroppac, glfstrm
!
      call w3tagb('GODAS_EDITPRF',2005,0319,0319,'NP23')
!
! set some limits and some parameters
!
! threshold for deleting profiles with too few points; counter
!
      npmin = 3
      nnpm = 0
!
! counter for profiles with non-monotomic depths
!
      nzerr = 0
!
! threshold for deleting profiles with too deep a start; counter
!
      z1max = 36.0
      nz1err = 0
!
! threshold for deleting profiles with a large vertical gap; counter
!
      gpmax = 500.0
      nlgp = 0
!
! threshold gradient for removing surface spikes; counter
!
      t1smax = 0.01
      nt1spk = 0
!
! threshold gradient for removing small bottom spikes; minimum profile depth
!  for applying this fix; counter
!
      tbsmax = 0.06
      zbsmin = 500.0
      ntbspk = 0
!
! settings for removing spikes other than at surface or bottom; counter
!
      dtroff = 2.0
      dtsgn = -1.0
      ntfspk = 0
!
! thresholds for deleting a profile for gross differences; 
!  single point (mostly TRITON spikes at 250m only); rms; counter
!  (goffmax and goffdeep adjusted below according to type)
!
      goffmax = 4.0
      goffdeep = 4.0
      gdfm = 40.
      ngdiff = 0
!
! threshold for removing profiles with constant and deep offsets
!   (modified below according to profile type and location); counters
!
      ddtmx = 0.5
      ncoff = 0
      ndoff = 0
!
! parameters for identifying and deleting bad ~constant profiles; counter
!
      trmin = 0.05
      trlatn = 60.0
      trlats = -60.0
      trz = 100.0
      ncnst = 0
!
! parameters for identifying and truncating constant bottom sections of
!  profiles (appears limited to near-shore bathys); counter
!
      dtcnst = 0.12
      dzcnst = 150.0
      zcnst = 1000.0
      ndcnst = 0
!
! threshhold for rms of (t(k-1)-t(k)) for deleting noise profiles;
!   (modified below according to profile type and location); counter
!
      dtrmx = 1.75
      nnoise = 0
!
! counter for profiles deleted because not within bounds of model ocean
!
      nobnds = 0
!
! open existing netCDF ocean reference file
!
      status = nf_open('WOA.nc', 0, ncid)
      if (status .ne. NF_NOERR) go to 100
!
! get xt
!
      status = nf_inq_varid(ncid, 'grid_x', nid)
      if (status .ne. NF_NOERR) go to 100
      status = nf_get_var_real(ncid, nid, xt)
      if (status .ne. NF_NOERR) go to 100
!
! get yt
!
      status = nf_inq_varid(ncid, 'grid_y', nid)
      if (status .ne. NF_NOERR) go to 100
      status = nf_get_var_real(ncid, nid, yt)
      if (status .ne. NF_NOERR) go to 100
!
! get zt
!
      status = nf_inq_varid(ncid, 'grid_z', nid)
      if (status .ne. NF_NOERR) go to 100
      status = nf_get_var_real(ncid, nid, zt)
      if (status .ne. NF_NOERR) go to 100
!
! get mask
!
      status = nf_inq_varid(ncid, 'nlev', nid)
      if (status .ne. NF_NOERR) go to 100
      status = nf_get_var_int(ncid, nid, msk)
      if (status .ne. NF_NOERR) go to 100
!
! set fixed start / count
!
      count(1) = 1
      count(2) = 1
      start(3) = 1
      count(3) = kmx
      count(4) = 1
      msign = 'referPrf'
!
! open profile file and file for edited profiles
!
      open (11, form='unformatted', status='old', access='sequential', &
            &  err=110)
      open (51, form='unformatted', access='sequential')
!
! open file for reference profiles and file for obs - reference difference
!
!     open (80, status='scratch', form='unformatted', &
!             & access='sequential')
!    open (81, status='scratch', form='unformatted', &
!             & access='sequential')
      open (80, form='unformatted', access='sequential')
      open (81, form='unformatted', access='sequential')
!
! loop on profile file to count number of profiles
!
      nprf = 0
      npmx = 0
      do while (.true.)
        read (11, end=10, err=120) iyear,idate,csign, &
                               &  sid,dtyp,qkey,yp,xp,np
        nprf = nprf + 1
        if (np .gt. npmx) npmx = np
      end do
  10  continue
!
      allocate(pt(npmx))
      allocate(pz(npmx))
      allocate(wt(npmx))
      allocate(wz(npmx))
      allocate(tti(npmx))
      allocate(wti(npmx))
      allocate(sdi(npmx))
      allocate(wsdi(npmx))
      allocate(dt(npmx))
      allocate(dtdz(npmx))
      allocate(idtdz(npmx))
!
      allocate(ihdt(npmx))
      allocate(hst(nhst))
      allocate(chst(nhst))
!
      rewind 11
!
!
! begin loop on profile file
!
      npout = 0
      profile: do iprf=1,nprf
        read (11, err=120) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                           &  np,(pz(k),pt(k),k=1,np)
!
        if (np .lt. npmin) then
          nnpm = nnpm + 1
          cycle profile
        end if
!
        npi = np
        mon = idate / 1000000
        deep = 800.0
!
        noerr = .true.
!
! identify certain special areas
!
! TRITON mooring zone
!
        if (yp .gt. -9.0 .and. yp .lt. 9.0 .and. xp .gt. 125.0 .and. xp .lt. 160.0) then
          triton = .true.
        else
          triton = .false.
        end if
!
! eastern tropical pacific
!
!       if (yp .gt. -10.75 .and. yp .lt. 10.75 .and. xp .gt. 210.0 .and. xp .lt. 280.0) then
        if (yp .gt. -10.75 .and. yp .lt. 10.75 .and. xp .gt. 190.0 .and. xp .lt. 280.0) then
          etroppac = .true.
        else
          etroppac = .false.
        end if
!
! Gulf Stream
!
        if (yp .gt. 30.0 .and. yp .lt. 41.5 .and. xp .gt. 280.0 .and. xp .lt. 293.0) then
          glfstrm = .true.
        else if (yp .gt. 36.0 .and. yp .lt. 45.0 .and. xp .gt. 290.0 .and. xp .lt. 310) then
          glfstrm = .true.
        else
          glfstrm = .false.
        end if
!
! Indian Ocean
!
        if (yp .gt. -40.0 .and. yp .lt. 30.0 .and. xp .gt. 20.0 .and. xp .lt. 116.0) then
          indocn = .true.
        else
          indocn = .false.
        end if
!
! Southern Ocean
!
        if (yp .le. -40.0) then
          soocn = .true.
        else
          soocn = .false.
        end if
!
! Mediterranean outflow
!
        if (yp .gt. 26.0 .and. yp .lt. 48.0 .and. xp .gt. 320.0 .and. xp .lt. 355.0) then
          medi = .true.
        else
          medi = .false.
        end if
!
! southeast of South Africa
!
        if (yp .gt. -42.0 .and. yp .lt. -24.0 .and. xp .gt. 25.0 .and. xp .lt. 60.0) then
          safr = .true.
        else
          safr = .false.
        end if
!
! Gulf of Mexico
!
        if (yp .gt. 21.0 .and. yp .lt. 31.0 .and. xp .gt. 260.0 .and. xp .lt. 279.0) then
          gom = .true.
        else
          gom = .false.
        end if
!
! set parameters for different types
!
! moorings (TAO, TRITON, PIRATA, RAMA)
!
        if (dtyp .eq. 'TA' .or. dtyp .eq. 'TR' .or. dtyp .eq. 'BU') then
          mooring = .true.
        else
          mooring = .false.
        end if
!
! bathys (XBTs)
!
        if (dtyp .eq. 'BA') then
          bathy = .true.
        else
          bathy = .false.
        end if
!
! set threshhold for offsets, noise rms and single point gross error
!
        if (bathy) then
          deep = 800.0
          goffmax = 7.0
          goffdeep = 6.0
          if (indocn .or. soocn) then
            ddtmx = 1.0
            dtrmx = 1.5
          else if (glfstrm) then
            ddtmx = 4.0
            dtrmx = 2.5
            goffmax = 8.0
            goffdeep = 8.0
          else if (etroppac) then
            goffmax = 11.0
            goffdeep = 10.2
          else
!           ddtmx = 1.0
            ddtmx = 1.7
            dtrmx = 2.2
          end if
        else
          ddtmx = 0.75
          dtrmx = 1.75
          if (mooring .or. etroppac) then
            if (triton) then
              goffmax = 7.0
              goffdeep = 7.0
              goff250 = 3.5
            else
              goffmax = 11.0
              goffdeep = 10.2
            endif
          else if (glfstrm) then
            ddtmx = 4.0
            dtrmx = 2.5
            goffmax = 9.0
            goffdeep = 9.0
          else if (indocn .or. soocn) then
            goffmax = 9.5
            goffdeep = 3.0
          else
            goffmax = 9.0
            goffdeep = 3.0
          end if
        end if
!
! treat moorings separately
!
        if (mooring) then
!
! check for non-monotonic depths
!
          if (noerr) then
            do k=2,np
              if (pz(k) .le. pz(k-1)) then
                noerr = .false.
                nzerr = nzerr + 1
                exit
              end if
            end do
          end if
!
! get 'temp' profile from ocean file
!
          ii = indx(xp,xt,imx,.true.)
          jj = indx(yp,yt,jmx,.false.)
!
          if (noerr .and. ii .gt. 0 .and. jj .gt. 0) then
            status = nf_inq_varid(ncid, 'temp', nid)
            if (status .ne. NF_NOERR) go to 100
            status = nf_inq_var(ncid,nid,vname,vtype,nvdm,vdm,nvatts)
            if (status .ne. NF_NOERR) go to 100
            start(1) = ii
            start(2) = jj
            start(4) = mon
            status = nf_get_vara_real(ncid, nid, start, count, temp)
            if (status .ne. NF_NOERR) go to 100
            status = nf_inq_varid(ncid, 'tstd', nid)
            if (status .ne. NF_NOERR) go to 100
            start(4) = mfll
            status = nf_get_vara_real(ncid, nid, start, count, std)
            if (status .ne. NF_NOERR) go to 100
!
            kk = msk(ii,jj)
!
            if (kk .gt. 0) then
              call zintrp(temp,zt,kk,tti,pz,np,npi)
              call zintrp(std,zt,kk,sdi,pz,np,npi)
              rms = 0.0
              dtoff = 0.0
              if (triton) then
                dt250 = 0.0
                do k=1,npi
                  dt(k) = pt(k) - tti(k)
                  rms = rms + dt(k)**2
                  if (abs(pz(k)-250.0) .lt. 5.0) then
                    if (abs(dt(k)) .gt. dt250) dt250 = abs(dt(k))
                  else
                    if (abs(dt(k)) .gt. dtoff) dtoff = abs(dt(k))
                  end if
                end do
              else
                do k=1,npi
                  dt(k) = pt(k) - tti(k)
                  rms = rms + dt(k)**2
                  if (abs(dt(k)) .gt. dtoff) dtoff = abs(dt(k))
                end do
              end if
              rms = sqrt(rms/npi)
!
! check for large offsets (this is to catch TRITON spikes at 250m)
!
              if (triton .and. dt250 .gt. goff250) then
                noerr = .false.
                ngdiff = ngdiff + 1
              end if
!
! check for other large offsets 
!
              if (dtoff .gt. goffmax) then
                noerr = .false.
                ngdiff = ngdiff + 1
              end if
!
! check for large rms difference
!
              if (rms .gt. gdfm) then
                noerr = .false.
                ngdiff = ngdiff + 1
              end if
!
            else
              noerr = .false.
              nobnds = nobnds + 1
            end if
!
          else
            noerr = .false.
            nobnds = nobnds + 1
          end if
!
! begin qc of non-mooring profiles (Argo, XBT)
!
        else
!
! check for non-monotonic depths
!
          if (noerr) then
            k = 2
            do while (k .le. np)
              if (pz(k) .le. pz(k-1)) then
                if (k .eq. np) then
                  np = np - 1
                else
!                 if (pz(k-1) .lt. pz(k+1)) then
                  if (pz(k) .eq. pz(k-1)) then
                    np = np - 1
                    do kk=k,np
                      pz(kk) = pz(kk+1)
                      pt(kk) = pt(kk+1)
                    end do
                  else
                    noerr = .false.
                    nzerr = nzerr + 1
                    exit
                  end if
                end if
              end if
              k = k + 1
            end do
          end if
!
! check for deep start
!
          if (noerr) then
            if (pz(1) .gt. z1max) then
              noerr = .false.
              nz1err = nz1err + 1
            end if
          end if
!
! check for large gaps
!
          if (noerr) then
            do k=2,np
              if (pz(k) - pz(k-1) .gt. gpmax) then
                np = k - 1
                nlgp = nlgp + 1
                exit
              end if
            end do
          end if
!
! check for unrealistic near-constant profiles
!
          if (noerr .and. yp .gt. trlats .and. yp .lt. trlatn .and. pz(np) .gt. trz) then
            tmin = pt(1)
            tmax = pt(1)
            do k=2,np
              if (pt(k) .lt. tmin) tmin = pt(k)
              if (pt(k) .gt. tmax) tmax = pt(k)
            end do
            if (tmax-tmin .le. trmin) then
              noerr = .false.
              ncnst = ncnst + 1
            end if
          end if
!
! fix surface spike, type 1
!
!         if (noerr .and. np .gt. 1) then
!           if (abs((pt(1)-pt(2))/(pz(2)-pz(1))) .gt. t1smax) then
!             pt(1) = pt(2)
!             nt1spk = nt1spk + 1
!           end if
!         end if
!
! get 'temp' profile from ocean file
!
          ii = indx(xp,xt,imx,.true.)
          jj = indx(yp,yt,jmx,.false.)
!
! if initial location in the reference field is a land point, try
!   to find a nearby ocean point
!
          if (msk(ii,jj) .eq. 0) then
            ih = ii
            ii = ii + 1
            if (ii .gt. imx) ii = 1
            if (msk(ii,jj) .eq. 0) then
              ii = ih - 1
              if (ii .lt. 1) ii = imx - 1
              if (msk(ii,jj) .eq. 0) then
                ii = ih
                jh = jj
                jj = jj + 1
                if (jj .gt. jmx) jj = jmx
                if (msk(ii,jj) .eq. 0) then
                  jj = jh - 1
                  if (jj .lt. 1) jj = 1
                  if (msk(ii,jj) .eq. 0) then
                    jj = jh
                  end if
                end if
              end if
            end if
          end if
!
          if (noerr .and. ii .gt. 0 .and. jj .gt. 0) then
            status = nf_inq_varid(ncid, 'temp', nid)
            if (status .ne. NF_NOERR) go to 100
            status = nf_inq_var(ncid,nid,vname,vtype,nvdm,vdm,nvatts)
            if (status .ne. NF_NOERR) go to 100
            start(1) = ii
            start(2) = jj
            start(4) = mon
            status = nf_get_vara_real(ncid, nid, start, count, temp)
            if (status .ne. NF_NOERR) go to 100
            status = nf_inq_varid(ncid, 'tstd', nid)
            if (status .ne. NF_NOERR) go to 100
            start(4) = mfll
            status = nf_get_vara_real(ncid, nid, start, count, std)
            if (status .ne. NF_NOERR) go to 100
!
            kk = msk(ii,jj)
            if (kk .gt. 0) then
              call zintrp(temp,zt,kk,tti,pz,np,npi)
              call zintrp(std,zt,kk,sdi,pz,np,npi)
              np = npi
!
      mm = idate/1000000
      id = (idate - mm*1000000)/10000
      write(dbgs,'(i2.2,1x,i2.2,1x,f6.2)') mm, id, yp
!
! truncate very large bottom errors (essentially a problem with XBTs)
!
              kw = 0
              del = 0
              do k=1,npi
                 dpt = pt(k-1)-pt(k)
                if (pz(k) .lt. 150.0 .or. &
                        &  (abs(dpt) .lt. abs(tti(k-1)-tti(k))+dtroff .and. dpt > dtsgn)) then
                    kw = kw + 1
                    wz(kw) = pz(k)
                    wt(kw) = pt(k)
                    wti(kw) = tti(k)
                    wsdi(kw) = sdi(k)
                else
                  exit
                end if
              end do
              if (kw < npi) then
                ntfspk = ntfspk + 1
                do k=1,kw
                  pz(k) = wz(k)
                  pt(k) = wt(k)
                  tti(k) = wti(k)
                  sdi(k) = wsdi(k)
                  dt(k) = abs(pt(k) - tti(k))
                end do
                npi = kw
                np = kw
              end if
!
! truncate vertically constant temperature bottom sections of nearshore profiles
!   rule out TAO etc. and Argo which are never near shore
!
              if (bathy .and. pz(npi) .le. zcnst) then
                kw = npi
                do k=npi-1,1,-1
                  if (pt(k)-pt(npi) > dtcnst) then
                    if (pz(npi)-pz(k) > dzcnst) kw = k
                    exit
                  end if
                end do
                if (npi .ne. kw) then
                  ndcnst = ndcnst + 1
                  npi = kw
                  kw = 1
                  knt = 0
                  do k=2,npi
                    if (pt(k) .le. pt(kw)) then
                      kw = k
                    else
                      knt = knt + 1
                      if (knt .ge. 3) exit
                    end if
                  end do
                  npi = kw
                  np = kw
                end if
              end if
!
! if the root sum of squares of the delta-T of sequential depths is large, 
!           the profile may be noisy
!
              dts = 0.0
              nn = 0
              do k = 2,npi
                dt2 = (pt(k-1) - pt(k))**2
                if (dt2 .gt. 0.0) then
                  dts = dts + (pt(k-1) - pt(k))**2
                  nn = nn + 1
                end if
              end do
              if (nn .gt. 0) then
                dts = sqrt(dts/float(npi-1))
!
                if (dts .gt. dtrmx) then
                  noerr = .false.
                  nnoise = nnoise + 1
                end if
!                if (bathy) then
!                  if (indocn) then
!                    if (dts .gt. 1.5) then
!                      noerr = .false.
!                      nnoise = nnoise + 1
!                    end if
!                  else
!                    if (dts .gt. 2.2) then
!                      noerr = .false.
!                      nnoise = nnoise + 1
!                    end if
!                  end if
!                else
!                  if (dts .gt. 1.75) then
!                    noerr = .false.
!                    nnoise = nnoise + 1
!                  end if
!                end if
              end if
!
! find max difference with reference profile and vertical gradient of reference profile
!
              if (noerr) then
                dtoff = 0.0
                do k=1,npi
                  if (k .eq. 1) then
                    dtdz(k) = abs((tti(2)-tti(1))/(pz(2)-pz(1)))
                  else if (k .eq. npi) then
                    dtdz(k) = abs((tti(npi)-tti(npi-1))/(pz(npi)-pz(npi-1)))
                  else
                    dtdz(k) = abs((tti(k+1)-tti(k-1))/(pz(k+1)-pz(k-1)))
                  end if
                  dt(k) = abs(pt(k) - tti(k))
                  if (dt(k) .gt. dtoff) dtoff = dt(k)
                end do
!
! check for large single point offsets
!
                if (dtoff .gt. goffmax) then
                  noerr = .false.
                  ngdiff = ngdiff + 1
                end if
              end if
!
! compute a gradient index for the reference profile
!
              if (noerr) then
                dtdzmx = 0.0
                do k=1,npi
                  if (dtdz(k) .gt. dtdzmx) then
                    dtdzmx = dtdz(k)
                    ktc = k
                  end if
                end do
                if (dtdzmx .gt. 0.0) then
                  do k=1,npi
                    idtdz(k) = int(20.0*dtdz(k)/dtdzmx)
                  end do
                else
                  do k=1,npi
                    idtdz(k) = 0
                  end do
                end if
!
! construct histo of obs profile - reference profile difference
!  nhdt is the bin number through which 90% of the del-T's are accounted for
!  nspk id the bin number beyond which del-T's may be spikes
!
                dhst = dtoff / (nhst-1)
                hst(1) = -0.01
                chst(1) = 0
                do n=2,nhst-1
                  hst(n) = (n-1)*dhst
                  chst(n) = 0
                end do
                hst(nhst) = dtoff+0.01
                do k=1,npi
                  do n=2,nhst
                    if (dt(k) .ge. hst(n-1) .and. dt(k) .lt. hst(n)) then
                      chst(n-1) = chst(n-1) + 1
                      ihdt(k) = n-1
                      exit
                    end if
                  end do
                end do
                nn = 0
                nhdt=nhst
                do n=1,nhst
                  nn = nn+chst(n)
                  if (float(nn)/float(npi) .gt. 0.9) then
                    nhdt = n
                    exit
                  end if
                end do
                nn = 0
                nspk = nhst - 1
                do n = nhdt+1,nhst
                  if (chst(n) .eq. 0) then
                    nn = nn + 1
                  else
                    if (nn .gt. 0) nn = nn - 1
                  end if
                  if (nn .ge. 2) then
                    nspk = n + 1
                    exit
                  end if
                end do
!
! if largest histo values occur at depth, the profile may have a deep offset.
!   Likely to be an issue for XBTs. Argo profiles are smoother to start with 
!   and may have a broadly distributed histo
!  exempt Gulf of Mexico because of Loop Current
!  exempt Gulf Stream
!
                if (bathy .and. .not.gom .and. .not.glfstrm .and. nhdt .ge. nhst/2) then
                  if (pz(npi) .ge. 300 .and. npi .gt. 20) then
                    ks = npi - 10
                    nn = 0
                    do k=ks,npi
                      if (ihdt(k) .ge. 3*nhst/4) nn = nn+1
                    end do
                    if (nn .ge. 8) then
                      noerr = .false.
                      ndoff = ndoff + 1
                    end if
                  end if
                end if
!
                if (noerr) then
!
!  use the histo to identify profile outliers (spikes) and remove them, taking into account
!    the depth of the thermocline (max gradient at k=ktc and within lower thermocline 
!    idtdz(k) <= 6) and which bins of the histo represent spikes (k > nspk)
!
        np = npi
                  k = 1
                  do while (k .le. npi)
                    ok = .false.
                    if (ihdt(k) .le. nhdt) ok = .true.
                    if (k .gt. 3) then
                      ihdtav = (ihdt(k-1)+ihdt(k-2)+ihdt(k-3)) / 3
                      if (ihdt(k) .gt. 8*ihdtav) ok = .false.
                    end if
                    if (k .le. ktc .or. idtdz(k) .gt. 6) ok = .true.
                    if (ok) then
                      k = k + 1
                    else
                      npi = npi - 1
                      do kw = k,npi
                        pz(kw) = pz(kw+1)
                        pt(kw) = pt(kw+1)
                        tti(kw) = tti(kw+1)
                        sdi(kw) = sdi(kw+1)
                        dt(kw) = dt(kw+1)
                        ihdt(kw) = ihdt(kw+1)
                        idtdz(kw) = idtdz(kw+1)
                      end do
                    end if
                  end do
                  if (npi < np) then
                    ntfspk = ntfspk + 1
                    do k=1,npi
                    end do
                    np = npi
                  end if
!
!   fix small bottom spikes that might escape the histo test
!
                  spke = .false.
                  do while (pz(npi) .gt. zbsmin) 
                    if (abs((pt(npi-1)-pt(npi))/(pz(npi)-pz(npi-1)))  &
                                              &     .gt. tbsmax) then
                      npi = npi - 1
                      spke = .true.
                    else
                      exit
                    end if
                  end do
                  if (spke) ntbspk = ntbspk + 1
                  np = npi
!
!   delete profiles displaying a constant offset from the reference
!   exempt the Gulf of Mexico because of Loop Current
!
                  if (.not.gom) then
                    neg = 0
                    pos = 0
                    smt = 0.0
                    smz = 0.0
                    dt(1) = pt(1) - tti(1)
                    if (dt(1) .lt. 0.0) neg = neg + 1
                    if (dt(1) .gt. 0.0) pos = pos + 1
                    npd = 0
                    do k=2,npi
                      if (npd .eq. 0 .and. pz(k) .ge. deep) npd = k
                      dt(k) = pt(k) - tti(k)
                      if (dt(k) .lt. 0.0) neg = neg + 1
                      if (dt(k) .gt. 0.0) pos = pos + 1
                      smt = smt + 0.5*(dt(k)+dt(k-1)) * (pz(k)-pz(k-1))
                      smz = smz + (pz(k)-pz(k-1))
                    end do
                    if (smz .gt. 0.0) then
                      smt = smt / smz
                    else
                      smt = 0.0
                    end if 
                    if (((neg .ne. 0 .and. pos .eq. 0) .or.   &
                                 & (pos .ne. 0 .and. neg .eq. 0)) .and.   &
                                        & abs(smt) .gt. ddtmx) then
                      if (npd .gt. 0 .and. npi-npd .gt. 5) then
                        neg = 0
                        pos = 0
                        smt = 0.0
                        smsd = 0.0
                        smz = 0.0
                        do k=npd,npi
                          dt(k) = pt(k) - tti(k)
                          if (dt(k) .lt. 0.0) neg = neg + 1
                          if (dt(k) .gt. 0.0) pos = pos + 1
                          smt = smt + 0.5*(dt(k)+dt(k-1)) * (pz(k)-pz(k-1))
                          smsd = smsd + 0.5*(sdi(k)+sdi(k-1)) * (pz(k)-pz(k-1))
                          smz = smz + (pz(k)-pz(k-1))
                        end do
                        if (smz .gt. 0.0) then
                          smt = smt / smz
                          smsd = smsd / smz
                        else
                          smt = 0.0
                          smsd = 0.1
                        end if
                        if (((neg .ne. 0 .and. pos .eq. 0) .or.   &
                                     & (pos .ne. 0 .and. neg .eq. 0)) .and.   &
                                            & abs(smt) .gt. ddtmx) then

                          noerr = .false.
                          ncoff = ncoff + 1
!       write(6,'(a,2f9.2)') csign(1:8),yp,xp
                        end if
                      else
                        noerr = .false.
                        ncoff = ncoff + 1
!       write(6,'(a,2f9.2)') csign(1:8),yp,xp
                      end if
                    end if
                  end if
                end if
!
! another approach for deleting deep offsets taking into account the depth of the
!    thermocline (max gradient at k=ktc and within lower thermocline idtdz(k) <= 6)
!    excluding part of N.Atlantic to avoid rejecting profiles influenced by "Meddies" and
!     excluding part of S.Indian to avoid rejecting profiles influenced by complex 
!     circulation.  excluding also Gulf Stream and Loop Current region of Gulf of Mexico
!     in these cases there can be legitimate offsets below 1000 meters.
!
                if (noerr .and. npi-ktc .gt. 5 .and. .not.medi .and. .not.safr .and. .not.gom .and. .not.glfstrm) then
                  sgn = sign(1.0,dt(npi))
                  nn = 0
                  smt = 0.0
                  smsd = 0.0
                  smz = 0.0
                  do k=npi-1,ktc,-1
                    if (sign(1.0,dt(k)) .eq. sgn .and. pz(k) .ge. deep) then
                      smt = smt + 0.5*(dt(k)+dt(k+1)) * (pz(k+1)-pz(k))
                      smsd = smsd + 0.5*(sdi(k)+sdi(k+1)) * (pz(k+1)-pz(k))
                      smz = smz + (pz(k+1)-pz(k))
                      nn = nn + 1
                    else
                      exit
                    end if
                  end do
                  if (nn .ge. 5 .or. pz(npi)-deep .ge. 300.0) then
                    smt = smt / smz
                    smsd = smsd / smz
                    if (abs(smt) .gt. ddtmx) then
                      noerr = .false.
                      ndoff = ndoff + 1
                    end if
                  end if
                end if
!
! a last check for a gross deep offset
!
                if (noerr .and. bathy .and. npi .gt. ktc .and. .not.medi .and. .not.safr .and. .not.gom .and. .not.glfstrm) then
                  dtmx = 0.0
                  do k = ktc+1,npi
                    if (abs(dt(k)) .gt. dtmx) dtmx = abs(dt(k))
                  end do
                  if (dtmx .gt. goffdeep) then
                    noerr = .false.
                    ngdiff = ngdiff + 1
                  end if
                end if
              end if
!
            else
              noerr = .false.
              nobnds = nobnds + 1
            end if
!
          else
            noerr = .false.
            nobnds = nobnds + 1
          end if
!
        end if
!
! write edited, reference and difference profiles
!
        if (noerr) then
          npout = npout + 1
!
          write (51) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                           &  np,(pz(k),pt(k),k=1,np)
!
          write (80) iyear,idate,msign,sid,dtyp,qkey,yt(jj), &
                      & xt(ii),npi,(pz(k),tti(k),k=1,npi)
!
          write (81) iyear,idate,csign,sid,dtyp,qkey,yt(jj), &
                       & xt(ii),npi,(pz(k),dt(k),k=1,npi)
!
      else              ! DBG
        write (85) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                           &  np,(pz(k),pt(k),k=1,np)
        write (86) iyear,idate,msign,sid,dtyp,qkey,yp,xp, &
                           &  npi,(pz(k),tti(k),k=1,npi)
        write (87) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                           &  npi,(pz(k),dt(k),k=1,npi)
        end if
!
      end do profile
!
      status = nf_close(ncid)
      close (11)
      close (51)
      close (80)
      close (81)
!
      write(6,'(a)') 'Results of editing profiles'
      write(6,'(i5,a)') nprf, ' profiles read'
      write(6,'(i5,a)') npout, ' profiles retained'
      write(6,'(i5,a,i2)') nnpm, ' deleted for points < ', npmin
      write(6,'(i5,a)') nzerr, ' deleted for non-mono depths'
      write(6,'(i5,a,f3.0)') nz1err, ' deleted for deep z1 > ', z1max
      write(6,'(i5,a)') nobnds, ' deleted as out of bounds'
      write(6,'(i5,a)') ngdiff, ' deleted for gross differences'
      write(6,'(i5,a,f5.2)') ncnst, ' deleted for profile range < ', trmin
      write(6,'(i5,a)') ncoff, ' deleted for constant offset'
      write(6,'(i5,a)') ndoff, ' deleted for deep offset'
      write(6,'(i5,a)') nnoise, ' deleted as noisy'
      write(6,'(i5,a)') ndcnst, ' truncated deep constant section'
      write(6,'(i5,a,f4.0)') nlgp, ' truncated for large gap > ', gpmax
      write(6,'(i5,a)') nt1spk, ' surface spikes fixed'
      write(6,'(i5,a)') ntbspk, ' bottom spikes fixed'
      write(6,'(i5,a)') ntfspk, ' profiles had other spikes fixed'
      write(6,*)
!
      call w3tage('GODAS_EDITPRF')
      call errexit(0)
!
  100 continue
      write(6,'(a)') 'Error reading time-mean netCDF file'
      write(6,'(a)') nf_strerror(status)
      call w3tage('GODAS_EDITPRF')
      call errexit(10)
!
  110 write(6,'(a)') 'Error opening profile file on unit 11'
      call w3tage('GODAS_EDITPRF')
      call errexit(11)
!
  120 write(6,'(a)') 'Error reading profile file on unit 11'
      call w3tage('GODAS_EDITPRF')
      call errexit(12)
!
      end program editPrf

! -------------------------------------------------------------------

      integer function indx(p, pt, nmx, cyclic)
      real p, pt(*)
      integer nmx
      logical cyclic
!
      if (p .ge. pt(1) .and. p .le. pt(nmx)) then
        do n=2,nmx
          if (p .ge. pt(n-1) .and. p .le. pt(n)) then
            if (p-pt(n-1) .lt.  pt(n)-p) then
              indx = n-1
            else
              indx = n
            end if
          end if
        end do
      else
        if (cyclic) then
          indx = 1
        else
          indx = -1
        end if
      end if
!
      end function indx
!
! -------------------------------------------------------------------
!
      subroutine zintrp(ta,za,na,tb,zb,nb,nbi)
      integer na, nb
      real ta(*), za(*), tb(*), zb(*)
!
      nbi = -1
      do k=1,nb
        if (zb(k) .le. za(1)) then
          tb(k) = ta(1)
          nbi = k
        else if (zb(k) .gt. za(na)) then
          exit
        else
          do ka=2,na
            if (zb(k) .gt. za(ka-1) .and. zb(k) .le. za(ka)) then
              dzm = (zb(k) - za(ka-1)) / (za(ka) - za(ka-1))
              dzp = (za(ka) - zb(k)) / (za(ka) - za(ka-1))
              tb(k) = ta(ka-1)*dzp + ta(ka)*dzm
              nbi = k
              exit
            end if
          end do
        end if
      end do
!
      end subroutine zintrp
