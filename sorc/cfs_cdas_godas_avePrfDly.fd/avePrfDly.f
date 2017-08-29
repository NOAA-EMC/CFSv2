!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM:  AVEPRFDLY
!   PRGMMR: David W. Behringer  ORG: NP23        DATE: 2003-07-24
!
! ABSTRACT:  Perform quality control on subsurface temperature data
!   for use in Global Ocean Data Assimilation System
!
! PROGRAM HISTORY LOG:
! 2003-07-24  David W. Behringer
! 2004-01-30  David W. Behringer - bug fix in routine merge_files
! 2005-11-01  David W. Behringer - modify to perform daily average when
!             a platform reports more than 2.5x during one day and moves
!             less than 0.2 degrees between reports (previously only 
!             TRITON platforms were averaged).  To save time Argo
!             floats are assumed to report much less frequently and
!             are not checked for this condition.
!
! USAGE:
!   INPUT FILES:
!     UNIT 11  - TEMPERATURE PROFILE DATA IN IEEE
!
!   OUTPUT FILES:
!     UNIT 51  - DAILY AVERAGED MOORING  PROFILE DATA IN IEEE
!     UNIT 06  - UNIT 6 (STANDARD PRINTFILE)
!
!   WORK FILES:  (INCLUDING SCRATCH FILES)
!     UNIT 80  - SCRATCH FILE FOR PROFILE DATA
!     UNIT 81  - SCRATCH FILE FOR PROFILE DATA
!     UNIT 82  - SCRATCH FILE FOR PROFILE DATA
!     UNIT 83  - SCRATCH FILE FOR PROFILE DATA
!     UNIT 84  - SCRATCH FILE FOR PROFILE DATA
!
!   SUBPROGRAMS CALLED FROM PROGRAM: (LIST ALL CALLED FROM ANYWHERE IN CODES)
!     UNIQUE:    - chk_prof_dst, daily_ave, daily_ave_zaf, merge_files, zintrp
!     LIBRARY:
!       W3LIB    - w3tagb, w3tage, errexit, iw3jdn
!
!   SUBPROGRAMS CALLED FROM MAIN: (LIST ALL CALLED FROM MAIN)
!     UNIQUE:    - chk_prof_dst, daily_ave, daily_ave_zaf, merge_files
!     LIBRARY:
!       W3LIB    - w3tagb, w3tage, errexit
!
!   EXIT STATES:
!     COND =   0 - SUCCESSFUL RUN
!     COND =  11 - ERROR OPENING UNIT 11
!     COND =  12 - ERROR READING UNIT 11
!     COND =  51 - ERROR OPENING UNIT 51
!
! REMARKS AND IMPORTANT LOCAL VARIABLES:
!     None
!
! ATTRIBUTES:  (LIST MACHINES FOR WHICH CODE IS USED AND CHECKED OUT)
!
!   MACHINE:  IBM SP
!   LANGUAGE: F90
!
!
!$$$
!
      program avePrfDly
!
!  avePrfDly examines each platform individually, determining its
!  frequency of reporting and average distance between profiles.
!  If the number of profiles per day exceeds "tpmx" and the average 
!  distance between profiles is less than "aspmn" then it will make
!  daily averages of the profiles from that platform.
!
      character csign*8,tsign*8,sid*2,dtyp*2,qkey*1
      character cmd*80, str*80, dbgs*25
      real, allocatable, dimension(:) :: pt,pz,za,ta
      integer, allocatable, dimension(:) :: na
!
      character csign1*8,sid1*2,dtyp1*2,qkey1*1
      real, allocatable, dimension(:) :: t1, z1
      character csign2*8,sid2*2,dtyp2*2,qkey2*1
      real, allocatable, dimension(:) :: t2, z2
!
      parameter (nfx=44)
      integer, dimension(nfx) :: naf
      real, dimension(nfx) :: zaf, taf, wti
      data zaf /0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, &
             & 70, 80, 90, 100, 110, 125, 150, 175, 200, 250, &
             & 300, 400, 500, 600, 700, 800, 900, 1000, 1100, &
             & 1200, 1300, 1400, 1500, 1750, 2000, 2500, 3000, &
             & 3500, 4000, 4500, 5000, 5500/
!
      logical dense, mooring
      logical, dimension(50) :: avd
      integer ndy
!
      call w3tagb('GODAS_AVEPRFDLY',2005,0319,0319,'NP23')
!
      eps = 1.0
      tpmx = 2.5
      aspmn = 0.05
!
! open input profile file
!
      open (11, form='unformatted', status='old', &
                            & access='sequential', err=110)
!
      nprf = 0
      npmx = 8000
      do while (.true.)
        read (11, end=100, err=120) iyear,idate,csign,sid, &
                                       &  dtyp,qkey,yp,xp,np
        nprf = nprf + 1
        if (np .gt. npmx) npmx = np
      end do
  100 continue
!
      rewind (11)
!
      allocate(pt(npmx))
      allocate(pz(npmx))
      allocate(ta(npmx))
      allocate(za(npmx))
      allocate(na(npmx))
      allocate(t1(npmx))
      allocate(z1(npmx))
      allocate(t2(npmx))
      allocate(z2(npmx))
!
! open 5 scratch files
!
      open (80, form='unformatted', access='sequential')
      open (81, form='unformatted', access='sequential')
      open (82, form='unformatted', access='sequential')
      open (83, form='unformatted', access='sequential')
      open (84, form='unformatted', access='sequential')
!
! copy Argo profiles to scratch file 80, all others to 81
!
      nplt = 0
      nqplt = 0
      do n=1,nprf
        read (11, err=120) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                           &  np,(pz(k),pt(k),k=1,np)
        if (csign(1:1) .eq. 'Q') then
          write (80) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                             &  np,(pz(k),pt(k),k=1,np)
          nqplt = nqplt + 1
        else
          write (81) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                             &  np,(pz(k),pt(k),k=1,np)
          nplt = nplt + 1
        end if
      end do
!
      close (11)

      rewind(80)
      rewind(81)
      close(80); open (80,form='unformatted', access='sequential')
      close(81); open (81,form='unformatted', access='sequential')
!
! begin separating individual platforms (non-Argo)
!
      npltF = 0
      do while (nplt .ne. 0)
!
! start a scratch file for single platform
!
        read (81, end=200) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                           &  np,(pz(k),pt(k),k=1,np)
        write (82) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                           & np,(pz(k),pt(k),k=1,np)
        npltF = npltF + 1
        tsign = csign
!
! begin loop on profile file
!
        nplt = 0
        do while (.true.)
          read (81, end=200,err=200) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                            & np,(pz(k),pt(k),k=1,np)
          if (csign .eq. tsign) then
            write (82) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                            & np,(pz(k),pt(k),k=1,np)
          else
            write (83) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                            & np,(pz(k),pt(k),k=1,np)
            nplt = nplt + 1
          end if
        end do
  200   continue
!
        rewind (81)
        rewind (82)
        rewind (83)
        close(81); open (81,form='unformatted', access='sequential')
        close(82); open (82,form='unformatted', access='sequential')
        close(83); open (83,form='unformatted', access='sequential')
!
        if (nplt .ne. 0) then
          do n=1,nplt
            read (83) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                            & np,(pz(k),pt(k),k=1,np)
            write (81) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                            & np,(pz(k),pt(k),k=1,np)
          end do
          rewind (81)
          rewind (83)
          close(81); open (81,form='unformatted', access='sequential')
          close(83); open (83,form='unformatted', access='sequential')
        end if
!
        call chk_prof_dst
!
        if (dense) then
          if (mooring) then
            call daily_ave
          else
            call daily_ave_zaf
          end if
        else
          do while (.true.)
            read (82,end=300) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                            & np,(pz(k),pt(k),k=1,np)
            write (83) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                            & np,(pz(k),pt(k),k=1,np)
          end do
  300     continue
          rewind (82)
          rewind (83)
          close(82); open (82,form='unformatted', access='sequential')
          close(83); open (83,form='unformatted', access='sequential')
        end if
!
        if (npltF .eq. 1) then
          do while (.true.)
            read (83,end=400) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                            & np,(pz(k),pt(k),k=1,np)
            write (84) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                            & np,(pz(k),pt(k),k=1,np)
          end do
  400     continue
          rewind (83)
          rewind (84)
          close(83); open (83,form='unformatted', access='sequential')
          close(84); open (84,form='unformatted', access='sequential')
        else
          call merge_files(83,84,82)
          do while (.true.)
            read (82,end=500) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                            & np,(pz(k),pt(k),k=1,np)
            write (84) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                            & np,(pz(k),pt(k),k=1,np)
          end do
  500     continue
          rewind (82)
          rewind (84)
          close(82); open (82,form='unformatted', access='sequential')
          close(84); open (84,form='unformatted', access='sequential')
        end if
!
      end do
!
! open output profile file
!
      open (51, form='unformatted', access='sequential', err=510)
!
      if (nqplt .ne. 0) then
        call merge_files(80,84,51)
      else
        do while (.true.)
          read (84,end=600) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                          & np,(pz(k),pt(k),k=1,np)
          write (51) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                          & np,(pz(k),pt(k),k=1,np)
        end do
  600   continue
      end if
!
      close (51)
      close (80)
      close (81)
      close (82)
      close (83)
      close (84)
!
      call w3tage('GODAS_AVEPRFDLY')
      call errexit(0)
!
  110 write(6,'(a)') 'Error opening profile file on unit 11'
      call w3tage('GODAS_AVEPRFDLY')
      call errexit(11)
!
  120 write(6,'(a)') 'Error reading profile file on unit 11'
      call w3tage('GODAS_AVEPRFDLY')
      call errexit(12)
!
  510 write(6,'(a)') 'Error opening profile file on unit 51'
      call w3tage('GODAS_AVEPRFDLY')
      call errexit(51)
!
  520 write(6,'(a)') 'Error writing profile file on unit 51'
      call w3tage('GODAS_AVEPRFDLY')
      call errexit(52)
!
      contains
!
! -------------------------------------------------------------- 
!
      subroutine chk_prof_dst
!
      real(8) :: sum8
!
      dense = .false.
!
      read (82,end=100) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                      & np,(pz(k),pt(k),k=1,np)
!
      if (dtyp .eq. 'TA' .or. dtyp .eq. 'TR' .or. dtyp .eq. 'BU') then
        mooring = .true.
      else
        mooring = .false.
      end if
!
      n = 1
      mdy0 = idate / 10000
      iy0 = iyear
      id0 = idate
      iym = iyear
      idm = idate
      xm = xp
      ym = yp
      sum8 = 0.0
      ndy = 1
      avd(ndy) = .false.
!
      do while (.true.)
        read (82,end=100) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                        & np,(pz(k),pt(k),k=1,np)
        mdy = idate / 10000
        if (mdy .eq. mdy0) then
!
          n = n + 1
          dx = abs(xp - xm)
          if (dx .gt. 300.0) dx = 360.0 - dx
          dy = yp -ym
          sum8 = sum8 + sqrt(dx**2 + dy**2) 
          iym = iyear
          idm = idate
          xm = xp
          ym = yp
!
        else
!
          mon = id0 / 1000000
          idy = mod(id0/10000,100)
          jd0 = iw3jdn(iy0,mon,idy)
          mon = idm / 1000000
          idy = mod(idm/10000,100)
          jd1 = iw3jdn(iym,mon,idy)
          ndys = jd1 - jd0 + 1
          tppd = float(n) / float(ndys)
          asep = sum8 / float(n)
          if (tppd .ge. tpmx .and. asep .le. aspmn) then
            dense = .true.
            avd(ndy) = .true.
          end if
!
          n = 1
          mdy0 = idate / 10000
          iy0 = iyear
          id0 = idate
          iym = iyear
          idm = idate
          xm = xp
          ym = yp
          sum8 = 0.0
          ndy = ndy + 1
          avd(ndy) = .false.
!
        end if
      end do
  100 continue
      rewind(82)
      close(82); open (82,form='unformatted', access='sequential')
!
      mon = id0 / 1000000
      idy = mod(id0/10000,100)
      jd0 = iw3jdn(iy0,mon,idy)
      mon = idm / 1000000
      idy = mod(idm/10000,100)
      jd1 = iw3jdn(iym,mon,idy)
      ndys = jd1 - jd0 + 1
      tppd = float(n) / float(ndys)
      asep = sum8 / float(n)
!
      if (tppd .ge. tpmx .and. asep .le. aspmn) then
        dense = .true.
        avd(ndy) = .true.
      end if
!
      end subroutine chk_prof_dst
!
! -------------------------------------------------------------- 
!
      subroutine daily_ave
!
      logical flg
!
      read (82, end=100) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                          & np,(pz(k),pt(k),k=1,np)
      ndy = 1
      if (avd(ndy)) then
        iyear0 = iyear
        iday0 = idate / 10000
        ihr = (idate - iday0*10000) / 100
        ya = yp
        xa = xp
        do k=1,np
          za(k) = pz(k)
          ta(k) = pt(k)
          na(k) = 1
        end do
        npa = np
        nav = 1
      else
        iday0 = idate / 10000
        write (83) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                          & np,(pz(k),pt(k),k=1,np)
        nav = 0
      end if
!
      do while (.true.)
        read (82, end=100) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                            & np,(pz(k),pt(k),k=1,np)
        if (avd(ndy)) then
          iday = idate / 10000
          if (iday .eq. iday0) then
            ihr = ihr + (idate - iday*10000) / 100
            ya = ya + yp
            xa = xa + xp
            do k=1,np
              flg = .true.
              do ka=1,npa
                if (abs(za(ka)-pz(k)) .lt. eps) then
                  ta(ka) = ta(ka) + pt(k)
                  na(ka) = na(ka) + 1
                  flg = .false.
                  exit
                end if
              end do
              if (flg) then
                if (pz(k) .gt. za(npa)) then
                  npa = npa + 1
                  za(npa) = pz(k)
                  ta(npa) = pt(k)
                  na(npa) = 1
                else if (pz(k) .lt. za(1)) then
                  npa = npa + 1
                  do ka=npa,2,-1
                    za(ka) = za(ka-1)
                    ta(ka) = ta(ka-1)
                    na(ka) = na(ka-1)
                  end do
                  za(1) = pz(k)
                  ta(1) = pt(k)
                  na(1) = 1
                else
                  do ka=2,npa
                    if (pz(k) .gt. za(ka-1)  &
                                 & .and. pz(k) .lt. za(ka)) then
                      kb = ka
                      exit
                    end if
                  end do
                  npa = npa + 1
                  do ka=npa,kb+1,-1
                    za(ka) = za(ka-1)
                    ta(ka) = ta(ka-1)
                    na(ka) = na(ka-1)
                  end do
                  za(kb) = pz(k)
                  ta(kb) = pt(k)
                  na(kb) = 1
                end if
              end if
            end do
            nav = nav + 1
          else
            ya = ya / float(nav)
            xa = xa / float(nav)
            ihr = ihr / nav
            idate0 = iday0*10000 + ihr*100
            do k=1,npa
              ta(k) = ta(k) / float(na(k))
            end do
            write (83) iyear0,idate0,csign,sid,dtyp,qkey,ya,xa, &
                            & npa,(za(k),ta(k),k=1,npa)
!
            ndy = ndy + 1
            if (avd(ndy)) then
              iyear0 = iyear
              iday0 = idate / 10000
              ya = yp
              xa = xp
              ihr = (idate - iday*10000) / 100
              do k=1,np
                ta(k) = pt(k)
                za(k) = pz(k)
                na(k) = 1
              end do
              npa = np
              nav = 1
            else
              iday0 = idate / 10000
              write (83) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                                & np,(pz(k),pt(k),k=1,np)
            end if
          end if
        else
          iday = idate / 10000
          if (iday .eq. iday0) then
            write (83) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                              & np,(pz(k),pt(k),k=1,np)
          else
            ndy = ndy + 1
            if (avd(ndy)) then
              iyear0 = iyear
              iday0 = idate / 10000
              ihr = (idate - iday0*10000) / 100
              ya = yp
              xa = xp
              do k=1,np
                za(k) = pz(k)
                ta(k) = pt(k)
                na(k) = 1
              end do
              npa = np
              nav = 1
            else
              iday0 = idate / 10000
              write (83) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                                & np,(pz(k),pt(k),k=1,np)
            end if
          end if
        end if
      end do
  100 continue
!
      if (avd(ndy)) then
        ya = ya / float(nav)
        xa = xa / float(nav)
        ihr = ihr / nav
        idate0 = iday0*10000 + ihr*100
        do k=1,npa
          ta(k) = ta(k) / float(na(k))
        end do
        write (83) iyear0,idate0,csign,sid,dtyp,qkey,ya,xa, &
                           & npa,(za(k),ta(k),k=1,npa)
      else
        write (83) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                          & np,(pz(k),pt(k),k=1,np)
      end if
!
      rewind (82)
      rewind (83)
      close(82); open (82,form='unformatted', access='sequential')
      close(83); open (83,form='unformatted', access='sequential')
!
      end subroutine daily_ave
!
! -------------------------------------------------------------- 
!
      subroutine daily_ave_zaf
!
      logical flg
!
      read (82, end=100) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                          & np,(pz(k),pt(k),k=1,np)
      ndy = 1
      if (avd(ndy)) then
        iyear0 = iyear
        iday0 = idate / 10000
        ihr = (idate - iday0*10000) / 100
        ya = yp
        xa = xp
        call zintrp(pt,pz,np,wti,zaf,nfx,npi)
        do k=1,npi
          taf(k) = wti(k)
          naf(k) = 1
        end do
        do k=npi+1,nfx
          taf(k) = 0.0
          naf(k) = 0
        end do
!
        npa = npi
        nav = 1
      else
        iday0 = idate / 10000
        write (83) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                          & np,(pz(k),pt(k),k=1,np)
        nav = 0
      end if
!
      do while (.true.)
        read (82, end=100) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                            & np,(pz(k),pt(k),k=1,np)
        if (avd(ndy)) then
          iday = idate / 10000
          if (iday .eq. iday0) then
            ihr = ihr + (idate - iday*10000) / 100
            ya = ya + yp
            xa = xa + xp
            call zintrp(pt,pz,np,wti,zaf,nfx,npi)
            do k=1,npi
              taf(k) = taf(k) + wti(k)
              naf(k) = naf(k) + 1
            end do
            if (npi .gt. npa) npa = npi
            nav = nav + 1
          else
            ya = ya / float(nav)
            xa = xa / float(nav)
            ihr = ihr / nav
            idate0 = iday0*10000 + ihr*100
            do k=1,npa
              taf(k) = taf(k) / float(naf(k))
            end do
            write (83) iyear0,idate0,csign,sid,dtyp,qkey,ya,xa, &
                            & npa,(zaf(k),taf(k),k=1,npa)
!
            ndy = ndy + 1
            if (avd(ndy)) then
              iyear0 = iyear
              iday0 = idate / 10000
              ya = yp
              xa = xp
              ihr = (idate - iday*10000) / 100
              call zintrp(pt,pz,np,wti,zaf,nfx,npi)
              do k=1,npi
                taf(k) = wti(k)
                naf(k) = 1
              end do
              do k=npi+1,nfx
                taf(k) = 0.0
                naf(k) = 0
              end do
              npa = npi
              nav = 1
            else
              iday0 = idate / 10000
              write (83) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                                & np,(pz(k),pt(k),k=1,np)
            end if
          end if
        else
          iday = idate / 10000
          if (iday .eq. iday0) then
            write (83) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                              & np,(pz(k),pt(k),k=1,np)
          else
            ndy = ndy + 1
            if (avd(ndy)) then
              iyear0 = iyear
              iday0 = idate / 10000
              ihr = (idate - iday0*10000) / 100
              ya = yp
              xa = xp
              call zintrp(pt,pz,np,wti,zaf,nfx,npi)
              do k=1,npi
                taf(k) = wti(k)
                naf(k) = 1
              end do
              do k=npi+1,nfx
                taf(k) = 0.0
                naf(k) = 0
              end do
              npa = npi
              nav = 1
            else
              iday0 = idate / 10000
              write (83) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                                & np,(pz(k),pt(k),k=1,np)
            end if
          end if
        end if
      end do
  100 continue
!
      if (avd(ndy)) then
        ya = ya / float(nav)
        xa = xa / float(nav)
        ihr = ihr / nav
        idate0 = iday0*10000 + ihr*100
        do k=1,npa
          taf(k) = taf(k) / float(naf(k))
        end do
        write (83) iyear0,idate0,csign,sid,dtyp,qkey,ya,xa, &
                           & npa,(zaf(k),taf(k),k=1,npa)
      else
        write (83) iyear,idate,csign,sid,dtyp,qkey,yp,xp, &
                          & np,(pz(k),pt(k),k=1,np)
      end if
!
      rewind (82)
      rewind (83)
      close(82); open (82,form='unformatted', access='sequential')
      close(83); open (83,form='unformatted', access='sequential')
!
      end subroutine daily_ave_zaf
!
! -------------------------------------------------------------- 
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
!
! -------------------------------------------------------------- 
!
      subroutine merge_files(nu1,nu2,nu3)
!
      integer nu1, nu2, nu3
      integer(kind=8) :: iyd1, iyd2
      integer(kind=8), parameter :: iyfct = 100000000
      logical flg1, flg2
!
      flg1 = .true.
      flg2 = .true.
!
      read (nu1) iyear1,idate1,csign1,sid1,dtyp1,qkey1,y1,x1, &
                           &  np1,(z1(k),t1(k),k=1,np1)
      iyd1 = iyear1*iyfct + idate1
      read (nu2) iyear2,idate2,csign2,sid2,dtyp2,qkey2,y2,x2, &
                           &  np2,(z2(k),t2(k),k=1,np2)
      iyd2 = iyear2*iyfct + idate2
!
      do while (flg1 .or. flg2)
        if (flg1 .and. flg2) then
          if (iyd1 .le. iyd2) then
            write (nu3) iyear1,idate1,csign1,sid1,dtyp1,qkey1,y1,x1, &
                           &  np1,(z1(k),t1(k),k=1,np1)
            read (nu1, iostat=ios) iyear1,idate1,csign1,sid1,dtyp1, &
                           &  qkey1,y1,x1,np1,(z1(k),t1(k),k=1,np1)
            iyd1 = iyear1*iyfct + idate1
            if (ios .ne. 0) flg1 = .false.
          else
            write (nu3) iyear2,idate2,csign2,sid2,dtyp2,qkey2,y2,x2, &
                           &  np2,(z2(k),t2(k),k=1,np2)
            read (nu2, iostat=ios) iyear2,idate2,csign2,sid2,dtyp2, &
                           &  qkey2,y2,x2,np2,(z2(k),t2(k),k=1,np2)
            iyd2 = iyear2*iyfct + idate2
            if (ios .ne. 0) flg2 = .false.
          end if
        else if (flg1) then
          write (nu3) iyear1,idate1,csign1,sid1,dtyp1,qkey1,y1,x1, &
                           &  np1,(z1(k),t1(k),k=1,np1)
          read (nu1, iostat=ios) iyear1,idate1,csign1,sid1,dtyp1, &
                           &  qkey1,y1,x1,np1,(z1(k),t1(k),k=1,np1)
          iyd1 = iyear1*iyfct + idate1
          if (ios .ne. 0) flg1 = .false.
        else if (flg2) then
          write (nu3) iyear2,idate2,csign2,sid2,dtyp2,qkey2,y2,x2, &
                           &  np2,(z2(k),t2(k),k=1,np2)
          read (nu2, iostat=ios) iyear2,idate2,csign2,sid2,dtyp2, &
                           &  qkey2,y2,x2,np2,(z2(k),t2(k),k=1,np2)
          iyd2 = iyear2*iyfct + idate2
          if (ios .ne. 0) flg2 = .false.
        end if
      end do
!
      rewind (nu1)
      rewind (nu2)
      rewind (nu3)
      close(nu1); open (nu1,form='unformatted', access='sequential')
      close(nu2); open (nu2,form='unformatted', access='sequential')
      close(nu3); open (nu3,form='unformatted', access='sequential')
!
      end subroutine merge_files
!
      end program avePrfDly
