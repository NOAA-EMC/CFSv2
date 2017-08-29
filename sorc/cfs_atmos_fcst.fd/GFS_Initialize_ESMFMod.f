
! !MODULE: GFS_Initialize_ESMFMod --- Initialize module of the ESMF 
!                                     gridded component of the GFS system.
!
! !DESCRIPTION: GFS gridded component initialize module.
!
! !REVISION HISTORY:
!
!  November 2004  Weiyu Yang     Initial code.
!  January 2006  S. Moorthi      Update to the new GFS version
!  August 2006   H. Juang        Add option to run generalized coordinates
!  December 2006 S. Moorthi      GFSIO included
!  February 2008  Weiyu Yang     Modified for the ESMF 3.1.0 version, fixed bug for runDuration.
!  Mar.  2010   Y. Hou           Add 4 sw sfc downward flux components
!  Aug. 2010    S. Moorthi       T574 related changes
!
!
! !INTERFACE:
!
 MODULE GFS_Initialize_ESMFMod

!
!!USES:
!
 USE GFS_GetCf_ESMFMod
 USE MACHINE, ONLY : kind_io4
 use gfsio_module , only : gfsio_init

 IMPLICIT none

 CONTAINS

 SUBROUTINE GFS_Initialize(gcGFS, gis, clock, rc)

! This subroutine set up the internal state variables,
! allocate internal state arrays for initializing the GFS system.
!----------------------------------------------------------------

 TYPE(ESMF_VM)                                   :: vm_local   ! the ESMF virtual machine
 TYPE(ESMF_GridComp),              INTENT(inout) :: gcGFS
 TYPE(GFS_InternalState), POINTER, INTENT(inout) :: gis
 TYPE(ESMF_Clock),                 INTENT(inout) :: clock
 INTEGER,                          INTENT(out)   :: rc

 TYPE(ESMF_TimeInterval) :: timeStep
 TYPE(ESMF_TimeInterval) :: runDuration
 TYPE(ESMF_Time)         :: startTime
 TYPE(ESMF_Time)         :: stopTime
 TYPE(ESMF_Time)         :: currTime
 INTEGER                 :: timeStep_sec
 INTEGER                 :: runDuration_hour
 INTEGER                 :: ifhmax
 INTEGER                 :: rc1 = ESMF_SUCCESS
 INTEGER                 :: ierr, jerr
 INTEGER                 :: yyc, mmc, ddc, hhc, minsc
 INTEGER                 :: NUM_PARTHDS
!

 INTEGER :: l, ilat, locl, ikey, nrank_all, nfluxes
 real (kind=kind_io4) blatc4
 real (kind=kind_io4), allocatable :: pl_lat4(:), pl_pres4(:), pl_time4(:)

! Set up parameters of MPI communications.
! Use ESMF utility to get PE identification and total number of PEs.
!-------------------------------------------------------------------
 me     = gis%me
 NODES  = gis%nodes
 nlunit = gis%nam_gfs%nlunit

 CALL COMPNS(gis%DELTIM,gis%IRET,                                            &
             gis%ntrac,   gis%nxpt, gis%nypt, gis%jintmx, gis%jcap,          &
             gis%levs,    gis%levr, gis%lonf, gis%lonr,   gis%latg, gis%latr,&
             gis%ntoz,    gis%ntcw, gis%ncld, gis%lsoil,  gis%nmtvr,         &
             gis%num_p3d, gis%num_p2d, me,    gis%nam_gfs%nlunit, gis%nam_gfs%gfs_namelist)
!
 CALL set_soilveg(me,gis%nam_gfs%nlunit)
 call set_tracer_const(gis%ntrac,me,gis%nam_gfs%nlunit)			! hmhj
!

      ntrac   = gis%ntrac
      nxpt    = gis%nxpt
      nypt    = gis%nypt
      jintmx  = gis%jintmx
      jcap    = gis%jcap
      levs    = gis%levs
      levr    = gis%levr
      lonf    = gis%lonf
      lonr    = gis%lonr
      latg    = gis%latg
      latr    = gis%latr
      ntoz    = gis%ntoz
      ntcw    = gis%ntcw
      ncld    = gis%ncld
      lsoil   = gis%lsoil
      nmtvr   = gis%nmtvr
      num_p3d = gis%num_p3d
      num_p2d = gis%num_p2d
      if (gis%nam_gfs%Total_Member <= 1) then
        ens_nam=' '
      else
        write(ens_nam,'("_",I2.2)') gis%nam_gfs%Member_Id
      endif
!
!     ivssfc  = 200501
      ivssfc  = 200509
      ivssfc_restart  = 200509
      if (ivssfc .gt. ivssfc_restart) ivssfc_restart = ivssfc
      ivsupa  = 0
      if (levs .gt. 99) ivsupa  = 200509
!
      levh   = ntrac*levs
      latgd   = latg+ 2*jintmx 
      jcap1  = jcap+1 
      jcap2  = jcap+2 
      latg2  = latg/2 
      latr2  = latr/2 
      levm1  = levs-1 
      levp1  = levs+1 
!jfe  parameter ( lonfx  = lonf+2 )
      lonfx  = lonf + 1 + 2*nxpt+1 
      lonrx  = lonr+2 
      lnt    = jcap2*jcap1/2 
      lnuv   = jcap2*jcap1 
      lnt2   = 2*lnt 
      lnt22  = 2*lnt+1 
      lnte   = (jcap2/2)*((jcap2/2)+1)-1 
      lnto   = (jcap2/2)*((jcap2/2)+1)-(jcap2/2) 
      lnted  = lnte 
      lntod  = lnto 

!     ngrids_sfcc = 32+LSOIL*3
!     ngrids_sfcc = 29+LSOIL*3   ! No CV, CVB, CVT!
      ngrids_sfcc = 32+LSOIL*3   ! No CV, CVB, CVT! includes T2M, Q2M, TISFC
!*RADFLX*
!*    ngrids_flx  = 66+30        ! additional fields (most related to land surface)
      if (climate) then
!       ngrids_flx  = 66+33      ! additional fields (land surface + rad flux)
!       ngrids_flx  = 66+37      ! additional fields (land surface + rad flux) - yth 3/10
        ngrids_flx  = 66+38      ! additional fields (land surface + rad flux+snowfall) - yth 3/10
      else
!       ngrids_flx  = 66+40      ! additional fields (land surface + rad flux)
!       ngrids_flx  = 66+44      ! additional fields (land surface + rad flux) - yth 3/10
        ngrids_flx  = 66+45      ! additional fields (land surface + rad flux_snowfall) - yth 3/10
      endif
!*RADFLX*
!*    nfxr        = 27
!*    nfxr        = 33
      nfxr        = 37           ! yth 03/10 added 4 sfc sw flux components
      ngrids_gg   = 2+LEVS*(4+ntrac)

      gis%lnt2    = lnt2

      allocate(lat1s_a(0:jcap))
      allocate(lat1s_r(0:jcap))
      allocate(lon_dims_a(latgd))
      allocate(lon_dims_ext(latgd))
!my   allocate(lon_dims_r(latgd))
      allocate(lon_dims_r(latr))

      allocate(colrad_a(latg2))
      allocate(wgt_a(latg2))
      allocate(wgtcs_a(latg2))
      allocate(rcs2_a(latg2))
      allocate(sinlat_a(latg2))

      allocate(colrad_r(latr))
      allocate(wgt_r(latr2))
      allocate(wgtcs_r(latr2))
      allocate(rcs2_r(latr2))
      allocate(sinlat_r(latr))
      allocate(coslat_r(latr))

      allocate(am(levs,levs))
      allocate(bm(levs,levs))
      allocate(cm(levs,levs))
      allocate(dm(levs,levs,jcap1))
      allocate(tor(levs))
      allocate(si(levp1))
      allocate(sik(levp1))
      allocate(sl(levs))
      allocate(slk(levs))
      allocate(del(levs))
      allocate(rdel2(levs))
      allocate(ci(levp1))
      allocate(cl(levs))
      allocate(tov(levs))
      allocate(sv(levs))

      allocate(AK5(LEVP1))
      allocate(BK5(LEVP1))
      allocate(CK5(LEVP1))                                            ! hmhj
      allocate(THREF(LEVP1))                                          ! hmhj
      allocate(CK(LEVS))
      allocate(DBK(LEVS))
      allocate(bkl(LEVS))
      allocate(AMHYB(LEVS,LEVS))
      allocate(BMHYB(LEVS,LEVS))
      allocate(SVHYB(LEVS))
      allocate(tor_hyb(LEVS))
      allocate(D_HYB_m(levs,levs,jcap1))
      allocate(dm205_hyb(jcap1,levs,levs))

      allocate(spdmax(levs))

!     allocate(buf_sig(lnt2,3*levs+2),buff_grid(lonr,latr),
!     allocate(buf_sig(lnt2,3*levs+2),
!    &         buff_mult(lonr,latr,ngrids_sfc))
!     allocate(buf_sig_n(lnt2,levs,ntrac))
      allocate(buff_mult(lonr,latr,ngrids_sfc))
      if (gfsio_out) then
        allocate(buff_multg(lonr*latr,ngrids_gg))
      endif

      allocate(LBASDZ(4,2,levs),LBASIZ(4,2,LEVS),DETAI(levp1), &
       DETAM(levs),ETAMID(levs),ETAINT(levp1),                 &
       SINLAMG(lonf,latg2),COSLAMG(lonf,latg2))
!

      allocate(tor_sig(levs), d_m(levs,levs,jcap1),            &
         dm205(jcap1,levs,levs))
         dm205=555555555.
         d_m  =444444444.
!

      allocate(z(lnt2))
      allocate(z_r(lnt2))
!
      nfluxes = 153
      allocate(fmm(lonr*latr,nfluxes),lbmm(lonr*latr,nfluxes))
      allocate(ibufm(50,nfluxes),rbufm(50,nfluxes))

!
      allocate(gis%LONSPERLAT(latg))

      allocate(gis%lonsperlar(latr))

      rc = 0
      if (num_reduce == 0) then
        gis%lonsperlat = lonf
        gis%lonsperlar = lonr
      else
        call set_lonsgg(gis%lonsperlat,gis%lonsperlar,num_reduce,me)
      endif
!
      if (ras) then
        nrcm = max(nrcmax, nint((nrcmax*gis%deltim)/600.0))
      else
        nrcm = 1
      endif
!
      if (ntoz .le. 0) then      ! Diagnostic ozone
        rewind (kozc)
        read (kozc,end=101) latsozc, levozc, timeozc, blatc4
  101   if (levozc .lt. 10 .or. levozc .gt. 100) then
          rewind (kozc)
          levozc  = 17
          latsozc = 18
          blatc   = -85.0
        else
          blatc   = blatc4
        endif
        latsozp   = 2
        levozp    = 1
        timeoz    = 1
        pl_coeff  = 0
      else                       ! Prognostic Ozone
        rewind (kozpl)
        read (kozpl) pl_coeff, latsozp, levozp, timeoz
        allocate (pl_lat(latsozp), pl_pres(levozp),pl_time(timeoz+1))
        allocate (pl_lat4(latsozp), pl_pres4(levozp),pl_time4(timeoz+1))
        rewind (kozpl)
        read (kozpl) pl_coeff, latsozp, levozp, timeoz, pl_lat4, pl_pres4,  &
                     pl_time4
        pl_pres(:) = pl_pres4(:)
        pl_lat(:)  = pl_lat4(:)
        pl_time(:) = pl_time4(:)
        latsozc = 2
        blatc   = 0.0
      endif
      dphiozc = -(blatc+blatc)/(latsozc-1)
!
      if (me .eq. 0) then
        print *,' latsozp=',latsozp,' levozp=',levozp,' timeoz=',timeoz
        print *,' latsozc=',latsozc,' levozc=',levozc,' timeozc=',        &
                  timeozc, 'dphiozc=',dphiozc
        print *,' pl_lat=',pl_lat
        print *,' pl_pres=',pl_pres
        print *,' pl_time=',pl_time
      endif
      pl_pres(:) = log(0.1*pl_pres(:))       ! Natural log of pres in cbars
!
      allocate(gis%OZPLIN(LATSOZP,LEVOZP,pl_coeff,timeoz)) !OZONE P-L coeffcients
!
      P_GZ  = 0*LEVS+0*LEVH+1  !      GZE/O(LNTE/OD,2),
      P_ZEM = 0*LEVS+0*LEVH+2  !     ZEME/O(LNTE/OD,2,LEVS),
      P_DIM = 1*LEVS+0*LEVH+2  !     DIME/O(LNTE/OD,2,LEVS),
      P_TEM = 2*LEVS+0*LEVH+2  !     TEME/O(LNTE/OD,2,LEVS),
      P_RM  = 3*LEVS+0*LEVH+2  !      RME/O(LNTE/OD,2,LEVH),
      P_QM  = 3*LEVS+1*LEVH+2  !      QME/O(LNTE/OD,2),
      P_ZE  = 3*LEVS+1*LEVH+3  !      ZEE/O(LNTE/OD,2,LEVS),
      P_DI  = 4*LEVS+1*LEVH+3  !      DIE/O(LNTE/OD,2,LEVS),
      P_TE  = 5*LEVS+1*LEVH+3  !      TEE/O(LNTE/OD,2,LEVS),
      P_RQ  = 6*LEVS+1*LEVH+3  !      RQE/O(LNTE/OD,2,LEVH),
      P_Q   = 6*LEVS+2*LEVH+3  !       QE/O(LNTE/OD,2),
      P_DLAM= 6*LEVS+2*LEVH+4  !  DPDLAME/O(LNTE/OD,2),
      P_DPHI= 6*LEVS+2*LEVH+5  !  DPDPHIE/O(LNTE/OD,2),
      P_ULN = 6*LEVS+2*LEVH+6  !     ULNE/O(LNTE/OD,2,LEVS),
      P_VLN = 7*LEVS+2*LEVH+6  !     VLNE/O(LNTE/OD,2,LEVS),
      P_W   = 8*LEVS+2*LEVH+6  !       WE/O(LNTE/OD,2,LEVS),
      P_X   = 9*LEVS+2*LEVH+6  !       XE/O(LNTE/OD,2,LEVS),
      P_Y   =10*LEVS+2*LEVH+6  !       YE/O(LNTE/OD,2,LEVS),
      P_RT  =11*LEVS+2*LEVH+6  !      RTE/O(LNTE/OD,2,LEVH),
      P_ZQ  =11*LEVS+3*LEVH+6  !      ZQE/O(LNTE/OD,2)
!C
      LOTS = 5*LEVS+1*LEVH+3 
      LOTD = 6*LEVS+2*LEVH+0 
      LOTA = 3*LEVS+1*LEVH+1 
!
      kwq  = 0*levs+0*levh+1   !   qe/o_ls
      kwte = 0*levs+0*levh+2   !  tee/o_ls
      kwdz = 1*levs+0*levh+2   !  die/o_ls  zee/o_ls
      kwrq = 3*levs+0*levh+2   !  rqe/o_ls

!
      gis%P_GZ  = 0*LEVS+0*LEVH+1  !      GZE/O(LNTE/OD,2),
      gis%P_ZEM = 0*LEVS+0*LEVH+2  !     ZEME/O(LNTE/OD,2,LEVS),
      gis%P_DIM = 1*LEVS+0*LEVH+2  !     DIME/O(LNTE/OD,2,LEVS),
      gis%P_TEM = 2*LEVS+0*LEVH+2  !     TEME/O(LNTE/OD,2,LEVS),
      gis%P_RM  = 3*LEVS+0*LEVH+2  !      RME/O(LNTE/OD,2,LEVH),
      gis%P_QM  = 3*LEVS+1*LEVH+2  !      QME/O(LNTE/OD,2),
      gis%P_ZE  = 3*LEVS+1*LEVH+3  !      ZEE/O(LNTE/OD,2,LEVS),
      gis%P_DI  = 4*LEVS+1*LEVH+3  !      DIE/O(LNTE/OD,2,LEVS),
      gis%P_TE  = 5*LEVS+1*LEVH+3  !      TEE/O(LNTE/OD,2,LEVS),
      gis%P_RQ  = 6*LEVS+1*LEVH+3  !      RQE/O(LNTE/OD,2,LEVH),
      gis%P_Q   = 6*LEVS+2*LEVH+3  !       QE/O(LNTE/OD,2),
      gis%P_DLAM= 6*LEVS+2*LEVH+4  !  DPDLAME/O(LNTE/OD,2),
      gis%P_DPHI= 6*LEVS+2*LEVH+5  !  DPDPHIE/O(LNTE/OD,2),
      gis%P_ULN = 6*LEVS+2*LEVH+6  !     ULNE/O(LNTE/OD,2,LEVS),
      gis%P_VLN = 7*LEVS+2*LEVH+6  !     VLNE/O(LNTE/OD,2,LEVS),
      gis%P_W   = 8*LEVS+2*LEVH+6  !       WE/O(LNTE/OD,2,LEVS),
      gis%P_X   = 9*LEVS+2*LEVH+6  !       XE/O(LNTE/OD,2,LEVS),
      gis%P_Y   =10*LEVS+2*LEVH+6  !       YE/O(LNTE/OD,2,LEVS),
      gis%P_RT  =11*LEVS+2*LEVH+6  !      RTE/O(LNTE/OD,2,LEVH),
      gis%P_ZQ  =11*LEVS+3*LEVH+6  !      ZQE/O(LNTE/OD,2)
!C
      gis%LOTS = 5*LEVS+1*LEVH+3 
      gis%LOTD = 6*LEVS+2*LEVH+0 
      gis%LOTA = 3*LEVS+1*LEVH+1 
!C
      allocate(gis%TEE1(LEVS))

      gis%LSLAG=.FALSE.  ! IF FALSE EULERIAN SCHEME =.true. for semilag

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!
!!      Create IO communicator and comp communicator
!!
!sela LIOPE=.FALSE.
!!       LIOPE=.TRUE.
      IF (me == 0) write(*,*) 'IO OPTION ,LIOPE :',LIOPE
!
      CALL ESMF_VMGetCurrent(vm_local, rc = ierr)
      CALL ESMF_VMGet(vm_local, mpiCommunicator = MPI_COMM_ALL,     &
                      peCount = nodes, rc = ierr)
!
      CALL MPI_COMM_DUP(MPI_COMM_ALL, MPI_COMM_ALL_DUP, ierr)
      CALL MPI_Barrier (MPI_COMM_ALL_DUP,               ierr)

      IF (NODES == 1) LIOPE=.FALSE.
      IF (LIOPE) THEN
!       CALL MPI_COMM_SPLIT(MPI_COMM_WORLD,1,1,MPI_COMM_ALL,ierr)
        CALL MPI_COMM_RANK(MPI_COMM_ALL_DUP,nrank_all,ierr)
        icolor=1
        ikey=1
        nodes_comp=nodes-1
        if (nrank_all.eq.nodes-1) then
!!  IO server
          write(*,*) 'IO server task'
          icolor=2
          gis%kcolor=MPI_UNDEFINED
          CALL MPI_COMM_SPLIT(MPI_COMM_ALL_DUP,icolor,ikey,MC_IO,ierr)
          CALL MPI_COMM_SPLIT(MPI_COMM_ALL_DUP,gis%kcolor,ikey,MC_COMP,ierr)
        else
!sela     write(*,*) 'COMPUTE SERVER TASK '
          icolor=MPI_UNDEFINED
          gis%kcolor=1
          CALL MPI_COMM_SPLIT(MPI_COMM_ALL_DUP,gis%kcolor,ikey,MC_COMP,ierr)
          CALL MPI_COMM_SPLIT(MPI_COMM_ALL_DUP,icolor,ikey,MC_IO,ierr)
          CALL MPI_COMM_SIZE(MC_COMP,NODES,IERR)
        endif
      ELSE
        icolor=2
        MC_COMP=MPI_COMM_ALL_DUP
        nodes_comp=nodes
      ENDIF
!!
!C
      CALL f_hpminit(ME,"EVOD")  !jjt hpm stuff
!C
      CALL f_hpmstart(25,"GET_LS_GFTLONS")
!C
      if(me.eq.0) then
        call w3tagb('gsm     ',0000,0000,0000,'np23   ')
      endif
!!
      CALL synchro
      CALL init_countperf(latg)
!$$$      time0=timer()
!jfe  CALL countperf(0,15,0.)
!
      if (me.eq.0) then
      PRINT 100, JCAP,LEVS
100   FORMAT (' SMF ',I3,I3,' CREATED AUGUST 2000 EV OD RI ')
      PRINT*,'NUMBER OF THREADS IS ',NUM_PARTHDS()
        if (liope) then
          PRINT*,'NUMBER OF MPI PROCS IS ',NODES
          PRINT*,'NUMBER OF MPI IO PROCS IS 1 (nodes)'
        else
          PRINT*,'NUMBER OF MPI PROCS IS ',NODES
        endif
      endif
!C
      gis%CONS0    =    0.0D0
      gis%CONS0P5  =    0.5D0
      gis%CONS1200 = 1200.D0
      gis%CONS3600 = 3600.D0
!C
      if (liope) then
         if (icolor.eq.2) then
           LS_DIM = JCAP1
         else
           LS_DIM = (JCAP1-1)/NODES+1
         endif
      else
         LS_DIM = (JCAP1-1)/NODES+1
      endif
!!
!C
!CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!C
!C
      ALLOCATE (      gis%LS_NODE (LS_DIM*3) )
      ALLOCATE (      gis%LS_NODES(LS_DIM,NODES) )
      ALLOCATE (  gis%MAX_LS_NODES(NODES) )
!C
      ALLOCATE (  gis%LATS_NODES_A(NODES) )
      ALLOCATE ( gis%GLOBAL_LATS_A(LATG) )
!C
      ALLOCATE (  gis%LATS_NODES_R(NODES) )
      ALLOCATE ( gis%GLOBAL_LATS_R(LATR) )
!C
      ALLOCATE (   gis%LATS_NODES_EXT(NODES) )
      ALLOCATE ( gis%GLOBAL_LATS_EXT(LATG+2*JINTMX+2*NYPT*(NODES-1)) )
!C
!C
      gis%IPRINT = 0
      CALL GET_LS_NODE( ME, gis%LS_NODE, LS_MAX_NODE, gis%IPRINT )
!C
!C
      LEN_TRIE_LS=0
      LEN_TRIO_LS=0
      DO LOCL=1,LS_MAX_NODE
           gis%LS_NODE(LOCL+  LS_DIM)=LEN_TRIE_LS
          gis%LS_NODE(LOCL+2*LS_DIM)=LEN_TRIO_LS
         L=gis%LS_NODE(LOCL)
         LEN_TRIE_LS=LEN_TRIE_LS+(JCAP+3-L)/2
         LEN_TRIO_LS=LEN_TRIO_LS+(JCAP+2-L)/2
      ENDDO
      LEN_TRIE_LS = max(1,LEN_TRIE_LS)
      LEN_TRIO_LS = max(1,LEN_TRIO_LS)
!C
      write(0,*)'LEN_TRIE_LS=',LEN_TRIE_LS,'  LEN_TRIO_LS=', LEN_TRIO_LS,' me=',me
!C
      ALLOCATE (       gis%EPSE  (LEN_TRIE_LS) )
      ALLOCATE (       gis%EPSO  (LEN_TRIO_LS) )
      ALLOCATE (       gis%EPSEDN(LEN_TRIE_LS) )
      ALLOCATE (       gis%EPSODN(LEN_TRIO_LS) )
!C
      ALLOCATE (      gis%SNNP1EV(LEN_TRIE_LS) )
      ALLOCATE (      gis%SNNP1OD(LEN_TRIO_LS) )
!C
      ALLOCATE (       gis%NDEXEV(LEN_TRIE_LS) )
      ALLOCATE (       gis%NDEXOD(LEN_TRIO_LS) )
!C
      ALLOCATE (      gis%PLNEV_A(LEN_TRIE_LS,LATG2) )
      ALLOCATE (      gis%PLNOD_A(LEN_TRIO_LS,LATG2) )
      ALLOCATE (      gis%PDDEV_A(LEN_TRIE_LS,LATG2) )
      ALLOCATE (      gis%PDDOD_A(LEN_TRIO_LS,LATG2) )
      ALLOCATE (      gis%PLNEW_A(LEN_TRIE_LS,LATG2) )
      ALLOCATE (      gis%PLNOW_A(LEN_TRIO_LS,LATG2) )
!C
      ALLOCATE (      gis%PLNEV_R(LEN_TRIE_LS,LATR2) )
      ALLOCATE (      gis%PLNOD_R(LEN_TRIO_LS,LATR2) )
      ALLOCATE (      gis%PDDEV_R(LEN_TRIE_LS,LATR2) )
      ALLOCATE (      gis%PDDOD_R(LEN_TRIO_LS,LATR2) )
      ALLOCATE (      gis%PLNEW_R(LEN_TRIE_LS,LATR2) )
      ALLOCATE (      gis%PLNOW_R(LEN_TRIO_LS,LATR2) )
!C
      gis%MAXSTP=36

 
      IF(ME.EQ.0) PRINT*,'FROM COMPNS : IRET=',gis%IRET,' NSOUT=',NSOUT, &
       ' NSSWR=',NSSWR,' NSLWR=',NSLWR,' NSZER=',NSZER,' NSRES=',NSRES,  &
       ' NSDFI=',NSDFI,' NSCYC=',NSCYC,' RAS=',RAS
      IF(gis%IRET.NE.0) THEN
        IF(ME.EQ.0) PRINT *,' INCOMPATIBLE NAMELIST - ABORTED IN MAIN'
        CALL MPI_QUIT(13)
      ENDIF
!!
!     IF PREDICTED OZON IS DESIRED SET JO3=2
      JO3=2          !USING PREDICTED OZONE IN RADIATION.
!C
      gis%LATS_NODES_EXT = 0
      CALL GETCON(gis%NGES,gis%NRADR,gis%NRADF,gis%NNMOD,               &
           gis%N3,gis%N4,gis%NFLPS,gis%NSIGI,gis%NSIGS,gis%NSFCI,       &
           gis%NZNLI,gis%NSFCF,gis%NZNLF,gis%NSFCS,gis%NZNLS,           &
           gis%NDGI,gis%NDGF,gis%NGPKEN,                                &
           gis%MODS,gis%NITER,gis%INI,gis%NSTEP,gis%NFILES,             &
           gis%KSOUT,gis%IFGES,gis%IBRAD,                               &
           gis%LS_NODE,gis%LS_NODES,gis%MAX_LS_NODES,                   &
           gis%LATS_NODES_A,gis%GLOBAL_LATS_A,                          &
           gis%LONSPERLAT,                                              &
           gis%LATS_NODES_R,gis%GLOBAL_LATS_R,                          &
           gis%LONSPERLAR,                                              &
           gis%LATS_NODES_EXT,gis%GLOBAL_LATS_EXT,                      &
           gis%EPSE,gis%EPSO,gis%EPSEDN,gis%EPSODN,                     &
           gis%SNNP1EV,gis%SNNP1OD,gis%NDEXEV,gis%NDEXOD,               &
           gis%PLNEV_A,gis%PLNOD_A,gis%PDDEV_A,gis%PDDOD_A,             &
           gis%PLNEW_A,gis%PLNOW_A,                                     &
           gis%PLNEV_R,gis%PLNOD_R,gis%PDDEV_R,gis%PDDOD_R,             &
           gis%PLNEW_R,gis%PLNOW_R,gis%colat1)
!!
      call sfcvar_aldata(lonr,lats_node_r,lsoil,gis%sfc_fld,ierr)
      call flxvar_aldata(lonr,lats_node_r,gis%flx_fld,ierr)

      ALLOCATE (   gis%XLON(LONR,LATS_NODE_R))
      ALLOCATE (   gis%XLAT(LONR,LATS_NODE_R))
      ALLOCATE (   gis%COSZDG(LONR,LATS_NODE_R))
      ALLOCATE (   gis%SFALB(LONR,LATS_NODE_R))
      ALLOCATE (   gis%HPRIME(NMTVR,LONR,LATS_NODE_R))
      ALLOCATE (   gis%FLUXR(nfxr,LONR,LATS_NODE_R))

      gis%NBLCK=LONR/NGPTC+1
      ALLOCATE (   gis%SWH(NGPTC,LEVS,gis%NBLCK,LATS_NODE_R))
      ALLOCATE (   gis%HLW(NGPTC,LEVS,gis%NBLCK,LATS_NODE_R))

      ALLOCATE (gis%JINDX1(LATS_NODE_R),gis%JINDX2(LATS_NODE_R))
      ALLOCATE (gis%DDY(LATS_NODE_R))
!
      allocate (gis%phy_f3d(NGPTC,LEVS,gis%NBLCK,lats_node_r,num_p3d))
      allocate (gis%phy_f2d(lonr,lats_node_r,num_p2d))
!
      if (ldiag3d) then
        call d3d_init(ngptc,gis%nblck,lonr,lats_node_r,levs,pl_coeff)
      else
        call d3d_init(1,gis%nblck,1,lats_node_r,1,pl_coeff) ! Needs allocation
      endif
      if (gfsio_out .or. gfsio_in) then
        call gfsio_init(ierr)
      endif

      if (icolor /= 2 .or. .not. liope) then
        if (num_p3d .gt. 0) gis%phy_f3d = 0.0
        if (num_p2d .gt. 0) gis%phy_f2d = 0.0
      endif
!!
      CALL countperf(0,18,0.)
!!
! Modified by Weiyu.
!-------------------
!     if (.NOT.LIOPE.or.icolor.ne.2) then
!!
      CALL countperf(0,15,0.)
      ALLOCATE (      gis%TRIE_LS(LEN_TRIE_LS,2,11*LEVS+3*LEVH+6) )
      ALLOCATE (      gis%TRIO_LS(LEN_TRIO_LS,2,11*LEVS+3*LEVH+6) )
!C
      ALLOCATE (     gis%SYN_LS_A(4*LS_DIM,gis%LOTS,LATG2) )
      ALLOCATE (     gis%DYN_LS_A(4*LS_DIM,gis%LOTD,LATG2) )
!C
      ALLOCATE (   gis%SYN_GR_A_1(LONFX*gis%LOTS,LATS_DIM_EXT) )
      ALLOCATE (   gis%SYN_GR_A_2(LONFX*gis%LOTS,LATS_DIM_EXT) )
      ALLOCATE (   gis%DYN_GR_A_1(LONFX*gis%LOTD,LATS_DIM_EXT) )
      ALLOCATE (   gis%DYN_GR_A_2(LONFX*gis%LOTD,LATS_DIM_EXT) )
      ALLOCATE (   gis%ANL_GR_A_1(LONFX*gis%LOTA,LATS_DIM_EXT) )
      ALLOCATE (   gis%ANL_GR_A_2(LONFX*gis%LOTA,LATS_DIM_EXT) )
!!
!     endif !(.NOT.LIOPE.or.icolor.ne.2)
!!
      if (me == 0) then
        PRINT*, ' LATS_DIM_A=', LATS_DIM_A, ' LATS_NODE_A=', LATS_NODE_A
        PRINT*, ' LATS_DIM_EXT=', LATS_DIM_EXT,              &
                ' LATS_NODE_EXT=', LATS_NODE_EXT
        PRINT*, ' LATS_DIM_R=', LATS_DIM_R, ' LATS_NODE_R=', LATS_NODE_R
      endif
!
      IF (gis%LSLAG) THEN
        ILAT=LATS_NODE_EXT
      ELSE
        ILAT=LATS_NODE_A
      ENDIF
      CALL countperf(1,15,0.)
!!
!C......................................................................
!C
      CALL countperf(0,15,0.)
      CALL f_hpmstop(25)
!C
      WRITE(*,*) 'NUMBER OF LATITUDES EXT. :',LATS_NODE_EXT,              &
                  LATS_DIM_EXT,LATS_NODE_A
!!
!JFE  ALLOCATE (LATLOCAL(LATGD,0:NODES-1))
!JFE  ALLOCATE (LBASIY(4,2,LATS_NODE_EXT))
!JFE  ALLOCATE (PHI(LATS_NODE_EXT))
!JFE  ALLOCATE (DPHI(LATS_NODE_EXT))
!JFE  ALLOCATE (DLAM(LATS_NODE_EXT))
!JFE  ALLOCATE (LAMEXT(LONFX,LATS_NODE_EXT))
!JFE  ALLOCATE (LAM(LONFX,LATS_NODE_A+1))
!JFE  ALLOCATE (LAMMP(LONF,LEVS,LATS_NODE_A))
!JFE  ALLOCATE (SIGMP(LONF,LEVS,LATS_NODE_A))
!JFE  ALLOCATE (PHIMP(LONF,LEVS,LATS_NODE_A))
!!
!JFE  ALLOCATE (LATSINPE(LATS_NODE_A))
!JFE  JPT=0
!JFE  DO NODE=1,NODES
!JFE     IF ( LATS_NODES_A(NODE) .GT. 0 .AND.ME+1.EQ.NODE) THEN
!JFE        DO JCOUNT=1,LATS_NODES_A(NODE)
!JFE           LATSINPE(JCOUNT)=GLOBAL_LATS_A(JPT+JCOUNT)
!JFE        ENDDO
!JFE     ENDIF
!JFE     JPT=JPT+LATS_NODES_A(NODE)
!JFE  ENDDO
!!
!JFE  IF (LSLAG) THEN
!JFE    CALL SULAG(LAM,DLAM,LAMEXT,LATLOCAL,PHI,DPHI,DPHIBR,PHIBS,
!JFE &           LBASIY,LAMMP,PHIMP,SIGMP,gis%LONSPERLAT,
!JFE &           IPRINT,LATSINPE)
!JFE  ENDIF
!C
      CALL countperf(1,15,0.)
!!
      print *,' sig_ini=',gis%nam_gfs%sig_ini,' sig_ini2=',gis%nam_gfs%sig_ini2 &
             ,' sfc_ini=',gis%nam_gfs%sfc_ini
      CALL countperf(0,18,0.)
      gis%pdryini = 0.0
!     write(0,*)' ME=',me,' Before calling spect_fields'
      CALL spect_fields(gis%n1, gis%n2,                                &
        gis%PDRYINI, gis%TRIE_LS,  gis%TRIO_LS,                        &
        gis%LS_NODE, gis%LS_NODES, gis%MAX_LS_NODES,                   &
        gis%SNNP1EV, gis%SNNP1OD,  gis%phy_f3d,  gis%phy_f2d,          &
        gis%global_lats_r,  gis%nblck,   gis%lonsperlar,               &
        gis%epse, gis%epso, gis%plnev_r, gis%plnod_r,                  &
                            gis%plnew_r, gis%plnow_r, gis%lats_nodes_r,&
        gis%nam_gfs%sig_ini, gis%nam_gfs%sig_ini2)
!     write(0,*)' ME=',me,' After calling spect_fields'
!!
      CALL fix_fields(gis%LONSPERLAR,gis%GLOBAL_LATS_R,                &
        gis%XLON,gis%XLAT,gis%sfc_fld,                                 &
        gis%HPRIME,gis%JINDX1,gis%JINDX2,gis%DDY,                      &
        gis%OZPLIN,gis%nam_gfs%sfc_ini)
        CALL countperf(1,18,0.)
!     write(0,*)' ME=',me,' After calling fix_fields'
!!
!     if(hybrid)then
!      CALL deldifs_hyb(gis%epse,gis%epse,gis%epse,gis%epse,gis%epse,  &
!                  gis%epso,gis%epso,gis%epso,gis%epso,gis%epso,       &
!                  gis%cons0,gis%LS_NODE,gis%epse,0)
!
!     else
 
      tov = 0.0
      if (.not. (hybrid.or.gen_coord_hybrid) ) then                   ! hmhj
       call setsig(si,ci,del,sl,cl,rdel2,tov,me)
       am=-8888888.
       bm=-7777777.
       call amhmtm(del,sv,am)
       CALL BMDI_sig(ci,bm)
      endif
      CALL deldifs(gis%epse,gis%epse,gis%epse,gis%epse,gis%epse,gis%epse,      & ! hmhj
                   gis%epso,gis%epso,gis%epso,gis%epso,gis%epso,gis%epso,      & ! hmhj
                   gis%cons0,SL,gis%LS_NODE,gis%epse,0,hybrid,gen_coord_hybrid)  ! hmhj
 
!C
      CALL f_hpmstart(26,"STEP1")
!C
!!
      CALL countperf(1,18,0.)
!!
      CALL countperf(0,15,0.)

! Modified by Weiyu Yang to fix the bug related to the "runDuration".
!--------------------------------------------------------------------
      CALL ESMF_ClockGet(clock, timeStep    = timeStep,    &
                                startTime   = startTime,   &
                                currTime    = currTime,   &
                                rc          = rc1)

      runDuration_hour  = NINT(FHMAX) - NINT(FHINI)
      CALL ESMF_TimeIntervalSet(runDuration, h = runDuration_hour, rc = rc1)

!wy      CALL ESMF_ClockGet(clock, timeStep    = timeStep,    & 
!wy                                runDuration = runDuration, &
!wy                                startTime   = startTime,   &
!wy                                currTime    = currTime,   &
!wy                                rc          = rc1)
 
!
!     currTime = startTime
!
!     CALL ESMF_TimeIntervalGet(timeStep, s = timeStep_sec, rc = rc1)

!  print *,' timestep_sec=',timestep_sec,' rc1=',rc1

!wy CALL ESMF_TimeIntervalGet(runDuration, h = runDuration_hour, rc = rc1)

!  print *,' runduration_hour=',runduration_hour,' rc1=',rc1
!
!Moor ifhmax = NINT(gis%nam_gfs%FHMAX)
      ifhmax = NINT(FHMAX)
      IF(runDuration_hour <= 0    .OR.                  &
          ifhmax /= 0             .AND.                 &
          ifhmax <= gis%kfhour + runDuration_hour) THEN
!Moor     gis%nam_gfs%FHMAX = MAX(gis%nam_gfs%FHMAX, REAL(gis%kfhour))
! ,,      ifhmax            = NINT(gis%nam_gfs%FHMAX)
          ifhmax            = NINT(FHMAX)
! ,,      runDuration_hour  = ifhmax - gis%kfhour
          runDuration_hour  = NINT(FHMAX) - NINT(FHINI)
          CALL ESMF_TimeIntervalSet(runDuration, h = runDuration_hour, rc = rc1)
!  print *,' runduration_hour=',runduration_hour,' rc1=',rc1
      END IF
      if (runDuration_hour < 0) then
        print *,' FHINI=',FHINI, ' > FHMAX=',FHMAX,' JOB ABORTED'
        call mpi_quit(444)
      endif
!     stopTime = startTime + runDuration
      stopTime = currTime  + runDuration

      CALL ESMF_ClockSet(clock, stopTime = stopTime, &
!                               currTime = currTime, &
                                rc       = rc1)
!
      CALL ESMF_TimeIntervalGet(timeStep, s = timeStep_sec, rc = rc1)

      if (me == 0) print *,' timestep_sec=',timestep_sec,' rc1=',rc1
!!
      IF (me.eq.0) THEN
        CALL out_para(REAL(timeStep_sec))
      ENDIF
!!
      IF (me.eq.0) THEN
        PRINT *,' THE GSM WILL FORECAST ',runDuration_hour,' HOURS',      &
                ' FROM HOUR ',gis%kfhour,' TO HOUR ',runDuration_hour+gis%kfhour
      ENDIF
!
!
!CALL ESMF_TimeGet (stopTime, yy = yyc, mm = mmc, dd = ddc, h = hhc, &
!                             m = minsc, rc = rc1)
!PRINT*, ' In Initialize , stopTime=', yyc, mmc, ddc, hhc, minsc
!

!
      CALL synchro
      CALL countperf(1,15,0.)
!
!     zero fluxes and diagnostics
      CALL countperf(0,14,0.)
!
      gis%zhour  = fhour
      gis%FLUXR=0.
!
      call flx_init(gis%flx_fld,ierr)
!
      if (ldiag3d) then
        call d3d_zero
      endif
      CALL countperf(1,14,0.)
!
 END SUBROUTINE GFS_Initialize
!
 SUBROUTINE set_lonsgg(lonsperlat,lonsperlar,num_reduce,me)
      use resol_def
      use reduce_lons_grid_module, only : reduce_grid           ! hmhj
      integer num_reduce, me                                    ! hmhj
      integer lonsperlat(latg),lonsperlar(latr)

      integer lonsperlat_62(94),lonsperlar_62(94)
      integer lonsperlat_126(190),lonsperlar_126(190)
      integer lonsperlat_170(256),lonsperlar_170(256)
      integer lonsperlat_190(288),lonsperlar_190(288)
      integer lonsperlat_254(384),lonsperlar_254(384)
      integer lonsperlat_382(576),lonsperlar_382(576)
      integer lonsperlat_510(766),lonsperlar_510(766)
      integer lonsperlat_574(880),lonsperlar_574(880)

      data lonsperlat_62/                                                &
        30,  30,  30,  40,  48,  56,  60,  72,  72,  80,  90,  90,       &
        96, 110, 110, 120, 120, 128, 144, 144, 144, 144, 154, 160,       &
       160, 168, 168, 180, 180, 180, 180, 180, 180, 192, 192, 192,       &
       192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 47*0/

      data lonsperlar_62/                                                &
        30,  30,  30,  40,  48,  56,  60,  72,  72,  80,  90,  90,       &
        96, 110, 110, 120, 120, 128, 144, 144, 144, 144, 154, 160,       &
       160, 168, 168, 180, 180, 180, 180, 180, 180, 192, 192, 192,       &
       192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 47*0/

      data lonsperlat_126      /                                         &
          30,   30,   36,   48,   56,   60,   72,   72,   80,   90,      &
          96,  110,  110,  120,  120,  128,  144,  144,  154,  160,      &
         160,  180,  180,  180,  192,  192,  210,  210,  220,  220,      &
         240,  240,  240,  240,  240,  252,  256,  280,  280,  280,      &
         280,  288,  288,  288,  288,  308,  308,  308,  320,  320,      &
         320,  320,  330,  330,  360,  360,  360,  360,  360,  360,      &
         360,  360,  360,  360,  360,  360,  384,  384,  384,  384,      &
         384,  384,  384,  384,  384,  384,  384,  384,  384,  384,      &
         384,  384,  384,  384,  384,  384,  384,  384,  384,  384,      &
         384,  384,  384,  384,  384, 95*0 /
 
      data lonsperlar_126      /                                         &
          30,   30,   36,   48,   56,   60,   72,   72,   80,   90,      &
          96,  110,  110,  120,  120,  128,  144,  144,  154,  160,      &
         160,  180,  180,  180,  192,  192,  210,  210,  220,  220,      &
         240,  240,  240,  240,  240,  252,  256,  280,  280,  280,      &
         280,  288,  288,  288,  288,  308,  308,  308,  320,  320,      &
         320,  320,  330,  330,  360,  360,  360,  360,  360,  360,      &
         360,  360,  360,  360,  360,  360,  384,  384,  384,  384,      &
         384,  384,  384,  384,  384,  384,  384,  384,  384,  384,      &
         384,  384,  384,  384,  384,  384,  384,  384,  384,  384,      &
         384,  384,  384,  384,  384, 95*0 /
 
      data lonsperlat_170 /                                              &
         48,  48,  48,  48,  48,  56,  60,  72,  72,  80,  90,  96,      &
        110, 110, 120, 120, 128, 144, 144, 144, 154, 160, 168, 180,      &
        180, 180, 192, 210, 210, 220, 220, 240, 240, 240, 240, 240,      &
        252, 256, 280, 280, 280, 288, 288, 288, 308, 308, 320, 320,      &
        320, 320, 330, 360, 360, 360, 360, 360, 360, 360, 384, 384,      &
        384, 384, 384, 384, 420, 420, 420, 440, 440, 440, 440, 440,      &
        440, 440, 440, 440, 462, 462, 462, 462, 462, 480, 480, 480,      &
        480, 480, 480, 480, 480, 480, 480, 480, 504, 504, 504, 504,      &
        504, 504, 504, 504, 504, 512, 512, 512, 512, 512, 512, 512,      &
        512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512,      &
        512, 512, 512, 512, 512, 512, 512, 512, 128*0 /
 
      data lonsperlar_170 /                                              &
         48,  48,  48,  48,  48,  56,  60,  72,  72,  80,  90,  96,      &
        110, 110, 120, 120, 128, 144, 144, 144, 154, 160, 168, 180,      &
        180, 180, 192, 210, 210, 220, 220, 240, 240, 240, 240, 240,      &
        252, 256, 280, 280, 280, 288, 288, 288, 308, 308, 320, 320,      &
        320, 320, 330, 360, 360, 360, 360, 360, 360, 360, 384, 384,      &
        384, 384, 384, 384, 420, 420, 420, 440, 440, 440, 440, 440,      &
        440, 440, 440, 440, 462, 462, 462, 462, 462, 480, 480, 480,      &
        480, 480, 480, 480, 480, 480, 480, 480, 504, 504, 504, 504,      &
        504, 504, 504, 504, 504, 512, 512, 512, 512, 512, 512, 512,      &
        512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512, 512,      &
        512, 512, 512, 512, 512, 512, 512, 512, 128*0 /
 
      data lonsperlat_190 /                                              &
        64,  64,  64,  64,  64,  64,  64,  70,  80,  84,                 &
        88, 110, 110, 110, 120, 126, 132, 140, 144, 154,                 &
       160, 168, 176, 176, 192, 192, 198, 210, 210, 220,                 &
       220, 240, 240, 240, 252, 252, 256, 264, 280, 280,                 &
       280, 288, 308, 308, 308, 320, 320, 320, 330, 336,                 &
       352, 352, 352, 352, 360, 384, 384, 384, 384, 384,                 &
       396, 396, 420, 420, 420, 420, 420, 440, 440, 440,                 &
       440, 440, 448, 448, 462, 462, 462, 480, 480, 480,                 &
       480, 480, 504, 504, 504, 504, 504, 504, 504, 512,                 &
       512, 528, 528, 528, 528, 528, 528, 560, 560, 560,                 &
       560, 560, 560, 560, 560, 560, 560, 560, 560, 560,                 &
       560, 576, 576, 576, 576, 576, 576, 576, 576, 576,                 &
       576, 576, 576, 576, 576, 576, 576, 576, 576, 576,                 &
       576, 576, 576, 576, 576, 576, 576, 576, 576, 576,                 &
       576, 576, 576, 576, 144*   0/
!
      data lonsperlar_190 /                                              &
        64,  64,  64,  64,  64,  64,  64,  70,  80,  84,                 &
        88, 110, 110, 110, 120, 126, 132, 140, 144, 154,                 &
       160, 168, 176, 176, 192, 192, 198, 210, 210, 220,                 &
       220, 240, 240, 240, 252, 252, 256, 264, 280, 280,                 &
       280, 288, 308, 308, 308, 320, 320, 320, 330, 336,                 &
       352, 352, 352, 352, 360, 384, 384, 384, 384, 384,                 &
       396, 396, 420, 420, 420, 420, 420, 440, 440, 440,                 &
       440, 440, 448, 448, 462, 462, 462, 480, 480, 480,                 &
       480, 480, 504, 504, 504, 504, 504, 504, 504, 512,                 &
       512, 528, 528, 528, 528, 528, 528, 560, 560, 560,                 &
       560, 560, 560, 560, 560, 560, 560, 560, 560, 560,                 &
       560, 576, 576, 576, 576, 576, 576, 576, 576, 576,                 &
       576, 576, 576, 576, 576, 576, 576, 576, 576, 576,                 &
       576, 576, 576, 576, 576, 576, 576, 576, 576, 576,                 &
       576, 576, 576, 576, 144*   0/
 
      data lonsperlat_254      /                                         &
          64,   64,   64,   64,   64,   64,   72,   72,   80,   90,      &
          96,  110,  110,  120,  120,  128,  144,  144,  154,  160,      &
         168,  180,  180,  180,  192,  192,  210,  220,  220,  240,      &
         240,  240,  240,  252,  256,  280,  280,  280,  288,  288,      &
         288,  308,  308,  320,  320,  320,  330,  360,  360,  360,      &
         360,  360,  360,  384,  384,  384,  384,  420,  420,  420,      &
         440,  440,  440,  440,  440,  440,  462,  462,  462,  480,      &
         480,  480,  480,  480,  480,  504,  504,  504,  504,  512,      &
         512,  560,  560,  560,  560,  560,  560,  576,  576,  576,      &
         576,  576,  576,  576,  576,  616,  616,  616,  616,  616,      &
         616,  640,  640,  640,  640,  640,  640,  640,  640,  640,      &
         640,  660,  660,  660,  720,  720,  720,  720,  720,  720,      &
         720,  720,  720,  720,  720,  720,  720,  720,  720,  720,      &
         720,  720,  720,  720,  720,  720,  720,  720,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  192*0/
 
      data lonsperlar_254      /                                         &
          64,   64,   64,   64,   64,   64,   72,   72,   80,   90,      &
          96,  110,  110,  120,  120,  128,  144,  144,  154,  160,      &
         168,  180,  180,  180,  192,  192,  210,  220,  220,  240,      &
         240,  240,  240,  252,  256,  280,  280,  280,  288,  288,      &
         288,  308,  308,  320,  320,  320,  330,  360,  360,  360,      &
         360,  360,  360,  384,  384,  384,  384,  420,  420,  420,      &
         440,  440,  440,  440,  440,  440,  462,  462,  462,  480,      &
         480,  480,  480,  480,  480,  504,  504,  504,  504,  512,      &
         512,  560,  560,  560,  560,  560,  560,  576,  576,  576,      &
         576,  576,  576,  576,  576,  616,  616,  616,  616,  616,      &
         616,  640,  640,  640,  640,  640,  640,  640,  640,  640,      &
         640,  660,  660,  660,  720,  720,  720,  720,  720,  720,      &
         720,  720,  720,  720,  720,  720,  720,  720,  720,  720,      &
         720,  720,  720,  720,  720,  720,  720,  720,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  768,  768,  768,  768,  768,  768,  768,  768,      &
         768,  768,  192*0/
 
      data lonsperlat_382      /                                         &
         64,  64,  64,  64,  64,  64,  64,  70,  80,  84,                &
         88,  96, 110, 110, 120, 126, 132, 140, 144, 154,                &
        160, 168, 176, 180, 192, 192, 198, 210, 220, 220,                &
        224, 240, 240, 252, 252, 256, 264, 280, 280, 280,                &
        288, 308, 308, 308, 320, 320, 330, 336, 352, 352,                &
        352, 360, 384, 384, 384, 384, 396, 396, 420, 420,                &
        420, 420, 440, 440, 440, 448, 448, 462, 462, 480,                &
        480, 480, 504, 504, 504, 504, 512, 528, 528, 528,                &
        560, 560, 560, 560, 560, 560, 576, 576, 616, 616,                &
        616, 616, 616, 616, 616, 616, 630, 630, 640, 640,                &
        660, 660, 660, 660, 672, 672, 704, 704, 704, 704,                &
        704, 704, 720, 720, 720, 768, 768, 768, 768, 768,                &
        768, 768, 768, 768, 768, 792, 792, 792, 792, 792,                &
        840, 840, 840, 840, 840, 840, 840, 840, 840, 840,                &
        880, 880, 880, 880, 880, 880, 880, 880, 880, 880,                &
        896, 896, 896, 896, 924, 924, 924, 924, 924, 924,                &
        960, 960, 960, 960, 960, 960, 960, 960, 960, 960,                &
        990, 990, 990, 990, 990, 990, 990, 990,1008,1008,                &
       1008,1008,1008,1008,1024,1024,1024,1024,1024,1024,                &
       1056,1056,1056,1056,1056,1056,1056,1056,1056,1056,                &
       1120,1120,1120,1120,1120,1120,1120,1120,1120,1120,                &
       1120,1120,1120,1120,1120,1120,1120,1120,1120,1120,                &
       1120,1120,1120,1120,1120,1120,1120,1120,1120,1120,                &
       1120,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152, 288*   0/
 
      data lonsperlar_382      /                                         &
         64,  64,  64,  64,  64,  64,  64,  70,  80,  84,                &
         88,  96, 110, 110, 120, 126, 132, 140, 144, 154,                &
        160, 168, 176, 180, 192, 192, 198, 210, 220, 220,                &
        224, 240, 240, 252, 252, 256, 264, 280, 280, 280,                &
        288, 308, 308, 308, 320, 320, 330, 336, 352, 352,                &
        352, 360, 384, 384, 384, 384, 396, 396, 420, 420,                &
        420, 420, 440, 440, 440, 448, 448, 462, 462, 480,                &
        480, 480, 504, 504, 504, 504, 512, 528, 528, 528,                &
        560, 560, 560, 560, 560, 560, 576, 576, 616, 616,                &
        616, 616, 616, 616, 616, 616, 630, 630, 640, 640,                &
        660, 660, 660, 660, 672, 672, 704, 704, 704, 704,                &
        704, 704, 720, 720, 720, 768, 768, 768, 768, 768,                &
        768, 768, 768, 768, 768, 792, 792, 792, 792, 792,                &
        840, 840, 840, 840, 840, 840, 840, 840, 840, 840,                &
        880, 880, 880, 880, 880, 880, 880, 880, 880, 880,                &
        896, 896, 896, 896, 924, 924, 924, 924, 924, 924,                &
        960, 960, 960, 960, 960, 960, 960, 960, 960, 960,                &
        990, 990, 990, 990, 990, 990, 990, 990,1008,1008,                &
       1008,1008,1008,1008,1024,1024,1024,1024,1024,1024,                &
       1056,1056,1056,1056,1056,1056,1056,1056,1056,1056,                &
       1120,1120,1120,1120,1120,1120,1120,1120,1120,1120,                &
       1120,1120,1120,1120,1120,1120,1120,1120,1120,1120,                &
       1120,1120,1120,1120,1120,1120,1120,1120,1120,1120,                &
       1120,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152,1152,1152,                &
       1152,1152,1152,1152,1152,1152,1152,1152, 288*   0/
 
      data lonsperlat_510      /                                         &
          64,   64,   64,   64,   64,   64,   72,   72,   80,   90,      &
          96,  110,  110,  120,  120,  128,  144,  144,  154,  160,      &
         168,  180,  180,  180,  192,  210,  210,  220,  220,  240,      &
         240,  240,  240,  252,  256,  280,  280,  288,  288,  288,      &
         308,  308,  320,  320,  320,  330,  360,  360,  360,  360,      &
         360,  384,  384,  384,  384,  420,  420,  440,  440,  440,      &
         440,  440,  440,  462,  462,  462,  480,  480,  480,  480,      &
         504,  504,  504,  504,  512,  512,  560,  560,  560,  560,      &
         576,  576,  576,  576,  576,  576,  616,  616,  616,  616,      &
         640,  640,  640,  640,  640,  640,  640,  660,  720,  720,      &
         720,  720,  720,  720,  720,  720,  720,  720,  720,  720,      &
         720,  768,  768,  768,  768,  768,  768,  768,  768,  840,      &
         840,  840,  840,  840,  840,  840,  840,  880,  880,  880,      &
         880,  880,  880,  880,  880,  880,  880,  924,  924,  924,      &
         924,  924,  924,  924,  960,  960,  960,  960,  960,  960,      &
         960,  960,  960,  960,  960,  990,  990,  990, 1008, 1008,      &
        1008, 1008, 1008, 1024, 1024, 1024, 1024, 1024, 1120, 1120,      &
        1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120,      &
        1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152,      &
        1152, 1152, 1152, 1152, 1152, 1152, 1232, 1232, 1232, 1232,      &
        1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1260, 1260,      &
        1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260,      &
        1280, 1280, 1280, 1280, 1280, 1280, 1280, 1280, 1280, 1320,      &
        1320, 1320, 1320, 1386, 1386, 1386, 1386, 1386, 1386, 1386,      &
        1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386,      &
        1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440,      &
        1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440,      &
        1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440,      &
        1440, 1440, 1440, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536,  383*0/
 
      data lonsperlar_510      /                                         &
          64,   64,   64,   64,   64,   64,   72,   72,   80,   90,      &
          96,  110,  110,  120,  120,  128,  144,  144,  154,  160,      &
         168,  180,  180,  180,  192,  210,  210,  220,  220,  240,      &
         240,  240,  240,  252,  256,  280,  280,  288,  288,  288,      &
         308,  308,  320,  320,  320,  330,  360,  360,  360,  360,      &
         360,  384,  384,  384,  384,  420,  420,  440,  440,  440,      &
         440,  440,  440,  462,  462,  462,  480,  480,  480,  480,      &
         504,  504,  504,  504,  512,  512,  560,  560,  560,  560,      &
         576,  576,  576,  576,  576,  576,  616,  616,  616,  616,      &
         640,  640,  640,  640,  640,  640,  640,  660,  720,  720,      &
         720,  720,  720,  720,  720,  720,  720,  720,  720,  720,      &
         720,  768,  768,  768,  768,  768,  768,  768,  768,  840,      &
         840,  840,  840,  840,  840,  840,  840,  880,  880,  880,      &
         880,  880,  880,  880,  880,  880,  880,  924,  924,  924,      &
         924,  924,  924,  924,  960,  960,  960,  960,  960,  960,      &
         960,  960,  960,  960,  960,  990,  990,  990, 1008, 1008,      &
        1008, 1008, 1008, 1024, 1024, 1024, 1024, 1024, 1120, 1120,      &
        1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120, 1120,      &
        1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152, 1152,      &
        1152, 1152, 1152, 1152, 1152, 1152, 1232, 1232, 1232, 1232,      &
        1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1260, 1260,      &
        1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1260,      &
        1280, 1280, 1280, 1280, 1280, 1280, 1280, 1280, 1280, 1320,      &
        1320, 1320, 1320, 1386, 1386, 1386, 1386, 1386, 1386, 1386,      &
        1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386,      &
        1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440,      &
        1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440,      &
        1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440, 1440,      &
        1440, 1440, 1440, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536,  383*0/

      data lonsperlat_574      /                                         &
          18,   28,   32,   42,   48,   56,   64,   72,   80,   84,      &
          90,  110,  110,  110,  120,  126,  132,  140,  144,  154,      &
         160,  168,  176,  176,  192,  192,  198,  210,  210,  220,      &
         224,  240,  240,  252,  252,  256,  264,  280,  280,  288,      &
         288,  308,  308,  308,  320,  320,  330,  330,  352,  352,      &
         352,  360,  384,  384,  384,  384,  396,  396,  420,  420,      &
         420,  420,  440,  440,  440,  448,  462,  462,  462,  480,      &
         480,  480,  504,  504,  504,  504,  512,  528,  528,  528,      &
         560,  560,  560,  560,  560,  576,  576,  576,  616,  616,      &
         616,  616,  616,  616,  630,  630,  630,  640,  660,  660,      &
         660,  660,  672,  672,  704,  704,  704,  704,  704,  720,      &
         720,  720,  768,  768,  768,  768,  768,  768,  768,  768,      &
         770,  792,  792,  792,  792,  840,  840,  840,  840,  840,      &
         840,  840,  840,  880,  880,  880,  880,  880,  880,  880,      &
         896,  896,  896,  896,  924,  924,  924,  924,  924,  960,      &
         960,  960,  960,  960,  960,  960,  990,  990,  990,  990,      &
         990, 1008, 1008, 1008, 1008, 1024, 1024, 1024, 1056, 1056,      &
        1056, 1056, 1056, 1056, 1120, 1120, 1120, 1120, 1120, 1120,      &
        1120, 1120, 1120, 1120, 1120, 1120, 1120, 1152, 1152, 1152,      &
        1152, 1152, 1152, 1152, 1232, 1232, 1232, 1232, 1232, 1232,      &
        1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232,      &
        1232, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1280, 1280,      &
        1280, 1280, 1280, 1320, 1320, 1320, 1320, 1320, 1320, 1320,      &
        1320, 1320, 1344, 1344, 1344, 1344, 1344, 1344, 1386, 1386,      &
        1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1408,      &
        1408, 1408, 1408, 1408, 1440, 1440, 1440, 1440, 1440, 1440,      &
        1440, 1440, 1440, 1440, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1584, 1584, 1584, 1584, 1584, 1584,      &
        1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584,      &
        1584, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,      &
        1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,      &
        1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,      &
        1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,      &
        1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        440*0/

      data lonsperlar_574      /                                         &
          18,   28,   32,   42,   48,   56,   64,   72,   80,   84,      &
          90,  110,  110,  110,  120,  126,  132,  140,  144,  154,      &
         160,  168,  176,  176,  192,  192,  198,  210,  210,  220,      &
         224,  240,  240,  252,  252,  256,  264,  280,  280,  288,      &
         288,  308,  308,  308,  320,  320,  330,  330,  352,  352,      &
         352,  360,  384,  384,  384,  384,  396,  396,  420,  420,      &
         420,  420,  440,  440,  440,  448,  462,  462,  462,  480,      &
         480,  480,  504,  504,  504,  504,  512,  528,  528,  528,      &
         560,  560,  560,  560,  560,  576,  576,  576,  616,  616,      &
         616,  616,  616,  616,  630,  630,  630,  640,  660,  660,      &
         660,  660,  672,  672,  704,  704,  704,  704,  704,  720,      &
         720,  720,  768,  768,  768,  768,  768,  768,  768,  768,      &
         770,  792,  792,  792,  792,  840,  840,  840,  840,  840,      &
         840,  840,  840,  880,  880,  880,  880,  880,  880,  880,      &
         896,  896,  896,  896,  924,  924,  924,  924,  924,  960,      &
         960,  960,  960,  960,  960,  960,  990,  990,  990,  990,      &
         990, 1008, 1008, 1008, 1008, 1024, 1024, 1024, 1056, 1056,      &
        1056, 1056, 1056, 1056, 1120, 1120, 1120, 1120, 1120, 1120,      &
        1120, 1120, 1120, 1120, 1120, 1120, 1120, 1152, 1152, 1152,      &
        1152, 1152, 1152, 1152, 1232, 1232, 1232, 1232, 1232, 1232,      &
        1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232, 1232,      &
        1232, 1260, 1260, 1260, 1260, 1260, 1260, 1260, 1280, 1280,      &
        1280, 1280, 1280, 1320, 1320, 1320, 1320, 1320, 1320, 1320,      &
        1320, 1320, 1344, 1344, 1344, 1344, 1344, 1344, 1386, 1386,      &
        1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1386, 1408,      &
        1408, 1408, 1408, 1408, 1440, 1440, 1440, 1440, 1440, 1440,      &
        1440, 1440, 1440, 1440, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536, 1536,      &
        1536, 1536, 1536, 1536, 1584, 1584, 1584, 1584, 1584, 1584,      &
        1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584, 1584,      &
        1584, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,      &
        1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,      &
        1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,      &
        1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680,      &
        1680, 1680, 1680, 1680, 1680, 1680, 1680, 1680, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760, 1760,      &
        440*0/


      integer i
      if (num_reduce < 0) then
        if (jcap .eq. 62) then
          lonsperlat=lonsperlat_62
          lonsperlar=lonsperlar_62
        endif
        if (jcap .eq. 126) then
          lonsperlat=lonsperlat_126
          lonsperlar=lonsperlar_126
        endif
        if (jcap .eq. 170) then
          lonsperlat=lonsperlat_170
          lonsperlar=lonsperlar_170
        endif
        if (jcap .eq. 190) then
          lonsperlat=lonsperlat_190
          lonsperlar=lonsperlar_190
        endif
        if (jcap .eq. 254) then
          lonsperlat=lonsperlat_254
          lonsperlar=lonsperlar_254
        endif
        if (jcap .eq. 382) then
          lonsperlat=lonsperlat_382
          lonsperlar=lonsperlar_382
        endif
        if (jcap .eq. 510) then
          lonsperlat=lonsperlat_510
          lonsperlar=lonsperlar_510
        endif
        if (jcap .eq. 574) then
          lonsperlat=lonsperlat_574
          lonsperlar=lonsperlar_574
        endif
      endif

      if (jcap .ne. 62 .and. jcap .ne. 126 .and. jcap .ne. 170 .and.     &
          jcap .ne. 190 .and. jcap .ne. 574 .and.                        &
          jcap .ne. 254 .and. jcap .ne. 382 .and. jcap .ne. 510) then
!        print*,' Resolution not supported - lonsperlar/lonsperlat &
!        &data is needed in read_lonsgg '
!        stop 55
! compute reduced grid using juang 2003
         if ( me == 0 ) then
           print*,' Non Standard Resolution  - lonsperlar/lonsperlat',   &
                  ' computed locally'
         endif
         call reduce_grid (abs(num_reduce),jcap,latg,lonsperlat)        ! hmhj
         lonsperlar=lonsperlat                                          ! hmhj
         if ( me == 0 ) then
           print*,' Reduced grid is computed - lonsperlar/lonsperlat '  ! hmhj
         endif
      endif

      if ( me == 0 ) then
        print*,' jcap = ',jcap
        print*,'min,max of lonsperlat = ',minval(lonsperlat),            &
                maxval(lonsperlat)
        print*,'min,max of lonsperlar = ',minval(lonsperlar),            &
                maxval(lonsperlar)
      endif
 END SUBROUTINE set_lonsgg

 END MODULE GFS_Initialize_ESMFMod
