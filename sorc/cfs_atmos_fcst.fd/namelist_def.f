      module namelist_def
      use machine
      implicit none
      save
      integer nszer,nsres,nslwr,nsout,nsswr,nsdfi,nscyc,igen,jo3,ngptc
     &,       lsm,ens_mem,ncw(2),num_reduce
      real(kind=kind_evod) fhswr,fhlwr,fhrot,fhseg,fhmax,fhout,fhres,
     & fhzer,fhini,fhdfi,fhcyc,filta,crtrh(3),flgmin(2),ref_temp,ccwf,
     & ctei_rm,bkgd_vdif
      logical ldiag3d,ras,zhao_mic,sashal,newsas,crick_proof,ccnorm
      logical mom4ice,mstrat,trans_trac,climate
      logical lsfwd,lssav,lscca,lsswr,lslwr
      logical shuff_lats_a,shuff_lats_r,reshuff_lats_a,reshuff_lats_r
      logical hybrid,gen_coord_hybrid,zflxtvd				! hmhj
      logical run_enthalpy, out_virttemp                                ! hmhj
      logical adiab,explicit,pre_rad,random_xkt2,old_monin,cnvgwd       ! hmhj
      logical restart, gfsio_in, gfsio_out

      character*20 ens_nam
!
!     Radiation control parameters
!
      logical norad_precip
      integer isol, ico2, ialb, iems, iaer, iovr_sw, iovr_lw, ictm
      integer isubc_sw, isubc_lw
      end module namelist_def
