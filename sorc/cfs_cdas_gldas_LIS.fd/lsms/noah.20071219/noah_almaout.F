!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: noah_almaout.F90
!
! !DESCRIPTION:  
!  LIS NOAH data writer:  Binary and stat files in ALMA convention
!
! !REVISION HISTORY:
!  4 Nov. 1999: Jon Radakovich; Initial Code
! 28 Apr. 2002: Kristi Arsenault; Added NOAH LSM to LDAS
! 15 Jun  2003: Sujay Kumar; ALMA version 
! 
! !INTERFACE:
subroutine noah_almaout (ld,tile,gindex)
! !USES:
!  use netcdf
  use lis_module      ! LIS non-model-specific 1-D variables
  use tile_module      ! LIS non-model-specific tile variables
  use grid_module      ! LIS non-model-specific grid variables
  use noah_varder      ! NOAH-specific variables
  use time_manager, only : get_nstep
  use drv_output_mod, only : drv_writevar_bin

  implicit none 
! !ARGUMENTS:
  type (lisdec) LD
  type (tiledec) tile(ld%d%glbnch)
  integer :: gindex(ld%d%lnc, ld%d%lnr)
  logical*1 :: lismask(ld%d%lnc,ld%d%lnr)
!EOP

  integer :: t,c,r,m,i,n,iret,ftn
  integer :: varids(32)
  integer :: kpds(32,25)
  character*8 :: today, yesterday
  character*80 mkfyrmo,filenmt,filenmg,cdir,namet,nameg,filengb
  character*80 mkfyrmo2
  character*1  fname(80),fbase(40),fmkdir(80)
  character*1  ftime(8),fcd(3),frm(3),flats(13),ftimec(4)
  character*1  fyrmodir(26),fsubft(80)
  character*1  fsubfg(80),ftimeb(10),fsubgb(9)
  
  character (len=100) :: fbinname
  character (len=100) :: temp1

  integer,parameter :: nvarsg=29,nvarst=29,kmg=1
  character*80 :: vname(nvarsg)
  integer      :: prec,kbegt,kountt
  
  data vname / "SWnet(W/m2)","LWnet(W/m2)", & 
       "Qle(W/m2)","Qh(W/m2)","Qg(W/m2)", & 
       "Snowf(kg/m2s)","Rainf(kg/m2s)","Evap(kg/m2s)", & 
       "Qs(kg/m2s)","Qsb(kg/m2s)","Qsm(kg/m2s)", & 
       "DelSoilMoist(kg/m2)","DelSWE(kg/m2)", & 
       "SnowT(K)","VegT(K)","BareSoilT(K)","AvgSurfT(K)", & 
       "RadT(K)","Albedo(-)","SWE(kg/m2)", & 
       "SoilMoist1(kg/m2)","SoilMoist2(kg/m2)", & 
       "SoilMoist3(kg/m2)","SoilMoist4(kg/m2)","SoilWet(-)", &
       "TVeg(kg/m2s)","ESoil(kg/m2s)","RootMoist(kg/m2)","ACond(m/s)" /
  
  
  character*40 file
  character*80 name
!BOC
!-------------------------------------------------------------------------
! Test to see if output writing interval has been reached
!-------------------------------------------------------------------------
  if(mod(ld%t%gmt,noahdrv%writeintn).eq.0)then
     noahdrv%numoutnh=noahdrv%numoutnh+1    
     write(unit=temp1,fmt='(i4,i2,i2)')ld%t%yr,ld%t%mo,ld%t%da
     read(unit=temp1,fmt='(8a1)') ftime
     do i=1,8
        if(ftime(i).eq.(' '))ftime(i)='0'
     enddo
     write(unit=temp1,fmt='(i4)')ld%t%yr
     read(unit=temp1,fmt='(8a1)')ftimec
     do i=1,4
        if(ftimec(i).eq.(' '))ftimec(i)='0'
     enddo
     write(unit=temp1,fmt='(a6,i3,a1)') '/LIS.E',ld%o%expcode,'.'
     read(unit=temp1,fmt='(80a1)') (fname(i),i=1,10)
     do i=1,10
        if(fname(i).eq.(' '))fname(i)='0'
     enddo
     write(unit=temp1,fmt='(a40)') ld%o%odir
     read(unit=temp1,fmt='(40a1)') (fbase(i),i=1,40)
     c=0
     do i=1,40
        if(fbase(i).eq.(' ').and.c.eq.0)c=i-1
     enddo
     
     write(unit=temp1,fmt='(a4,i3,a6,i4,a1,i4,i2,i2)')'/EXP', & 
          ld%o%expcode,'/NOAH/', & 
          ld%t%yr,'/',ld%t%yr,ld%t%mo,ld%t%da
     read(unit=temp1,fmt='(80a1)') (fyrmodir(i),i=1,26)
     do i=1,26
        if(fyrmodir(i).eq.(' '))fyrmodir(i)='0'
     enddo
     
     write(unit=temp1,fmt='(a9)')'mkdir -p '
     read(unit=temp1,fmt='(80a1)')(fmkdir(i),i=1,9)
     
     write(unit=temp1,fmt='(80a1)')(fmkdir(i),i=1,9),(fbase(i),i=1,c), & 
          (fyrmodir(i),i=1,26)
     read(unit=temp1,fmt='(a80)')mkfyrmo
     call system(mkfyrmo)
!----------------------------------------------------------------------
! Generate file name for BINARY output
!----------------------------------------------------------------------
     write(unit=fbinname, fmt='(i4,i2,i2,i2)') ld%t%yr,ld%t%mo, & 
          ld%t%da,ld%t%hr
     read(unit=fbinname,fmt='(10a1)') ftimeb
     do i=1,10
        if(ftimeb(i).eq.(' '))ftimeb(i)='0'
     enddo
     if(ld%o%wout.eq.1) then
        write(unit=fbinname,fmt='(a9)') '.NOAHgbin'
        read(unit=fbinname,fmt='(80a1)') (fsubgb(i),i=1,9)
     elseif(ld%o%wout.eq.2) then 
        write(unit=fbinname,fmt='(a9)') '.NOAH.grb'
        read(unit=fbinname,fmt='(80a1)') (fsubgb(i),i=1,9)
     elseif(ld%o%wout.eq.3) then         
        write(unit=fbinname,fmt='(a8)') '.NOAH.nc'
        read(unit=fbinname,fmt='(80a1)') (fsubgb(i),i=1,9)
     endif
     write(unit=fbinname,fmt='(80a1)')(fbase(i),i=1,c), & 
          (fyrmodir(i),i=1,26), & 
          (fname(i),i=1,10),(ftimeb(i),i=1,10), & 
          (fsubgb(i),i=1,9)
     read(unit=fbinname,fmt='(a80)')filengb
!-----------------------------------------------------------------------
! Open statistical output file
!-----------------------------------------------------------------------
     if(noahdrv%noahopen.eq.0)then
        file='Noahstats.dat'
        call openfile(name,ld%o%odir,ld%o%expcode,file)
        if(ld%o%startcode.eq.1)then
           open(65,file=name,form='formatted',status='unknown', & 
                position='append')
        else
           open(65,file=name,form='formatted',status='replace')       
        endif
        noahdrv%noahopen=1
     endif
          
       write(65,996)'       Statistical Summary of Noah output for:  ', & 
            ld%t%mo,'/',ld%t%da,'/',ld%t%yr,ld%t%hr,':',ld%t%mn,':',ld%t%ss
996    format(a47,i2,a1,i2,a1,i4,1x,i2,a1,i2,a1,i2)
       write(65,*)
       write(65,997)
997    format(t27,'Mean',t41,'Stdev',t56,'Min',t70,'Max')
       ftn = 58
       if(ld%o%wout.eq.1) then
          open(ftn,file=filengb,form='unformatted')
          call noah_binout(ld,ftn)
          close(58)
       elseif(ld%o%wout.eq.2) then 
          ftn = ftn+ld%t%hr
          call baopen (ftn,filengb, iret)
          call noah_gribout(ld,ftn)
          call baclose(ftn,iret)
!       elseif(ld%o%wout.eq.3) then !netcdf
!          iret = nf90_create(path=trim(filengb),cmode=nf90_clobber,ncid=ftn)
!          call noah_netcdfout(ld,ftn)
!          iret = nf90_close(ftn)
       endif
       call noah_writestats(ld,65)
       noah%count=0  !reset counters
       write(65,*)
       write(65,*)

    endif
!EOC
  end subroutine noah_almaout
  
