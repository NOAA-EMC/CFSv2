!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
! 
! !ROUTINE: noah_writerst.F90
!
! !DESCRIPTION:
!  This program writes restart files for NOAH.  This
!   includes all relevant water/energy storages, tile information,
!   and time information.  It also rectifies changes in the tile space.  
!
!
! !REVISION HISTORY:
!  1  Oct 1999: Jared Entin; Initial code
!  15 Oct 1999: Paul Houser; Significant F90 Revision
!  05 Sep 2001: Brian Cosgrove; Modified code to use Dag Lohmann's NOAA
!               initial conditions if necessary.  This is controlled with
!               local variable NOAAIC.  Normally set to 0 in this subroutine
!               but set to 1 if want to use Dag's NOAA IC's.  Changed output
!               directory structure, and commented out if-then check so that
!               directory is always made.
!  28 Apr 2002: Kristi Arsenault; Added NOAH LSM into LDAS
!  28 May 2002: Kristi Arsenault; For STARTCODE=4, corrected SNEQV values  
!                and put SMC, SH2O, STC limit for GDAS and GEOS forcing.
!  14 Jun 2003: Sujay Kumar , Separated the write restart from the original 
!                code
! RESTART FILE FORMAT(fortran sequential binary):
!  YR,MO,DA,HR,MN,SS,VCLASS,NCH !Restart time,Veg class,no.tiles, no.soil lay 
!  TILE(NCH)%COL        !Grid Col of Tile   
!  TILE(NCH)%ROW        !Grid Row of Tile
!  TILE(NCH)%FGRD       !Fraction of Grid covered by tile
!  TILE(NCH)%VEGT       !Vegetation Type of Tile
!  NOAH(NCH)%STATES     !Model States in Tile Space
! 
! !INTERFACE:
subroutine noah_writerst()
! !uses:
  use lisdrv_module, only : lis,tile
  use lis_module
!3.1  use time_module
  USE noah_varder      ! NOAH tile variables
  use time_manager
  use tile_spmdMod
!EOP
  IMPLICIT NONE      

!=== Local Variables =====================================================
  integer :: c,r,t,i,j,l,n,f ! loop counters


  character*80 filen,mkfyrmo
  character*1  fname(80),fbase(80),fsubs(80),fmkdir(80)
  character*1  ftime(10),fyrmodir(80)
!=== Temporary tile space transfer files (different than in lis_module)      
  real, allocatable :: tmptilen(:)
  CHARACTER (LEN=100) :: temp
!=== Variables for noah_binout 
  integer ftn
  character*80 fileb
!=== End Variable Definition =============================================
!BOC
  if(masterproc) then 
!-------------------------------------------------------------------------
! Restart Writing (2 files are written = active and archive)
!-------------------------------------------------------------------------
    PRINT*,"J---noah_writerst"
    PRINT*,"lis%t%gmt = ", lis%t%gmt
    PRINT*,"noahdrv%writeintn = ", noahdrv%writeintn
!JESSE 20071219 WRITERST AT 00Z ONLY
!     if((lis%t%gmt.eq.(24-noahdrv%writeintn)) & 
     if((lis%t%gmt.eq.0.) & 
          .or.lis%t%endtime.eq.1)then
        allocate(tmptilen(lis%d%nch))
    !    open(40,file=noahdrv%noah_rfile,form='unformatted') !Active archive restart
    !    call timemgr_write_restart(40)
    !
    !    write(40) lis%p%vclass,lis%d%lnc,lis%d%lnr,lis%d%nch  !Veg class, no tiles       
    !    write(40) noah%t1        !NOAH Skin Temperature (K)
    !    write(40) noah%cmc       !NOAH Canopy Water Content
    !    write(40) noah%snowh     !NOAH Actual Snow Depth
    !    write(40) noah%sneqv     !NOAH Water Equivalent Snow Depth
    !    do l=1,4
    !      do t=1,lis%d%nch
    !        tmptilen(t)=noah(t)%stc(l)
    !      enddo
    !      write(40) tmptilen  !NOAH Soil Temperature (4 layers)
    !    enddo
    !    do l=1,4
    !      do t=1,lis%d%nch
    !        tmptilen(t)=noah(t)%smc(l)
    !      enddo
    !      write(40) tmptilen  !NOAH Total Soil Moist. (4 layers)
    !    enddo
    !    do l=1,4
    !      do t=1,lis%d%nch
    !        tmptilen(t)=noah(t)%sh2o(l)
    !      enddo
    !      write(40) tmptilen  !NOAH Liquid Soil Moist. (4 layers)
    !    enddo
    !    write(40) noah%ch      !NOAH Heat/Moisture Sfc Exchange Coef.
    !    write(40) noah%cm      !NOAH Momentum Sfc Exchange Coef.
    !    close(40)   
    !    write(*,*)'Noah Active restart written: ',noahdrv%noah_rfile
        write(unit=temp,fmt='(i4,i2,i2,i2)') lis%t%yr,lis%t%mo, & 
            lis%t%da,lis%t%hr
        read(unit=temp,fmt='(10a1)')ftime
        do i=1,10
          if(ftime(i).eq.(' '))ftime(i)='0'
        enddo
        write(unit=temp,fmt='(a4,i3,a6,i4,a1,i4,i2,i2,a6,i3,a1)') & 
            '/EXP',lis%o%expcode,'/NOAH/',lis%t%yr, & 
            '/',lis%t%yr,lis%t%mo, & 
            lis%t%da,'/LIS.E',lis%o%expcode,'.'
        read(unit=temp,fmt='(80a1)') (fname(i),i=1,37)
        do i=1,37
          if(fname(i).eq.(' '))fname(i)='0'
        enddo
        write(unit=temp,fmt='(a9)')'mkdir -p '
        read(unit=temp,fmt='(80a1)')(fmkdir(i),i=1,9)
        write(unit=temp,fmt='(a4,i3,a6,i4,a1,i4,i2,i2)') & 
            '/EXP',lis%o%expcode,'/NOAH/', & 
            lis%t%yr,'/',lis%t%yr,lis%t%mo,lis%t%da
        read(unit=temp,fmt='(80a1)') (fyrmodir(i),i=1,26)
        do i=1,26
          if(fyrmodir(i).eq.(' '))fyrmodir(i)='0'
        enddo

        write(unit=temp,fmt='(a8)')'.Noahrst'
        read(unit=temp,fmt='(80a1)') (fsubs(i),i=1,8)

        write(unit=temp,fmt='(a40)') lis%o%odir                       
        read(unit=temp,fmt='(80a1)') (fbase(i),i=1,80)
        c=0
        do i=1,80
          if(fbase(i).eq.(' ').and.c.eq.0)c=i-1
        enddo
        write(unit=temp,fmt='(80a1)')(fbase(i),i=1,c),(fname(i),i=1,36), & 
                        (ftime(i),i=1,10),(fsubs(i),i=1,8) 
        read(unit=temp,fmt='(a80)')filen
 
        write(unit=temp,fmt='(80a1)')(fmkdir(i),i=1,9),(fbase(i),i=1,c), &
         (fyrmodir(i),i=1,26)
        read(unit=temp,fmt='(a80)')mkfyrmo

!-------------------------------------------------------------------------
! Archive File Name Generation Complete
! Make the directories for the NOAH restart file
!-------------------------------------------------------------------------
        CALL SYSTEM(MKFYRMO)
!-------------------------------------------------------------------------
! Archive File Name Generation Complete
!-------------------------------------------------------------------------
        open(40,file=filen,status='unknown',form='unformatted')
        call timemgr_write_restart(40)
        write(40) lis%p%vclass,lis%d%lnc,lis%d%lnr,lis%d%nch  !veg class, no tiles       
        write(40) noah%t1        !noah skin temperature (k)
        write(40) noah%cmc       !noah canopy water content
        write(40) noah%snowh     !noah actual snow depth
        write(40) noah%sneqv     !noah water equivalent snow depth
        do l=1,4
           do t=1,lis%d%nch
              tmptilen(t)=noah(t)%stc(l)
           enddo
           write(40) tmptilen  !noah soil temperature (4 layers)
        enddo
        do l=1,4
           do t=1,lis%d%nch
              tmptilen(t)=noah(t)%smc(l)
           enddo
           write(40) tmptilen  !noah total soil moist. (4 layers)
        enddo
        do l=1,4
           do t=1,lis%d%nch
              tmptilen(t)=noah(t)%sh2o(l)
           enddo
           write(40) tmptilen  !noah liquid soil moist. (4 layers)
        enddo
        write(40) noah%ch       !noah heat/moisture sfc exchange coef.
        write(40) noah%cm        !noah momentum sfc exchange coef.
        
        close(40)
        
        write(*,*)'noah archive restart written: ',filen
        deallocate(tmptilen)
!-------------------------------------------------------------------------
! WRITE RESTART FILE IN 2D BINARY
!-------------------------------------------------------------------------
        ftn = 58
        write(fileb,'(a5,i3,a6,i4,a1,i4,i2.2,i2.2,a6,i3,a1,i4,3i2.2,a9)') & 
             './EXP',lis%o%expcode,'/NOAH/', &
             lis%t%yr,'/',lis%t%yr,lis%t%mo,lis%t%da, &
             '/LIS.E',lis%o%expcode,'.', &
             lis%t%yr,lis%t%mo,lis%t%da,lis%t%hr,'.NOAHgbin'
        open(ftn,file=fileb,form='unformatted')
        call noah_binout(lis,ftn)
        close(ftn)

      endif 
   endif
   return
!EOC
 end subroutine noah_writerst








