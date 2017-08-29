!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: noah_alb.F90 
!
! !DESCRIPTION:
!  This subroutine takes surface snow free albedo data and the date to 
!  interpolate and determine the actual value of the albedo 
!  for that date.  This actual value is then returned to the main
!  program.  The assumption is that the data point is valid for the 15th
!  of the given month, at 00Z.
!
! !REVISION HISTORY:
!
!  28 Apr 2002: K. Arsenault; Added NOAH LSM to LDAS, initial code
!  18 Jun 2004: J. Meng; Move valid day to 15th day of month
!
! !INTERFACE:
subroutine noah_alb
! !USES:
  use noah_varder      
  use time_manager
!NOT USED IN 3.1  use time_module
  use lisdrv_module, only : grid,tile,lis
#if ( defined OPENDAP )
  use opendap_module
#endif
!EOP
  implicit none
!=== Local Variables =====================================================
  real,allocatable :: albout(:,:)
  integer :: cindex, rindex

  INTEGER :: I,J,T,C,R            ! Loop counters
  REAL*8  :: TIME1,TIME2          ! Temporary Time variables
  INTEGER :: YR1,MO1,YR2,MO2      ! Temporary Time variables
  INTEGER :: DOY1,DOY2            ! Temporary Time variables
  INTEGER :: ZEROI,NUMI           ! Integer Number Holders
  REAL :: WT1,WT2,GMT1,GMT2       ! Interpolation weights
#if ( defined OPENDAP )
  REAL :: VALUE1(parm_nc,1+nroffset:parm_nr+nroffset) ! Temporary value holder for MO1
  REAL :: VALUE2(parm_nc,1+nroffset:parm_nr+nroffset) ! Temporary value holder for MO2
#else
  REAL :: VALUE1(LIS%D%GNC,LIS%D%GNR)   ! Temporary value holder for MO1
  REAL :: VALUE2(LIS%D%GNC,LIS%D%GNR)   ! Temporary value holder for MO2
#endif
  CHARACTER*2 :: MM1,MM2        ! Filename places for integer. MO1, MO2
  INTEGER alb_flag

#if ( ! defined OPENDAP )
  integer :: tnroffset = 0
#endif

!=== End Variable Definition =============================================
!BOC
  noahdrv%noah_aflag = 0
  zeroi=0
  numi=15
!------------------------------------------------------------------------
! Determine Monthly data Times (Assume Monthly value valid at DA=15)
!------------------------------------------------------------------------
  if(lis%t%da.lt.15)then
     mo1=lis%t%mo-1
     yr1=lis%t%yr 
     if(mo1.eq.0)then
        mo1=12
        yr1=lis%t%yr-1
     endif
     mo2=lis%t%mo
     yr2=lis%t%yr
  else
     mo1=lis%t%mo
     yr1=lis%t%yr
     mo2=lis%t%mo+1
     yr2=lis%t%yr
     if(mo2.eq.13)then
        mo2=1
        yr2=lis%t%yr+1
     endif
  endif
  
  call date2time(time1,doy1,gmt1,yr1,mo1,&
       numi,zeroi,zeroi,zeroi)
  call date2time(time2,doy2,gmt2,yr2,mo2,&
       numi,zeroi,zeroi,zeroi)
!------------------------------------------------------------------------  
!  Weights to be used to interpolate albedo values.
!------------------------------------------------------------------------
  wt1= (time2-lis%t%time)/(time2-time1)
  wt2= (lis%t%time-time1)/(time2-time1)
!------------------------------------------------------------------------
!  Determine if ALBEDO files need to be updated
!------------------------------------------------------------------------
  if(time2 .gt. noahdrv%noah_albtime) then 
     alb_flag = 1
  else 
     alb_flag = 0
  endif
  
  if(alb_flag .eq. 1) then 
     noahdrv%noah_albtime = time2
     noahdrv%noah_aflag = 1
!------------------------------------------------------------------------
! Open albedo dataset of months corresponding to   
! time1 and time2 for selected LDAS domain and read data.
!------------------------------------------------------------------------
     write(mm1,3) mo1
     write(mm2,3) mo2
3    format(i2.2)


#if ( defined opendap )
     print*, 'msg: noah_alb -- retrieving albedo file ',&
          trim(noahdrv%noah_albfile)//'albedo_'//mm1//'.bfsa',&
         ' (',iam,')'
 !    call system("opendap_scripts/getalbedo.pl "//ciam//" "// &
 !         trim(noahdrv%noah_albfile)//'albedo_'//mm1//'.bfsa'       &
 !         //" "//cparm_slat//" "//cparm_nlat                      &
 !         //" "//cparm_wlon//" "//cparm_elon//" "//mm1)
     print*, 'msg: noah_alb -- retrieving albedo file ', &
          trim(noahdrv%noah_albfile)//'albedo_'//mm2//'.bfsa',& 
          ' (',iam,')'
 !    call system("opendap_scripts/getalbedo.pl "//ciam//" "// &
 !        trim(noahdrv%noah_albfile)//'albedo_'//mm2//'.bfsa'       &
 !        //" "//cparm_slat//" "//cparm_nlat                      &
 !        //" "//cparm_wlon//" "//cparm_elon//" "//mm2)
#endif
     print*, 'msg: noah_alb -- retrieving albedo file ',&
          trim(noahdrv%noah_albfile)//'albedo_'//mm1//'.bfsa',&
          ' (',iam,')'
 !    open (10, &
 !         file=trim(noahdrv%noah_albfile)//'albedo_'//mm1//'.bfsa', & 
 !         status='unknown', form='unformatted')
     print*, 'msg: noah_alb -- retrieving albedo file ', &
          trim(noahdrv%noah_albfile)//'albedo_'//mm2//'.bfsa',& 
          ' (',iam,')'
 !    open (11, &
 !         file=trim(noahdrv%noah_albfile)//'albedo_'//mm2//'.bfsa', &
 !         status='unknown', form='unformatted')
     
! jesse - get albedo from  forcing sflux    
  !   read(10) value1
  !   read(10) value1
  !   read(10) value1
  !   read(11) value2
  !   read(11) value2
  !   read(11) value2
  !   close(10)
  !   close(11)
  value1 = 0.
  value2 = 0.
!------------------------------------------------------------------------     
! Assign MONTHLY albedo to each tile. 
!------------------------------------------------------------------------
     do i=1,lis%d%nch
        if((value1(tile(i)%col, tile(i)%row-tnroffset) .ne. -9999.000) & 
         .and.(value2(tile(i)%col, tile(i)%row-tnroffset).ne.-9999.000)) &
         then
           noah(i)%albsf1=value1(tile(i)%col, tile(i)%row-tnroffset) 
           noah(i)%albsf2=value2(tile(i)%col, tile(i)%row-tnroffset) 
        endif
     end do
  endif   
!------------------------------------------------------------------------
!  Interpolate albedo values once daily
!------------------------------------------------------------------------

  if (noahdrv%noah_albdchk .ne. lis%t%da) then 
     noahdrv%noah_aflag = 1
     do i=1,lis%d%nch
        noah(i)%albsf = (wt1*noah(i)%albsf1)+(wt2*noah(i)%albsf2)
     end do
     noahdrv%noah_albdchk = lis%t%da

     print*, 'Done noah_alb',' (',iam,')'

     if(lis%o%wparam.eq.1) then
        allocate(albout(lis%d%lnc,lis%d%lnr))
        albout=-9999.0
        do i=1,lis%d%nch      
!3.1
           rindex = tile(i)%row - (lis%d%gridDesc(4)-lis%d%gridDesc(44)) &
                /lis%d%gridDesc(9)
           cindex = tile(i)%col - (lis%d%gridDesc(5)-lis%d%gridDesc(45)) &
                /lis%d%gridDesc(10)
!           rindex = tile(i)%row - (lis%d%kgds(4)-lis%d%kgds(44)) &
!                /lis%d%kgds(9)
!           cindex = tile(i)%col - (lis%d%kgds(5)-lis%d%kgds(45)) &
!                /lis%d%kgds(10)
           albout(cindex,rindex) = noah(i)%albsf
        enddo
        open(32,file="albout.bin",form='unformatted')
        write(32) albout
        close(32)   
        deallocate(albout)
     endif
  end if
  return
!EOC
end subroutine noah_alb
