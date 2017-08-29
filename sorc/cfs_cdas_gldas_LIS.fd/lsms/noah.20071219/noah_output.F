!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: noah_output.F90
! 
! !DESCRIPTION: This subroutines sets up methods to write noah
! output 
! 
! !INTERFACE:
subroutine noah_output
! !USES:
  use lisdrv_module, only : lis, tile, glbgindex
  use noah_varder, only : noahdrv
  use spmdMod, only : masterproc,npes
!EOP
  integer :: i 
  real :: var(lis%d%glbnch)
!BOC
  if(lis%o%wsingle ==1) then 
!------------------------------------------------------------------
! Writes each output variable to a separate file
!------------------------------------------------------------------
     if(mod(lis%t%gmt,noahdrv%writeintn).eq.0)then
        do i=1,32
           call noah_singlegather(i,var)
           if(masterproc) then 
              call noah_singleout(lis, tile, glbgindex, var, i)
           endif
        enddo
        call noah_totinit()
     endif
  else 
!------------------------------------------------------------------
! Writes bundled output
!------------------------------------------------------------------
     if(mod(lis%t%gmt,noahdrv%writeintn).eq.0)then
        if(npes > 1 ) then 
           call noah_gather()
        endif
        if(masterproc) then 
           call noah_almaout(lis, tile, glbgindex)
        endif
        call noah_totinit()
     endif
  endif
!EOC
end subroutine noah_output
