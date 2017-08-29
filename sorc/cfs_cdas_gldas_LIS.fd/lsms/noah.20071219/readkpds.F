!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: readkpds.F90
! 
! !DESCRIPTION: 
!   Reads the kpds array from the grib table
!
! !REVISION HISTORY:
!
! 23 Oct 2003; Sujay Kumar; Initial Version
!
! !INTERFACE:
  subroutine readkpds(ftn, kpds)
!
! !INTERFACE:
    use noah_varder, only : noahdrv
!EOP
    integer     :: kpds(25),ftn

    read (ftn, 15) kpds(5), kpds(6), kpds(7), kpds(14), &
         kpds(15), kpds(16), kpds(22)
    if(kpds(16).ne.0) kpds(15)=noahdrv%writeintn
    
15   format (29x, 7i6)     
  end subroutine readkpds
