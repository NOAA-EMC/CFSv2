!-------------------------------------------------------------------------
! NASA Goddard Space Flight Center Land Information System (LIS) V3.0
! Released May 2004
!
! See SOFTWARE DISTRIBUTION POLICY for software distribution policies
!
! The LIS source code and documentation are in the public domain,
! available without fee for educational, research, non-commercial and
! commercial purposes.  Users may distribute the binary or source
! code to third parties provided this statement appears on all copies and
! that no charge is made for such copies.
!
! NASA GSFC MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE
! SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED AS IS WITHOUT EXPRESS OR
! IMPLIED WARRANTY.  NEITHER NASA GSFC NOR THE US GOVERNMENT SHALL BE
! LIABLE FOR ANY DAMAGES SUFFERED BY THE USER OF THIS SOFTWARE.
!
! See COPYRIGHT.TXT for copyright details.
!
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: readagrmetcrd.F90
!
! !DESCRIPTION:
!  Routine to read AGRMET specific parameters from the card file. 
!
! !REVISION HISTORY:
! 11 Dec 2003; Sujay Kumar, Initial Code
!
! !INTERFACE:    
subroutine readagrmetcrd(agrmetdrv)
! !USES:
  use agrmetdrv_module
#if ( defined OPENDAP )
    use agrmetopendap_module, only : opendap_agrmet_init
#endif
!EOP
  implicit none
  type(agrmetdrvdec) :: agrmetdrv
  namelist /agrmet/agrmetdrv
!BOC
  open(11,file='lis.crd',form='formatted',status='old')
  read(unit=11,NML=agrmet)
  print*,'Using AGRMET forcing'
  print*, 'AGRMET forcing directory :',agrmetdrv%AGRMETDIR
  agrmetdrv%AGRMTIME1  = 3000.0
  agrmetdrv%AGRMTIME2  = 0.0

  close(11)
#if ( defined OPENDAP )
    call opendap_agrmet_init()
#endif
!EOC
end subroutine readagrmetcrd
