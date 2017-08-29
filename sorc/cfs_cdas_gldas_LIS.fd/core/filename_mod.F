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
!  !MODULE: filename_mod.F90
!  
!  !DESCRIPTION: 
!   Generates filenames for a number of routines. 
!
!  !REVISION HISTORY: 
!  13 Mar 2004   Sujay Kumar  Initial Specification
! 
!EOP
module filename_mod
contains
!BOP
! !ROUTINE: avhrr_file_1km
! 
! !DESCRIPTION:
! Generates the 1km filenames for AVHHR LAI data. 
! 
! !INTERFACE:
  subroutine avhrr_laifile_1km (name9,name10,name11,name12,&
       avhrrdir, cyr1, cyr2, cmo1, cmo2 )

    implicit none
! !ARGUMENTS:
    character(len=80), intent(out) :: name9,  name10, name11, name12
    character(len=40), intent(in)  :: avhrrdir 
    character(len=4), intent(in)   :: cyr1, cyr2
    character(len=2), intent(in)   :: cmo1, cmo2
!EOP
    character(len=100) :: temp1
    character(len=100) :: temp2
    character(len=100) :: temp3
    character(len=100) :: temp4
    integer :: i,c

    character*1 :: fbase(80), fbase_2(80), fbase_3(80), fbase_4(80)
    character*1 :: fdir(7), fdir_2(7), fdir_3(7), fdir_4(7)
    character*1 :: fsubsn(19), fsubsn_2(19), fsubsn_3(19), fsubsn_4(19)

    write(unit=temp1, fmt='(a40)') avhrrdir
    write(unit=temp2, fmt='(a40)') avhrrdir
    write(unit=temp3, fmt='(a40)') avhrrdir
    write(unit=temp4, fmt='(a40)') avhrrdir
    read(unit=temp1, fmt='(80a1)') (fbase(i), i=1,80)
    read(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,80)
    read(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,80)
    read(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,80)

    write(unit=temp1, fmt='(a1,a4,a2)') '/', cyr1, cmo1
    read(unit=temp1, fmt='(7a1)') fdir
    write(unit=temp2, fmt='(a1,a4,a2)') '/', cyr2, cmo2
    read(unit=temp2, fmt='(7a1)') fdir_2
    write(unit=temp3, fmt='(a5,a2)') '/CLIM', cmo1
    read(unit=temp3, fmt='(7a1)') fdir_3
    write(unit=temp4, fmt='(a5,a2)') '/CLIM', cmo2
    read(unit=temp4, fmt='(7a1)') fdir_4

    do i = 1, 7
       if ( fdir(i) == ' ' ) fdir(i) = '0'
       if ( fdir_2(i) == ' ' ) fdir_2(i) = '0'
       if ( fdir_3(i) == ' ' ) fdir_3(i) = '0'
       if ( fdir_4(i) == ' ' ) fdir_4(i) = '0'
    enddo

    write(unit=temp1, fmt='(a19)') '_avhrrlai_1km.1gd1c'
    write(unit=temp2, fmt='(a19)') '_avhrrlai_1km.1gd1c'
    write(unit=temp3, fmt='(a19)') '_avhrrlai_1km.1gd1c'
    write(unit=temp4, fmt='(a19)') '_avhrrlai_1km.1gd1c'
    read (unit=temp1, fmt='(80a1)') (fsubsn(i), i=1,19)
    read (unit=temp2, fmt='(80a1)') (fsubsn_2(i), i=1,19)
    read (unit=temp3, fmt='(80a1)') (fsubsn_3(i), i=1,19)
    read (unit=temp4, fmt='(80a1)') (fsubsn_4(i), i=1,19)

    c = 0
    do i = 1, 80
       if ( (fbase(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_2(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_3(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_4(i) == ' ') .and. (c == 0) ) c = i-1
    end do

    write(unit=temp1, fmt='(80a1)') (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn(i), i=1,19)
    write(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_2(i), i=1,19)
    write(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_3(i), i=1,19)
    write(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_4(i), i=1,19)
    read(unit=temp1, fmt='(a80)') name9
    read(unit=temp2, fmt='(a80)') name10
    read(unit=temp3, fmt='(a80)') name11
    read(unit=temp4, fmt='(a80)') name12

    write(*, *) "Name9: ", name9
    write(*, *) "Name10: ", name10
    write(*, *) "Name11: ", name11
    write(*, *) "Name12: ", name12

    return

  end subroutine avhrr_laifile_1km

!BOP
! !ROUTINE: avhrr_file_1km
! 
! !DESCRIPTION:
! Generates the 1km filenames for AVHHR LAI data. 
! 
! !INTERFACE:
  subroutine avhrr_saifile_1km (name13,&
       name14,name15,name16, &
       avhrrdir, cyr1, cyr2, cmo1, cmo2 )

    implicit none
! !ARGUMENTS:
    character(len=80), intent(out) :: name13, name14, name15, name16
    character(len=40), intent(in)  :: avhrrdir 
    character(len=4), intent(in)   :: cyr1, cyr2
    character(len=2), intent(in)   :: cmo1, cmo2
!EOP
    character(len=100) :: temp1
    character(len=100) :: temp2
    character(len=100) :: temp3
    character(len=100) :: temp4
    integer :: i,c

    character*1 :: fbase(80), fbase_2(80), fbase_3(80), fbase_4(80)
    character*1 :: fdir(7), fdir_2(7), fdir_3(7), fdir_4(7)
    character*1 :: fsubsn(19), fsubsn_2(19), fsubsn_3(19), fsubsn_4(19)
    character*1 :: fsubsn_5(19), fsubsn_6(19), fsubsn_7(19), fsubsn_8(19)

    write(unit=temp1, fmt='(a40)') avhrrdir
    write(unit=temp2, fmt='(a40)') avhrrdir
    write(unit=temp3, fmt='(a40)') avhrrdir
    write(unit=temp4, fmt='(a40)') avhrrdir
    read(unit=temp1, fmt='(80a1)') (fbase(i), i=1,80)
    read(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,80)
    read(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,80)
    read(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,80)

    write(unit=temp1, fmt='(a1,a4,a2)') '/', cyr1, cmo1
    read(unit=temp1, fmt='(7a1)') fdir
    write(unit=temp2, fmt='(a1,a4,a2)') '/', cyr2, cmo2
    read(unit=temp2, fmt='(7a1)') fdir_2
    write(unit=temp3, fmt='(a5,a2)') '/CLIM', cmo1
    read(unit=temp3, fmt='(7a1)') fdir_3
    write(unit=temp4, fmt='(a5,a2)') '/CLIM', cmo2
    read(unit=temp4, fmt='(7a1)') fdir_4

    do i = 1, 7
       if ( fdir(i) == ' ' ) fdir(i) = '0'
       if ( fdir_2(i) == ' ' ) fdir_2(i) = '0'
       if ( fdir_3(i) == ' ' ) fdir_3(i) = '0'
       if ( fdir_4(i) == ' ' ) fdir_4(i) = '0'
    enddo

    write(unit=temp1, fmt='(a19)') '_avhrrsai_1km.1gd1c'
    write(unit=temp2, fmt='(a19)') '_avhrrsai_1km.1gd1c'
    write(unit=temp3, fmt='(a19)') '_avhrrsai_1km.1gd1c'
    write(unit=temp4, fmt='(a19)') '_avhrrsai_1km.1gd1c'
    read (unit=temp1, fmt='(80a1)') (fsubsn_5(i), i=1,19)
    read (unit=temp2, fmt='(80a1)') (fsubsn_6(i), i=1,19)
    read (unit=temp3, fmt='(80a1)') (fsubsn_7(i), i=1,19)
    read (unit=temp4, fmt='(80a1)') (fsubsn_8(i), i=1,19)

    c = 0
    do i = 1, 80
       if ( (fbase(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_2(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_3(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_4(i) == ' ') .and. (c == 0) ) c = i-1
    end do

    write(unit=temp1, fmt='(80a1)') (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn_5(i), i=1,19)
    write(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_6(i), i=1,19)
    write(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_7(i), i=1,19)
    write(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_8(i), i=1,19)
    read(unit=temp1, fmt='(a80)') name13
    read(unit=temp2, fmt='(a80)') name14
    read(unit=temp3, fmt='(a80)') name15
    read(unit=temp4, fmt='(a80)') name16

    write(*, *) "Name13: ", name13
    write(*, *) "Name14: ", name14
    write(*, *) "Name15: ", name15
    write(*, *) "Name16: ", name16

    return

  end subroutine avhrr_saifile_1km
!BOP
! !ROUTINE: modis_file_1km
! 
! !DESCRIPTION:
! Generates the 1km filenames for MODIS LAI data. 
! 
! !INTERFACE:
  subroutine modis_file_1km (name9,name10,name11,name12,name13,&
       name14,name15,name16, &
       modisdir, cyr1, cyr2, cmo1, cmo2 )

    implicit none
! !ARGUMENTS:
    character(len=80), intent(out) :: name9,  name10, name11, name12
    character(len=80), intent(out) :: name13, name14, name15, name16
    character(len=40), intent(in)  :: modisdir 
    character(len=4), intent(in)   :: cyr1, cyr2
    character(len=2), intent(in)   :: cmo1, cmo2
!EOP
    !==== Local Variables=======================
    character(len=100) :: temp1
    character(len=100) :: temp2
    character(len=100) :: temp3
    character(len=100) :: temp4

    character*1 :: fbase(80), fbase_2(80), fbase_3(80), fbase_4(80)
    character*1 :: fdir(7), fdir_2(7), fdir_3(7), fdir_4(7)
    character*1 :: fsubsn(15), fsubsn_2(15), fsubsn_3(15), fsubsn_4(15)
    character*1 :: fsubsn_5(15), fsubsn_6(15), fsubsn_7(15), fsubsn_8(15)

    integer :: i,c

    write(unit=temp1, fmt='(a40)') modisdir
    write(unit=temp2, fmt='(a40)') modisdir
    write(unit=temp3, fmt='(a40)') modisdir
    write(unit=temp4, fmt='(a40)') modisdir
    read(unit=temp1, fmt='(80a1)') (fbase(i), i=1,80)
    read(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,80)
    read(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,80)
    read(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,80)

    write(unit=temp1, fmt='(a1,a4,a2)') '/', cyr1, cmo1
    read(unit=temp1, fmt='(7a1)') fdir
    write(unit=temp2, fmt='(a1,a4,a2)') '/', cyr2, cmo2
    read(unit=temp2, fmt='(7a1)') fdir_2
    write(unit=temp3, fmt='(a5,a2)') '/CLIM', cmo1
    read(unit=temp3, fmt='(7a1)') fdir_3
    write(unit=temp4, fmt='(a5,a2)') '/CLIM', cmo2
    read(unit=temp4, fmt='(7a1)') fdir_4

    do i = 1, 7
       if ( fdir(i) == ' ' ) fdir(i) = '0'
       if ( fdir_2(i) == ' ' ) fdir_2(i) = '0'
       if ( fdir_3(i) == ' ' ) fdir_3(i) = '0'
       if ( fdir_4(i) == ' ' ) fdir_4(i) = '0'
    enddo

    write(unit=temp1, fmt='(a10)') '_1KM.1gd1c'
    write(unit=temp2, fmt='(a10)') '_1KM.1gd1c'
    write(unit=temp3, fmt='(a10)') '_1KM.1gd1c'
    write(unit=temp4, fmt='(a10)') '_1KM.1gd1c'
    read (unit=temp1, fmt='(80a1)') (fsubsn(i), i=1,15)
    read (unit=temp2, fmt='(80a1)') (fsubsn_2(i), i=1,15)
    read (unit=temp3, fmt='(80a1)') (fsubsn_3(i), i=1,15)
    read (unit=temp4, fmt='(80a1)') (fsubsn_4(i), i=1,15)
    write(unit=temp1, fmt='(a14)') '_SAI_1KM.1gd1c'
    write(unit=temp2, fmt='(a14)') '_SAI_1KM.1gd1c'
    write(unit=temp3, fmt='(a14)') '_SAI_1KM.1gd1c'
    write(unit=temp4, fmt='(a14)') '_SAI_1KM.1gd1c'
    read (unit=temp1, fmt='(80a1)') (fsubsn_5(i), i=1,15)
    read (unit=temp2, fmt='(80a1)') (fsubsn_6(i), i=1,15)
    read (unit=temp3, fmt='(80a1)') (fsubsn_7(i), i=1,15)
    read (unit=temp4, fmt='(80a1)') (fsubsn_8(i), i=1,15)

    c = 0
    do i = 1, 80
       if ( (fbase(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_2(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_3(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_4(i) == ' ') .and. (c == 0) ) c = i-1
    end do

    write(unit=temp1, fmt='(80a1)') (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn(i), i=1,15)
    write(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_2(i), i=1,15)
    write(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_3(i), i=1,15)
    write(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_4(i), i=1,15)
    read(unit=temp1, fmt='(a80)') name9
    read(unit=temp2, fmt='(a80)') name10
    read(unit=temp3, fmt='(a80)') name11
    read(unit=temp4, fmt='(a80)') name12
    write(unit=temp1, fmt='(80a1)') (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn_5(i), i=1,15)
    write(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_6(i), i=1,15)
    write(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_7(i), i=1,15)
    write(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_8(i), i=1,15)
    read(unit=temp1, fmt='(a80)') name13
    read(unit=temp2, fmt='(a80)') name14
    read(unit=temp3, fmt='(a80)') name15
    read(unit=temp4, fmt='(a80)') name16

    write(*, *) "Name9: ", name9
    write(*, *) "Name10: ", name10
    write(*, *) "Name11: ", name11
    write(*, *) "Name15: ", name15

    return

  end subroutine modis_file_1km

  !BOP
  ! !ROUTINE: avhrr_laifilename
  !
  ! !DESCRIPTION: This subroutine puts together AVHRR file name
  ! 
  ! !INTERFACE:
  subroutine avhrr_laifilename ( & 
       name9,name10,name11,name12, & 
       avhrrdir, cyr1, cyr2, cmo1, cmo2 )

    implicit none
  ! !ARGUMENTS: 
    character(len=80), intent(out) :: name9,  name10, name11, name12
    character(len=40), intent(in)  :: avhrrdir 
    character(len=4),  intent(in)  :: cyr1, cyr2
    character(len=2),  intent(in)  :: cmo1, cmo2
  !EOP
    !==== Local Variables=======================
    character(len=100) :: temp1
    character(len=100) :: temp2
    character(len=100) :: temp3
    character(len=100) :: temp4
    integer :: i, c
    character*1 :: fbase(80), fbase_2(80), fbase_3(80), fbase_4(80)
    character*1 :: fdir(7), fdir_2(7), fdir_3(7), fdir_4(7)
    character*1 :: fsubsn(15), fsubsn_2(15), fsubsn_3(15), fsubsn_4(15)

    !=== End Variable Definition ===============
    !=== formats for filename segments
    !BOC
    write(unit=temp1,fmt='(a40)') avhrrdir
    write(unit=temp2,fmt='(a40)') avhrrdir
    write(unit=temp3,fmt='(a40)') avhrrdir
    write(unit=temp4,fmt='(a40)') avhrrdir

    read(unit=temp1, fmt='(80a1)') (fbase(i), i=1,80)
    read(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,80)
    read(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,80)
    read(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,80)

    write(unit=temp1,fmt='(a1,a4,a2)') '/', cyr1, cmo1
    read(unit=temp1, fmt='(7a1)') fdir
    write(unit=temp2,fmt='(a1,a4,a2)') '/', cyr2, cmo2
    read(unit=temp2, fmt='(7a1)') fdir_2
    write(unit=temp3, fmt='(a5,a2)') '/CLIM', cmo1
    read(unit=temp3, fmt='(7a1)') fdir_3
    write(unit=temp4, fmt='(a5,a2)') '/CLIM', cmo2
    read(unit=temp4,fmt='(7a1)') fdir_4

    do i = 1, 7
       if ( fdir(i) == ' ' ) fdir(i) = '0'
       if ( fdir_2(i) == ' ' ) fdir_2(i) = '0'
       if ( fdir_3(i) == ' ' ) fdir_3(i) = '0'
       if ( fdir_4(i) == ' ' ) fdir_4(i) = '0'
    enddo

    write(unit=temp1, fmt='(a15)') '_AVHRRLAI_0.125'
    write(unit=temp2, fmt='(a15)') '_AVHRRLAI_0.125'
    write(unit=temp3, fmt='(a15)') '_AVHRRLAI_0.125'
    write(unit=temp4, fmt='(a15)') '_AVHRRLAI_0.125'
    read (unit=temp1, fmt='(80a1)') (fsubsn(i), i=1,15)
    read (unit=temp2, fmt='(80a1)') (fsubsn_2(i), i=1,15)
    read (unit=temp3, fmt='(80a1)') (fsubsn_3(i), i=1,15)
    read (unit=temp4, fmt='(80a1)') (fsubsn_4(i), i=1,15)

    c = 0
    do i = 1, 80
       if ( (fbase(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_2(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_3(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_4(i) == ' ') .and. (c == 0) ) c = i-1
    end do

    write(unit=temp1, fmt='(80a1)') (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn(i), i=1,15)
    write(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_2(i), i=1,15)
    write(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_3(i), i=1,15)
    write(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_4(i), i=1,15)
    read(unit=temp1, fmt='(a80)') name9
    read(unit=temp2, fmt='(a80)') name10
    read(unit=temp3, fmt='(a80)') name11
    read(unit=temp4, fmt='(a80)') name12

    print*, 'lai1 : rt ',name9
    print*, 'lai2 : rt ',name10
    print*, 'lai1 : clim ',name11
    print*, 'lai2 : clim ',name12


    return
    !EOC       
  end subroutine avhrr_laifilename

  !BOP
  ! !ROUTINE: avhrr_laifilename
  !
  ! !DESCRIPTION: This subroutine puts together AVHRR file name
  ! 
  ! !INTERFACE:
  subroutine avhrr_saifilename ( & 
       name13,name14,name15,name16, &
       avhrrdir, cyr1, cyr2, cmo1, cmo2 )

    implicit none
  ! !ARGUMENTS: 
    character(len=80), intent(out) :: name13, name14, name15, name16
    character(len=40), intent(in)  :: avhrrdir 
    character(len=4),  intent(in)  :: cyr1, cyr2
    character(len=2),  intent(in)  :: cmo1, cmo2
  !EOP
    !==== Local Variables=======================
    character(len=100) :: temp1
    character(len=100) :: temp2
    character(len=100) :: temp3
    character(len=100) :: temp4
    integer :: i, c
    character*1 :: fbase(80), fbase_2(80), fbase_3(80), fbase_4(80)
    character*1 :: fdir(7), fdir_2(7), fdir_3(7), fdir_4(7)
    character*1 :: fsubsn_5(15), fsubsn_6(15), fsubsn_7(15), fsubsn_8(15)

    !=== End Variable Definition ===============
    !=== formats for filename segments
    !BOC
    write(unit=temp1,fmt='(a40)') avhrrdir
    write(unit=temp2,fmt='(a40)') avhrrdir
    write(unit=temp3,fmt='(a40)') avhrrdir
    write(unit=temp4,fmt='(a40)') avhrrdir

    read(unit=temp1, fmt='(80a1)') (fbase(i), i=1,80)
    read(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,80)
    read(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,80)
    read(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,80)

    write(unit=temp1,fmt='(a1,a4,a2)') '/', cyr1, cmo1
    read(unit=temp1, fmt='(7a1)') fdir
    write(unit=temp2,fmt='(a1,a4,a2)') '/', cyr2, cmo2
    read(unit=temp2, fmt='(7a1)') fdir_2
    write(unit=temp3, fmt='(a5,a2)') '/CLIM', cmo1
    read(unit=temp3, fmt='(7a1)') fdir_3
    write(unit=temp4, fmt='(a5,a2)') '/CLIM', cmo2
    read(unit=temp4,fmt='(7a1)') fdir_4

    do i = 1, 7
       if ( fdir(i) == ' ' ) fdir(i) = '0'
       if ( fdir_2(i) == ' ' ) fdir_2(i) = '0'
       if ( fdir_3(i) == ' ' ) fdir_3(i) = '0'
       if ( fdir_4(i) == ' ' ) fdir_4(i) = '0'
    enddo

    write(unit=temp1, fmt='(a15)') '_AVHRRSAI_0.125'
    write(unit=temp2, fmt='(a15)') '_AVHRRSAI_0.125'
    write(unit=temp3, fmt='(a15)') '_AVHRRSAI_0.125'
    write(unit=temp4, fmt='(a15)') '_AVHRRSAI_0.125'
    read (unit=temp1, fmt='(80a1)') (fsubsn_5(i), i=1,15)
    read (unit=temp2, fmt='(80a1)') (fsubsn_6(i), i=1,15)
    read (unit=temp3, fmt='(80a1)') (fsubsn_7(i), i=1,15)
    read (unit=temp4, fmt='(80a1)') (fsubsn_8(i), i=1,15)

    c = 0
    do i = 1, 80
       if ( (fbase(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_2(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_3(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_4(i) == ' ') .and. (c == 0) ) c = i-1
    end do

    write(unit=temp1, fmt='(80a1)') (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn_5(i), i=1,15)
    write(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_6(i), i=1,15)
    write(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_7(i), i=1,15)
    write(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_8(i), i=1,15)
    read(unit=temp1, fmt='(a80)') name13
    read(unit=temp2, fmt='(a80)') name14
    read(unit=temp3, fmt='(a80)') name15
    read(unit=temp4, fmt='(a80)') name16

    print*, 'sai1: real ',name13
    print*, 'sai2: real ',name14
    print*, 'sai1: clim ',name15
    print*, 'sai2: clim ',name16

    return
    !EOC       
  end subroutine avhrr_saifilename

  !BOP
  ! 
  ! !ROUTINE: avhrr_file_5km
  !
  ! !DESCRIPTION: This subroutine puts together AVHRR file name
  ! 
  ! !INTERFACE:
  subroutine avhrr_laifile_5km ( &  
       name9,name10,name11,name12, &
       avhrrdir, cyr1, cyr2, cmo1, cmo2 )
    implicit none

    character(len=40), intent(in) :: avhrrdir 
    character(len=80), intent(out) :: name9,  name10, name11, name12
    character(len=4), intent(in)  :: cyr1, cyr2
    character(len=2), intent(in)  :: cmo1, cmo2
    !EOP  

    integer :: i, c
    character*1 :: fbase(80), fbase_2(80), fbase_3(80), fbase_4(80)
    character*1 :: fdir(7), fdir_2(7), fdir_3(7), fdir_4(7)
    character*1 :: fsubsn(15), fsubsn_2(15), fsubsn_3(15), fsubsn_4(15)
    character(len=100) :: temp1
    character(len=100) :: temp2
    character(len=100) :: temp3
    character(len=100) :: temp4
    !=== End Variable Definition ===============
    !=== formats for filename segments
    !BOC
    write(unit=temp1, fmt='(a40)') avhrrdir
    write(unit=temp2, fmt='(a40)') avhrrdir
    write(unit=temp3, fmt='(a40)') avhrrdir
    write(unit=temp4, fmt='(a40)') avhrrdir
    read(unit=temp1, fmt='(80a1)') (fbase(i), i=1,80)
    read(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,80)
    read(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,80)
    read(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,80)

    write(unit=temp1, fmt='(a1,a4,a2)') '/', cyr1, cmo1
    read(unit=temp1, fmt='(7a1)') fdir
    write(unit=temp2, fmt='(a1,a4,a2)') '/', cyr2, cmo2
    read(unit=temp2, fmt='(7a1)') fdir_2
    write(unit=temp3, fmt='(a5,a2)') '/CLIM', cmo1
    read(unit=temp3, fmt='(7a1)') fdir_3
    write(unit=temp4, fmt='(a5,a2)') '/CLIM', cmo2
    read(unit=temp4, fmt='(7a1)') fdir_4

    do i = 1, 7
       if ( fdir(i) == ' ' ) fdir(i) = '0'
       if ( fdir_2(i) == ' ' ) fdir_2(i) = '0'
       if ( fdir_3(i) == ' ' ) fdir_3(i) = '0'
       if ( fdir_4(i) == ' ' ) fdir_4(i) = '0'
    enddo

    write(unit=temp1, fmt='(a8)') '_5KM.bin'
    write(unit=temp2, fmt='(a8)') '_5KM.bin'
    write(unit=temp3, fmt='(a8)') '_5KM.bin'
    write(unit=temp4, fmt='(a8)') '_5KM.bin'
    read (unit=temp1, fmt='(80a1)') (fsubsn(i), i=1,15)
    read (unit=temp2, fmt='(80a1)') (fsubsn_2(i), i=1,15)
    read (unit=temp3, fmt='(80a1)') (fsubsn_3(i), i=1,15)
    read (unit=temp4, fmt='(80a1)') (fsubsn_4(i), i=1,15)

    c = 0
    do i = 1, 80
       if ( (fbase(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_2(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_3(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_4(i) == ' ') .and. (c == 0) ) c = i-1
    end do

    write(unit=temp1, fmt='(80a1)') (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn(i), i=1,15)
    write(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_2(i), i=1,15)
    write(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_3(i), i=1,15)
    write(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_4(i), i=1,15)
    read(unit=temp1, fmt='(a80)') name9
    read(unit=temp2, fmt='(a80)') name10
    read(unit=temp3, fmt='(a80)') name11
    read(unit=temp4, fmt='(a80)') name12

    return
    !EOC  
  end subroutine avhrr_laifile_5km


  !BOP
  ! 
  ! !ROUTINE: avhrr_file_5km
  !
  ! !DESCRIPTION: This subroutine puts together AVHRR file name
  ! 
  ! !INTERFACE:
  subroutine avhrr_saifile_5km ( &  
       name13,name14,name15,name16, &
       avhrrdir, cyr1, cyr2, cmo1, cmo2 )
    implicit none

    character(len=40), intent(in) :: avhrrdir 
    character(len=80), intent(out) :: name13, name14, name15, name16
    character(len=4), intent(in)  :: cyr1, cyr2
    character(len=2), intent(in)  :: cmo1, cmo2
    !EOP  

    integer :: i, c
    character*1 :: fbase(80), fbase_2(80), fbase_3(80), fbase_4(80)
    character*1 :: fdir(7), fdir_2(7), fdir_3(7), fdir_4(7)
    character*1 :: fsubsn_5(15), fsubsn_6(15), fsubsn_7(15), fsubsn_8(15)
    character(len=100) :: temp1
    character(len=100) :: temp2
    character(len=100) :: temp3
    character(len=100) :: temp4
    !=== End Variable Definition ===============
    !=== formats for filename segments
    !BOC
    write(unit=temp1, fmt='(a40)') avhrrdir
    write(unit=temp2, fmt='(a40)') avhrrdir
    write(unit=temp3, fmt='(a40)') avhrrdir
    write(unit=temp4, fmt='(a40)') avhrrdir
    read(unit=temp1, fmt='(80a1)') (fbase(i), i=1,80)
    read(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,80)
    read(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,80)
    read(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,80)

    write(unit=temp1, fmt='(a1,a4,a2)') '/', cyr1, cmo1
    read(unit=temp1, fmt='(7a1)') fdir
    write(unit=temp2, fmt='(a1,a4,a2)') '/', cyr2, cmo2
    read(unit=temp2, fmt='(7a1)') fdir_2
    write(unit=temp3, fmt='(a5,a2)') '/CLIM', cmo1
    read(unit=temp3, fmt='(7a1)') fdir_3
    write(unit=temp4, fmt='(a5,a2)') '/CLIM', cmo2
    read(unit=temp4, fmt='(7a1)') fdir_4

    do i = 1, 7
       if ( fdir(i) == ' ' ) fdir(i) = '0'
       if ( fdir_2(i) == ' ' ) fdir_2(i) = '0'
       if ( fdir_3(i) == ' ' ) fdir_3(i) = '0'
       if ( fdir_4(i) == ' ' ) fdir_4(i) = '0'
    enddo

    write(unit=temp1, fmt='(a12)') '_SAI_5KM.bin'
    write(unit=temp2, fmt='(a12)') '_SAI_5KM.bin'
    write(unit=temp3, fmt='(a12)') '_SAI_5KM.bin'
    write(unit=temp4, fmt='(a12)') '_SAI_5KM.bin'
    read (unit=temp1, fmt='(80a1)') (fsubsn_5(i), i=1,15)
    read (unit=temp2, fmt='(80a1)') (fsubsn_6(i), i=1,15)
    read (unit=temp3, fmt='(80a1)') (fsubsn_7(i), i=1,15)
    read (unit=temp4, fmt='(80a1)') (fsubsn_8(i), i=1,15)

    c = 0
    do i = 1, 80
       if ( (fbase(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_2(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_3(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_4(i) == ' ') .and. (c == 0) ) c = i-1
    end do
    write(unit=temp1, fmt='(80a1)') (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn_5(i), i=1,15)
    write(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_6(i), i=1,15)
    write(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_7(i), i=1,15)
    write(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_8(i), i=1,15)
    read(unit=temp1, fmt='(a80)') name13
    read(unit=temp2, fmt='(a80)') name14
    read(unit=temp3, fmt='(a80)') name15
    read(unit=temp4, fmt='(a80)') name16

    return
    !EOC  
  end subroutine avhrr_saifile_5km

 
  !BOP
  ! 
  ! !ROUTINE: modis_file_2
  ! 
  ! !DESCRIPTION: This subroutine puts together MODIS file name
  ! 
  ! !INTERFACE:
  subroutine modis_file_2 (name9,name10,name11,name12,& 
       name13,name14,name15,name16, &
       modisdir, cyr1, cyr2, cmo1, cmo2 )
    !EOP
    implicit none
    character(len=40), intent(in) :: modisdir
    character(len=80), intent(out):: name9,  name10, name11, name12
    character(len=80), intent(out):: name13, name14, name15, name16
    character(len=4), intent(in)  :: cyr1, cyr2
    character(len=2), intent(in)  :: cmo1, cmo2
    integer :: i, c
    character*1 :: fbase(80), fbase_2(80), fbase_3(80), fbase_4(80)
    character*1 :: fdir(7), fdir_2(7), fdir_3(7), fdir_4(7)
    character*1 :: fsubsn(15), fsubsn_2(15), fsubsn_3(15), fsubsn_4(15)
    character*1 :: fsubsn_5(15), fsubsn_6(15), fsubsn_7(15), fsubsn_8(15)

    !=== End Variable Definition ===============
    !=== formats for filename segments
    !BOC
92  format (80a1)
93  format (a80)
94  format (i4, i2, i2, i2)
95  format (10a1)
96  format (a40)
97  format (a9)
67  format (a15)
98  format (a1, a4, a2)
66  format (a5,a2)
99  format (7a1)

    write(90, 96, rec=1) modisdir
    write(91, 96, rec=1) modisdir
    write(92, 96, rec=1) modisdir
    write(93, 96, rec=1) modisdir
    read(90, 92, rec=1) (fbase(i), i=1,80)
    read(91, 92, rec=1) (fbase_2(i), i=1,80)
    read(92, 92, rec=1) (fbase_3(i), i=1,80)
    read(93, 92, rec=1) (fbase_4(i), i=1,80)

    write(90, 98, rec=1) '/', cyr1, cmo1
    read(90, 99, rec=1) fdir
    write(91, 98, rec=1) '/', cyr2, cmo2
    read(91, 99, rec=1) fdir_2
    write(92, 66, rec=1) '/CLIM', cmo1
    read(92, 99, rec=1) fdir_3
    write(93, 66, rec=1) '/CLIM', cmo2
    read(93, 99, rec=1) fdir_4

    do i = 1, 7
       if ( fdir(i) == ' ' ) fdir(i) = '0'
       if ( fdir_2(i) == ' ' ) fdir_2(i) = '0'
       if ( fdir_3(i) == ' ' ) fdir_3(i) = '0'
       if ( fdir_4(i) == ' ' ) fdir_4(i) = '0'
    enddo

    write(90, 67, rec=1) '_MODISLAI_0.125'
    write(91, 67, rec=1) '_MODISLAI_0.125'
    write(92, 67, rec=1) '_MODISLAI_0.125'
    write(93, 67, rec=1) '_MODISLAI_0.125'
    read (90, 92, rec=1) (fsubsn(i), i=1,15)
    read (91, 92, rec=1) (fsubsn_2(i), i=1,15)
    read (92, 92, rec=1) (fsubsn_3(i), i=1,15)
    read (93, 92, rec=1) (fsubsn_4(i), i=1,15)
    write(90, 67, rec=1) '_MODISSAI_0.125'
    write(91, 67, rec=1) '_MODISSAI_0.125'
    write(92, 67, rec=1) '_MODISSAI_0.125'
    write(93, 67, rec=1) '_MODISSAI_0.125'
    read (90, 92, rec=1) (fsubsn_5(i), i=1,15)
    read (91, 92, rec=1) (fsubsn_6(i), i=1,15)
    read (92, 92, rec=1) (fsubsn_7(i), i=1,15)
    read (93, 92, rec=1) (fsubsn_8(i), i=1,15)

    c = 0
    do i = 1, 80
       if ( (fbase(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_2(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_3(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_4(i) == ' ') .and. (c == 0) ) c = i-1
    end do

    write(90, 92, rec=1) (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn(i), i=1,15)
    write(91, 92, rec=1) (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_2(i), i=1,15)
    write(92, 92, rec=1) (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_3(i), i=1,15)
    write(93, 92, rec=1) (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_4(i), i=1,15)
    read(90, 93, rec=1) name9
    read(91, 93, rec=1) name10
    read(92, 93, rec=1) name11
    read(93, 93, rec=1) name12

    write(90, 92, rec=1) (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn_5(i), i=1,15)
    write(91, 92, rec=1) (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_6(i), i=1,15)
    write(92, 92, rec=1) (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_7(i), i=1,15)
    write(93, 92, rec=1) (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_8(i), i=1,15)
    read(90, 93, rec=1) name13
    read(91, 93, rec=1) name14
    read(92, 93, rec=1) name15
    read(93, 93, rec=1) name16

    close(90)
    close(91)
    close(92)
    close(93)

    !  print*, 'n9 ',name9
    !  print*, 'n10 ',name10
    !  print*, 'n11 ',name11
    !  print*, 'n12 ',name12
    !  print*, 'n13 ',name13
    !  print*, 'n14 ',name14
    !  print*, 'n15 ',name15
    !  print*, 'n16 ',name16
    return
    !EOC  
  end subroutine modis_file_2
  !BOP
  ! 
  ! !ROUTINE: modis_file_5km
  ! 
  ! !DESCRIPTION: This subroutine puts together MODIS file name
  ! 
  ! !INTERFACE:

  subroutine modis_file_5km(name9,name10,name11,name12,name13,& 
       name14,name15,name16, &
       modisdir, cyr1, cyr2, cmo1, cmo2 )
    !EOP  
    implicit none

    character(len=40), intent(in)  :: modisdir 
    character(len=80), intent(out) :: name9,  name10, name11, name12
    character(len=80), intent(out) :: name13, name14, name15, name16
    character(len=4), intent(in) :: cyr1, cyr2
    character(len=2), intent(in)  :: cmo1, cmo2
    integer :: i, c
    character*1 :: fbase(80), fbase_2(80), fbase_3(80), fbase_4(80)
    character*1 :: fdir(7), fdir_2(7), fdir_3(7), fdir_4(7)
    character*1 :: fsubsn(15), fsubsn_2(15), fsubsn_3(15), fsubsn_4(15)
    character*1 :: fsubsn_5(15), fsubsn_6(15), fsubsn_7(15), fsubsn_8(15)
    character(len=100) :: temp1
    character(len=100) :: temp2
    character(len=100) :: temp3
    character(len=100) :: temp4
    !=== End Variable Definition ===============
    !=== formats for filename segments
    !BOC
92  format (80a1)
93  format (a80)
94  format (i4, i2, i2, i2)
95  format (10a1)
96  format (a40)
97  format (a9)
67  format (a8)
68  format (a12)
98  format (a1, a4, a2)
66  format (a5,a2)
99  format (7a1)

    write(unit=temp1, fmt='(a40)') modisdir
    write(unit=temp2, fmt='(a40)') modisdir
    write(unit=temp3, fmt='(a40)') modisdir
    write(unit=temp4, fmt='(a40)') modisdir
    read(unit=temp1, fmt='(80a1)') (fbase(i), i=1,80)
    read(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,80)
    read(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,80)
    read(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,80)

    write(unit=temp1, fmt='(a1,a4,a2)') '/', cyr1, cmo1
    read(unit=temp1, fmt='(7a1)') fdir
    write(unit=temp2, fmt='(a1,a4,a2)') '/', cyr2, cmo2
    read(unit=temp2, fmt='(7a1)') fdir_2
    write(unit=temp3, fmt='(a5,a2)') '/CLIM', cmo1
    read(unit=temp3, fmt='(7a1)') fdir_3
    write(unit=temp4, fmt='(a5,a2)') '/CLIM', cmo2
    read(unit=temp4, fmt='(7a1)') fdir_4

    do i = 1, 7
       if ( fdir(i) == ' ' ) fdir(i) = '0'
       if ( fdir_2(i) == ' ' ) fdir_2(i) = '0'
       if ( fdir_3(i) == ' ' ) fdir_3(i) = '0'
       if ( fdir_4(i) == ' ' ) fdir_4(i) = '0'
    enddo

    write(unit=temp1, fmt='(a8)') '_5KM.bin'
    write(unit=temp2, fmt='(a8)') '_5KM.bin'
    write(unit=temp3, fmt='(a8)') '_5KM.bin'
    write(unit=temp4, fmt='(a8)') '_5KM.bin'
    read (unit=temp1, fmt='(80a1)') (fsubsn(i), i=1,15)
    read (unit=temp2, fmt='(80a1)') (fsubsn_2(i), i=1,15)
    read (unit=temp3, fmt='(80a1)') (fsubsn_3(i), i=1,15)
    read (unit=temp4, fmt='(80a1)') (fsubsn_4(i), i=1,15)
    write(unit=temp1, fmt='(a12)') '_SAI_5KM.bin'
    write(unit=temp2, fmt='(a12)') '_SAI_5KM.bin'
    write(unit=temp3, fmt='(a12)') '_SAI_5KM.bin'
    write(unit=temp4, fmt='(a12)') '_SAI_5KM.bin'
    read (unit=temp1, fmt='(80a1)') (fsubsn_5(i), i=1,15)
    read (unit=temp2, fmt='(80a1)') (fsubsn_6(i), i=1,15)
    read (unit=temp3, fmt='(80a1)') (fsubsn_7(i), i=1,15)
    read (unit=temp4, fmt='(80a1)') (fsubsn_8(i), i=1,15)

    c = 0
    do i = 1, 80
       if ( (fbase(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_2(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_3(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_4(i) == ' ') .and. (c == 0) ) c = i-1
    end do

    write(unit=temp1, fmt='(80a1)') (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn(i), i=1,15)
    write(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_2(i), i=1,15)
    write(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_3(i), i=1,15)
    write(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_4(i), i=1,15)
    read(unit=temp1, fmt='(a80)') name9
    read(unit=temp2, fmt='(a80)') name10
    read(unit=temp3, fmt='(a80)') name11
    read(unit=temp4, fmt='(a80)') name12
    write(unit=temp1, fmt='(80a1)') (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn_5(i), i=1,15)
    write(unit=temp2, fmt='(80a1)') (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_6(i), i=1,15)
    write(unit=temp3, fmt='(80a1)') (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_7(i), i=1,15)
    write(unit=temp4, fmt='(80a1)') (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_8(i), i=1,15)
    read(unit=temp1, fmt='(a80)') name13
    read(unit=temp2, fmt='(a80)') name14
    read(unit=temp3, fmt='(a80)') name15
    read(unit=temp4, fmt='(a80)') name16
    print*, 'n11',name11
    print*, 'n15',name15
    return
  end subroutine modis_file_5KM


  !BOP
  ! !ROUTINE: avhrr_g_file
  !
  !  This subroutine puts together AVHRR LAI file name
  !
  ! !INTERFACE:
  subroutine avhrr_g_file (name9,name10,name11,name12,name13,name14, &
       name15,name16,avhrrdir,cyr1,cyr2,cmo1,cmo2 )

    implicit none
    ! !ARGUMENTS:
    character(len=40), intent(in)  :: avhrrdir
    character(len=80), intent(out) :: name9,  name10, name11, name12
    character(len=80), intent(out) :: name13, name14, name15, name16
    character(len=4), intent(in)   :: cyr1, cyr2
    character(len=2), intent(in)   :: cmo1, cmo2
    !EOP
    integer :: i, c
    character*1 :: fbase(80), fbase_2(80), fbase_3(80), fbase_4(80)
    character*1 :: fdir(7), fdir_2(7), fdir_3(7), fdir_4(7)
    character*1 :: fsubsn(15), fsubsn_2(15), fsubsn_3(15), fsubsn_4(15)
    character*1 :: fsubsn_5(15),fsubsn_6(15),fsubsn_7(15),fsubsn_8(15)

    !=== End Variable Definition ===============
    !=== formats for filename segments
92  format (80a1)
93  format (a80)
94  format (i4, i2, i2, i2)
95  format (10a1)
96  format (a40)
97  format (a9)
58  format (a9)
67  format (a15)
68  format (a15)
98  format (a1, a4, a2)
66  format (a5,a2)
99  format (7a1)		

    open(unit=90, file='temp', form='formatted', access='direct', recl=80)
    open(unit=91, file='temp_2', form='formatted', access='direct', recl=80)
    open(unit=92, file='temp_3', form='formatted', access='direct', recl=80)
    open(unit=93, file='temp_4', form='formatted', access='direct', recl=80)
    write(90, 96, rec=1) avhrrdir
    write(91, 96, rec=1) avhrrdir
    write(92, 96, rec=1) avhrrdir
    write(93, 96, rec=1) avhrrdir
    read(90, 92, rec=1) (fbase(i), i=1,80)
    read(91, 92, rec=1) (fbase_2(i), i=1,80)
    read(92, 92, rec=1) (fbase_3(i), i=1,80)
    read(93, 92, rec=1) (fbase_4(i), i=1,80)

    write(90, 98, rec=1) '/', cyr1, cmo1
    read(90, 99, rec=1) fdir
    write(91, 98, rec=1) '/', cyr2, cmo2
    read(91, 99, rec=1) fdir_2
    write(92, 66, rec=1) '/CLIM', cmo1
    read(92, 99, rec=1) fdir_3
    write(93, 66, rec=1) '/CLIM', cmo2
    read(93, 99, rec=1) fdir_4

    do i = 1, 7
       if ( fdir(i) == ' ' ) fdir(i) = '0'
       if ( fdir_2(i) == ' ' ) fdir_2(i) = '0'
       if ( fdir_3(i) == ' ' ) fdir_3(i) = '0'
       if ( fdir_4(i) == ' ' ) fdir_4(i) = '0'
    enddo

    write(90, 67, rec=1) '_AVHRRLAI_0.125'
    write(91, 67, rec=1) '_AVHRRLAI_0.125'
    write(92, 67, rec=1) '_AVHRRLAI_0.125'
    write(93, 67, rec=1) '_AVHRRLAI_0.125'
    read (90, 92, rec=1) (fsubsn(i), i=1,15)
    read (91, 92, rec=1) (fsubsn_2(i), i=1,15)
    read (92, 92, rec=1) (fsubsn_3(i), i=1,15)
    read (93, 92, rec=1) (fsubsn_4(i), i=1,15)
    write(90, 68, rec=1) '_AVHRRSAI_0.125'
    write(91, 68, rec=1) '_AVHRRSAI_0.125'
    write(92, 68, rec=1) '_AVHRRSAI_0.125'
    write(93, 68, rec=1) '_AVHRRSAI_0.125'
    read (90, 92, rec=1) (fsubsn_5(i), i=1,15)
    read (91, 92, rec=1) (fsubsn_6(i), i=1,15)
    read (92, 92, rec=1) (fsubsn_7(i), i=1,15)
    read (93, 92, rec=1) (fsubsn_8(i), i=1,15)

    !sets c as the last character position of fbase
    c = 0
    do i = 1, 80
       if ( (fbase(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_2(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_3(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_4(i) == ' ') .and. (c == 0) ) c = i-1
    end do

    write(90, 92, rec=1) (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn(i), i=1,15)
    write(91, 92, rec=1) (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_2(i), i=1,15)
    write(92, 92, rec=1) (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_3(i), i=1,15)
    write(93, 92, rec=1) (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_4(i), i=1,15)
    read(90, 93, rec=1) name9
    read(91, 93, rec=1) name10
    read(92, 93, rec=1) name11
    read(93, 93, rec=1) name12
    write(90, 92, rec=1) (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn_5(i), i=1,15)
    write(91, 92, rec=1) (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_6(i), i=1,15)
    write(92, 92, rec=1) (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_7(i), i=1,15)
    write(93, 92, rec=1) (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_8(i), i=1,15)
    read(90, 93, rec=1) name13
    read(91, 93, rec=1) name14
    read(92, 93, rec=1) name15
    read(93, 93, rec=1) name16

    close(90)
    close(91)
    close(92)
    close(93)

    return

  end subroutine avhrr_g_file

  !BOP
  ! !ROUTINE: modis_g_file
  !
  ! !DESCRIPTION: 
  ! This subroutine puts together MODIS LAI file name
  ! 
  ! !INTERFACE:
  subroutine modis_g_file (name9,name10,name11,name12,name13,name14, &
       name15,name16,modisdir,cyr1,cyr2,cmo1,cmo2 )
    
    implicit none
    ! !ARGUMENTS:
    character(len=40), intent(in)  :: modisdir
    character(len=80), intent(out) :: name9,  name10, name11, name12
    character(len=80), intent(out) :: name13, name14, name15, name16
    character(len=4), intent(in)   :: cyr1, cyr2
    character(len=2), intent(in)   :: cmo1, cmo2
    !EOP
    integer :: i, c
    character*1 :: fbase(80), fbase_2(80), fbase_3(80), fbase_4(80)
    character*1 :: fdir(7), fdir_2(7), fdir_3(7), fdir_4(7)
    character*1 :: fsubsn(15), fsubsn_2(15), fsubsn_3(15), fsubsn_4(15)
    character*1 :: fsubsn_5(15), fsubsn_6(15), fsubsn_7(15), fsubsn_8(15)

    !=== End Variable Definition ===============
    !=== formats for filename segments
92  format (80a1)
93  format (a80)
94  format (i4, i2, i2, i2)
95  format (10a1)
96  format (a40)
97  format (a9)
58  format (a9)
67  format (a15)
68  format (a15)
98  format (a1, a4, a2)
66  format (a5,a2)
99  format (7a1)

    open(unit=90, file='temp', form='formatted', access='direct', recl=80)
    open(unit=91, file='temp_2', form='formatted', access='direct', recl=80)
    open(unit=92, file='temp_3', form='formatted', access='direct', recl=80)
    open(unit=93, file='temp_4', form='formatted', access='direct', recl=80)
    write(90, 96, rec=1) modisdir
    write(91, 96, rec=1) modisdir
    write(92, 96, rec=1) modisdir
    write(93, 96, rec=1) modisdir
    read(90, 92, rec=1) (fbase(i), i=1,80)
    read(91, 92, rec=1) (fbase_2(i), i=1,80)
    read(92, 92, rec=1) (fbase_3(i), i=1,80)
    read(93, 92, rec=1) (fbase_4(i), i=1,80)

    write(90, 98, rec=1) '/', cyr1, cmo1
    read(90, 99, rec=1) fdir
    write(91, 98, rec=1) '/', cyr2, cmo2
    read(91, 99, rec=1) fdir_2
    write(92, 66, rec=1) '/CLIM', cmo1
    read(92, 99, rec=1) fdir_3
    write(93, 66, rec=1) '/CLIM', cmo2
    read(93, 99, rec=1) fdir_4

    do i = 1, 7
       if ( fdir(i) == ' ' ) fdir(i) = '0'
       if ( fdir_2(i) == ' ' ) fdir_2(i) = '0'
       if ( fdir_3(i) == ' ' ) fdir_3(i) = '0'
       if ( fdir_4(i) == ' ' ) fdir_4(i) = '0'
    enddo

    write(90, 67, rec=1) '_MODISLAI_0.125'
    write(91, 67, rec=1) '_MODISLAI_0.125'
    write(92, 67, rec=1) '_MODISLAI_0.125'
    write(93, 67, rec=1) '_MODISLAI_0.125'
    read (90, 92, rec=1) (fsubsn(i), i=1,15)
    read (91, 92, rec=1) (fsubsn_2(i), i=1,15)
    read (92, 92, rec=1) (fsubsn_3(i), i=1,15)
    read (93, 92, rec=1) (fsubsn_4(i), i=1,15)
    write(90, 68, rec=1) '_MODISSAI_0.125'
    write(91, 68, rec=1) '_MODISSAI_0.125'
    write(92, 68, rec=1) '_MODISSAI_0.125'
    write(93, 68, rec=1) '_MODISSAI_0.125'
    read (90, 92, rec=1) (fsubsn_5(i), i=1,15)
    read (91, 92, rec=1) (fsubsn_6(i), i=1,15)
    read (92, 92, rec=1) (fsubsn_7(i), i=1,15)
    read (93, 92, rec=1) (fsubsn_8(i), i=1,15)

    !sets c as the last character position of fbase
    c = 0
    do i = 1, 80
       if ( (fbase(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_2(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_3(i) == ' ') .and. (c == 0) ) c = i-1
       if ( (fbase_4(i) == ' ') .and. (c == 0) ) c = i-1
    end do

    write(90, 92, rec=1) (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn(i), i=1,15)
    write(91, 92, rec=1) (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_2(i), i=1,15)
    write(92, 92, rec=1) (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_3(i), i=1,15)
    write(93, 92, rec=1) (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_4(i), i=1,15)
    read(90, 93, rec=1) name9
    read(91, 93, rec=1) name10
    read(92, 93, rec=1) name11
    read(93, 93, rec=1) name12
    write(90, 92, rec=1) (fbase(i), i=1,c), (fdir(i), i=1,7),  &
         (fsubsn_5(i), i=1,15)
    write(91, 92, rec=1) (fbase_2(i), i=1,c), (fdir_2(i), i=1,7),  &
         (fsubsn_6(i), i=1,15)
    write(92, 92, rec=1) (fbase_3(i), i=1,c), (fdir_3(i), i=1,7),  &
         (fsubsn_7(i), i=1,15)
    write(93, 92, rec=1) (fbase_4(i), i=1,c), (fdir_4(i), i=1,7),  &
         (fsubsn_8(i), i=1,15)
    read(90, 93, rec=1) name13
    read(91, 93, rec=1) name14
    read(92, 93, rec=1) name15
    read(93, 93, rec=1) name16

    close(90)
    close(91)
    close(92)
    close(93)

    return

  end subroutine modis_g_file


end module filename_mod
