 module native_endianness
!$$$ module documentation block
!
! module:  native endianness
!   prgmmr: jovic          org: w/np2           date: 2012
!
! $Revision$
!
! abstract: contains routines to convert variables from
!   big endian to native endianness
!
! program history log:
! 2012        jovic    - initial version
!
! usage: use native_endianness
!
! remarks: none
!
!$$$

 implicit none

 private

 public to_native_endianness
 logical, parameter, public :: is_little_endian = ichar(transfer(1,'a')) == 1

 interface to_native_endianness
   module procedure to_native_endianness_i2
   module procedure to_native_endianness_i4
   module procedure to_native_endianness_r4
   module procedure to_native_endianness_r8
 end interface to_native_endianness

 contains

 subroutine to_native_endianness_i2(i2)
!$$$ subroutine documentation block
!
! subprogram:  to_native_endianness_i2
!   prgmmr: jovic          org: w/np2           date: 2012
!
! abstract: convert 2-byte integer scalar from big-endian to 
!   native-endian
!
! program history log:
! 2012        jovic     - initial version
!
! usage:  call routine one argument
!   input:
!     i2  - 2-byte integer in big endian
!
!   output:
!     i2  - 2-byte integer in native endianness
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$

 implicit none

 integer(kind=2), intent(inout) :: i2

 integer(kind=1), dimension(2) :: byte_arr, byte_arr_tmp
 integer :: i

 if (.not.is_little_endian) return

 byte_arr_tmp = transfer (i2, byte_arr_tmp)

 do i = 1, 2
   byte_arr(i) = byte_arr_tmp(3-i)
 end do

 i2 = transfer (byte_arr, i2)

 return

 end subroutine to_native_endianness_i2

 subroutine to_native_endianness_i4(i4)
!$$$ subroutine documentation block
!
! subprogram:  to_native_endianness_i4
!   prgmmr: jovic          org: w/np2           date: 2012
!
! abstract: convert 4-byte integer scalar from big-endian to 
!   native-endian
!
! program history log:
! 2012        jovic     - initial version
!
! usage:  call routine one argument
!   input:
!     i4  - 4-byte integer in big endian
!
!   output:
!     i4  - 4-byte integer in native endianness
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$

 implicit none

 integer(kind=4), intent(inout) :: i4

 integer(kind=1), dimension(4) :: byte_arr, byte_arr_tmp
 integer :: i

 if (.not.is_little_endian) return

 byte_arr_tmp = transfer (i4, byte_arr)

 do i = 1, 4
   byte_arr(i) = byte_arr_tmp(5-i)
 end do

 i4 = transfer (byte_arr, i4)

 return

 end subroutine to_native_endianness_i4

 subroutine to_native_endianness_r4(r4)
!$$$ subroutine documentation block
!
! subprogram:  to_native_endianness_r4
!   prgmmr: jovic          org: w/np2           date: 2012
!
! abstract: convert 4-byte real scalar from big-endian to 
!   native-endian
!
! program history log:
! 2012        jovic     - initial version
!
! usage:  call routine one argument
!   input:
!     r4  - 4-byte real in big endian
!
!   output:
!     r4  - 4-byte real in native endianness
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$

 implicit none

 real(kind=4), intent(inout) :: r4

 integer(kind=1), dimension(4) :: byte_arr, byte_arr_tmp
 integer :: i

 if (.not.is_little_endian) return

 byte_arr_tmp = transfer (r4, byte_arr)

 do i = 1, 4
   byte_arr(i) = byte_arr_tmp(5-i)
 end do

 r4 = transfer (byte_arr, r4)

 return

 end subroutine to_native_endianness_r4

 subroutine to_native_endianness_r8(r8)
!$$$ subroutine documentation block
!
! subprogram:  to_native_endianness_r8
!   prgmmr: jovic          org: w/np2           date: 2012
!
! abstract: convert 8-byte real scalar from big-endian to 
!   native-endian
!
! program history log:
! 2012        jovic     - initial version
!
! usage:  call routine one argument
!   input:
!     r8  - 8-byte real in big endian
!
!   output:
!     r8  - 8-byte real in native endianness
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$

 implicit none

 real(kind=8), intent(inout) :: r8

 integer(kind=1), dimension(8) :: byte_arr, byte_arr_tmp
 integer :: i

 if (.not.is_little_endian) return

 byte_arr_tmp = transfer (r8, byte_arr)

 do i = 1, 8
   byte_arr(i) = byte_arr_tmp(9-i)
 end do

 r8 = transfer (byte_arr, r8)

 return

 end subroutine to_native_endianness_r8

 end module native_endianness
