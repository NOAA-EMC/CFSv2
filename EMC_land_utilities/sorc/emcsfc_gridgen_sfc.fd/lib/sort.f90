 subroutine sort (x, iy, n)
!$$$ subroutine documentation block
!
! subprogram:  sort
!   prgmmr: gayno          org: w/np2           date: ????
!
! $Revision$
!
! abstract: sort an array.
!
! program history log:
! ????        gayno     - initial version
!
! usage:  call routine with the following arguments:
!
!   input:
!     n      - number of values to sort
!     x      - array of values to sort
!     iy     - array index of elements of x
!
!   output:
!     x      - sorted array
!     iy     - contains original array index position in
!              the unsorted x array.
!
! files: none
!
! condition codes: none
!
! remarks: none.
!
!$$$
 IMPLICIT NONE

 INTEGER                 :: I 
 INTEGER                 :: ISWAP(1)
 INTEGER                 :: ISWAP1
 INTEGER                 :: ITEMP
 INTEGER                 :: IY(N)
 integer                 :: N

 REAL*4                  :: TEMP
 REAL*4                  :: X(1:N)

 DO I=1,N-1
   ISWAP=MAXLOC(X(I:N))
   ISWAP1=ISWAP(1)+I-1
   IF(ISWAP1.NE.I) THEN
     TEMP=X(I)
     X(I)=X(ISWAP1)
     X(ISWAP1)=TEMP
     ITEMP=IY(I)
     IY(I)=IY(ISWAP1)
     IY(ISWAP1)=ITEMP
   ENDIF
 ENDDO 

 return

 end subroutine sort
