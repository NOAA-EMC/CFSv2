	subroutine getPentad( mxP, myP, pentadFile, p250_5dy )
C=====================================================================
C	GET 1 CPC PENTAD CMAP DATA 
C
C	CALL BY: cmap2gldas.f
C	
C	METHOD:
C	1. LOAD PENTAD DATA (LON=0,360; LAT=-90,90)
C	2. EXTRACT PARTIAL PENTAD ARRAY (LON=0,360; LAT=-60,60)
C
C	20010822 JESSE MENG
C=====================================================================

      implicit none

      integer				:: i
      integer				:: j
      integer				:: k
      integer,		intent(in)	:: mxP
      integer,		intent(in)	:: myP

      logical				:: exists

      character*100,	intent(in)	:: pentadFile
      real,		intent(out)	:: p250_5dy(mxP, myP)

      real				:: dummy(mxP, 72)

C     ----------------------------------------------------------------

      open(11, file=pentadFile, status='old')
      
      do j = 1, myP
      do i = 1, mxP
         read( 11, * ) dummy(i, j)
      end do
      end do

      do j = 1, myP
      do i = 1, mxP
         p250_5dy(i,j) = dummy(i,j)
      end do
      end do

      close(11)

      return
      end
