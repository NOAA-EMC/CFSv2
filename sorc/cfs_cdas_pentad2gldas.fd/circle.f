      subroutine circle( mx, my, A, i1, j1, i2, j2, nn )
c=====================================================================
c
c	circle around point (i1,j1) whthin 2D matrix A;
c	search for the nearest point where A(i2,j2) != 0;
c	return i2, j2
c
c             (i-1, j )->( i ,j+1)->(i+1, j )
c                 ^                     ^       
c                 |                     |
c             (i-1, j )<-( i , j )->(i+1, j )
c                 |                     |
c                 V                     V
c             (i-1,j-1)->( i ,j-1)->(i+1, j )
c
c=====================================================================

      implicit none

      integer,	intent(in)	:: mx
      integer,	intent(in)	:: my
      
      integer,	intent(in)	:: i1
      integer,	intent(out)	:: i2
      integer,	intent(in)	:: j1
      integer,	intent(out)	:: j2

      real,	intent(in)	:: A  (mx, my)

      real A1(mx, my)

      integer wd		!distance from (i2,j2) to (i1,j1)
      integer R  (mx, my)
      integer R1 (mx, my)

      integer n			!max radius search
      integer nn		!radius count
      integer i, j
      integer ii, jj
      integer ix, jy

      logical Lfound

c=====================================================================

      R  = 0
      R1 = 0

      Lfound = .FALSE.

      i = i1
      j = j1
      n = 8
      wd = n*n*2

!      write(*,'(A,2I2,A)') "START POINT = (", i, j, " )"
!      write(*,*) "MAX RADIUS  = ", n

      nn = 1
      do while (nn .LE. n)

!         print*, 'RADIUS = ', nn


c---------------------------------------------------------------------
c	LOWER LINE
c---------------------------------------------------------------------

         jj = j - nn
         jy = jj
         if( jy .LT. 1 ) jy = 1

         do ii = i-nn, i+nn
            ix = ii
            if( ix .LT. 1  ) ix = ix + mx
            if( ix .GT. mx ) ix = ix - mx
            A1(ix,jy) = 1
            R(ix,jy) = (ii-i)*(ii-i) + (jy-j)*(jy-j)

              if ( A(ix,jy).GT.0 .AND. R(ix,jy).LT.wd ) then
                 Lfound = .TRUE.
                 i2 = ix
                 j2 = jy
                 wd = R(ix,jy)
              end if
         end do

c---------------------------------------------------------------------
c	UPPER LINE
c---------------------------------------------------------------------

         jj = j + nn
         jy = jj
         if( jy .GE. my ) jy = my

         do ii = i-nn, i+nn
            ix = ii
            if( ix .LT. 1  ) ix = ix + mx
            if( ix .GT. mx ) ix = ix - mx
            A1(ix,jy) = 1
            R(ix,jy) = (ii-i)*(ii-i) + (jy-j)*(jy-j)

              if ( A(ix,jy).GT.0 .AND. R(ix,jy).LT.wd ) then
                 Lfound = .TRUE.
                 i2 = ix
                 j2 = jy
                 wd = R(ix,jy)
              end if
         end do

c---------------------------------------------------------------------
c	LEFT AND RIGHT LINES
c---------------------------------------------------------------------

         do jj = j-nn, j+nn

            jy = jj

            if( jy.GE.1 .AND. jy.LE.my ) then

              ii = i - nn
              ix = ii
              if( ix .LT. 1   ) ix = ix + mx
              A1(ix,jy) = 1
              R(ix,jy) = (ii-i)*(ii-i) + (jy-j)*(jy-j)

              if ( A(ix,jy).GT.0 .AND. R(ix,jy).LT.wd ) then
                 Lfound = .TRUE.
                 i2 = ix
                 j2 = jy
                 wd = R(ix,jy)
              end if

              ii = i + nn
              ix = ii
              if( ix .GT. mx ) ix = ix - mx
              A1(ix,jy) = 1
              R(ix,jy) = (ii-i)*(ii-i) + (jy-j)*(jy-j)

              if ( A(ix,jy).GT.0 .AND. R(ix,jy).LT.wd ) then
                 Lfound = .TRUE.
                 i2 = ix
                 j2 = jy
                 wd = R(ix,jy)
              end if

            end if
         end do
      
c---------------------------------------------------------------------
c	IF(FOUND)->RETURN;
C	ELSE->SEARCH OUTER CIRCLE 
c---------------------------------------------------------------------

         if ( Lfound ) return
               
         nn = nn + 1
      end do
       
      return
      end
      
