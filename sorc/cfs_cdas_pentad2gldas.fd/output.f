	subroutine output( outpath, ioUnit, mx, my, mk, data )
C=====================================================================
C	OUTPUT UTILITY FOR pentad2gldas PACKAGE
C
C	20010807 JESSE MENG
C=====================================================================

      implicit none

      character*100, intent(in)		:: outpath
      integer, 	intent(inout)		:: ioUnit
      integer,	intent(in)		:: mx
      integer,	intent(in)		:: my
      integer,	intent(in)		:: mk
      real,	intent(in)		:: data(mx, my, mk)

      integer i, j, k
      character*4 cUnit
      character*100 outfile

C     ----------------------------------------------------------------

      write( cUnit, '(I4.4)' ) ioUnit
      print*, 'output(): ioUnit = ', cUnit

      outfile = trim(outpath)//'fort.'//cUnit
      print*, outfile

      open ( ioUnit, file=outfile, 
     &       form='unformatted', access='direct', recl=mx*my*4,
     &       status='unknown' )

      do k = 1, mk
         write( ioUnit, rec=k ) ((data(i,j,k), i=1,mx), j=1,my)
      end do

      close ( ioUnit )

!      print*, '- END output()'

      return
      end
