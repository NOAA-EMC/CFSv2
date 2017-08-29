	subroutine outgrib( outpath, ioUnit, mx, my, data, kpds, kgds )
C=====================================================================
C	OUTPUT UTILITY FOR pentad2gldas PACKAGE
C
C	20010807 JESSE MENG
C=====================================================================

      implicit none

      character*100, intent(in)		:: outpath
      integer, 	intent(in)		:: ioUnit
      integer,	intent(in)		:: mx
      integer,	intent(in)		:: my
      real,	intent(in)		:: data(mx, my)
      integer,	intent(in)		:: kpds(200)
      integer,  intent(in)		:: kgds(200)

      integer i, j, k
      character*4 cUnit
      character*100 outfile

C--- PUTGB

      logical*1 	                :: bitmap(mx, my)
      integer                   	:: iret

C     ----------------------------------------------------------------

      write( cUnit, '(I4.4)' ) ioUnit
      print*, 'output(): ioUnit = ', cUnit
      outfile = trim(outpath)//'grib.'//cUnit
      print*, outfile

      bitmap = .TRUE.
    !  where ( data .LT. 0. )
    !      bitmap = .FALSE.
    !  end where

      iret = 0
      call baopenw(ioUnit, outfile, iret)
      print*, "AFTER baopenw(", ioUnit, "), iret =", iret
      
      iret = 0
      call putgb(ioUnit, mx*my, kpds, kgds, bitmap, data, iret)
      print*, "AFTER   putgb(", ioUnit, "), iret =", iret

      iret = 0
      call baclose ( ioUnit, iret )
      print*, "AFTER baclose(", ioUnit, "), iret =", iret

      return
      end
