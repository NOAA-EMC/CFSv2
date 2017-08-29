	subroutine input( mday, year, iyear, imonth, iday, ihour,
     &                    gdasPath, pentadFile, outPath, options )
C=====================================================================
C	GET INPUT INFO FOR pentad2gldas PACKAGE
C
C	CALL BY: pentad2gldas.f
C	
C	20021213 JESSE MENG
C=====================================================================

      implicit none

      integer,	parameter	:: maxt = 24	!max possible 6 hourly
						!time intervals in 1 pentad
					   	!24 in leap year pantad 12
      integer				:: mday
      integer,          intent(out)     :: year
      integer,		intent(out)	:: iyear(maxt)
      integer, 		intent(out)	:: imonth(maxt)
      integer,		intent(out)	:: iday(maxt)
      integer,		intent(out)	:: ihour(maxt)
      character*100,	intent(out)	:: gdasPath
      character*100, 	intent(out)	:: pentadFile
      character*100,	intent(out)	:: outPath
      integer,		intent(out)     :: options(10)

      integer, parameter		:: ioption = 2
      integer 				:: k

C     ----------------------------------------------------------------

      open(1, file='pentad2gldas.in', status='old')
    
      read(1, *) mday
      read(1, '(I4)') year
      do k = 1, mday*4
         read(1, '(I4,1X,I2,1X,I2,1X,I2)') 
     &   iyear(k), imonth(k), iday(k), ihour(k)
      end do
      read(1, *)
      read(1, '(A)') gdasPath
      read(1, *)
      read(1, '(A)') pentadFile
      read(1, *)
      read(1, '(A)') outPath
      read(1, *)
      do k = 1, ioption
         read(1, *) options(k)
      end do

      close(1)

      return
      end
