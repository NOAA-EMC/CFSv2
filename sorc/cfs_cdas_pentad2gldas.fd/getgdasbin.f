	subroutine getGDAS( mxR, myR, gdasFileHK, r070_6hr )
C=====================================================================
C	GET 5 DAYS GDAS PRATE
C
C	CALL BY: pentad2gldas.f
C	
C	20021213 JESSE MENG
C=====================================================================

      implicit none

      integer				:: i
      integer				:: j
      integer				:: k
      integer				:: h

      integer,		intent(in)	:: mxR
      integer,		intent(in)	:: myR
      character*100,	intent(in)	:: gdasFileHK
      real,		intent(out)	:: r070_6hr(mxR, myR)

      logical				:: exists
      real                              :: undef = -9999.

C     ----------------------------------------------------------------

      print*, trim(gdasFileHK)
      
      r070_6hr = 0.

      inquire (file = gdasFileHK, exist = exists)
      if ( .not. exists ) then
         print*, 'X-X-X GDAS FILE DOES NOT EXIST. CHECK PATH.'
         print*, 'X-X-X ENTIRE FIELD UNDEF.'
         r070_6hr = undef
         return
      endif

      open(12, file=gdasFileHK,
     &         form='unformatted', access='direct', recl=mxR*myR*4,
     &         status='old')
      
      read(12, rec=1) ((r070_6hr(i,j), i=1,mxR), j=1,myR)

      close(12)

      return
      end
