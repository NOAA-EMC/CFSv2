	subroutine getGDAS( mxR, myR, gdasFileHK, r190_6hr )
C=====================================================================
C	GET 5 DAYS GDAS PRATE
C
C	CALL BY: pentad2gldas.f
C	
C	20011128 JESSE MENG
C=====================================================================

      implicit none

C--- INPUT VARIABLES

      integer,		intent(in)	:: mxR
      integer,		intent(in)	:: myR
      character*100,	intent(in)	:: gdasFileHK
      real,		intent(out)	:: r190_6hr(mxR, myR)

C--- LOCAL VARIABLES

      integer				:: kunit
      save kunit
      data kunit /100/

      logical				:: exists

C--- GETGB

      integer			:: iret
      integer			:: jpds(200)
      integer			:: jgds(200)
      integer			:: kpds(200)
      integer			:: kgds(200)
      integer			:: kf
      integer			:: knum      
      logical*1			:: bitmap(mxR, myR)

C     ----------------------------------------------------------------

      kunit = kunit + 1

      print*, kunit, ' ', trim(gdasFileHK)
      
      r190_6hr = 0.

      inquire (file = gdasFileHK, exist = exists)
      if ( .not. exists ) then
         print*,'X-X-X GDAS FILE DOES NOT EXIST. CHECK PATH.'
         Print*,'X-X-X PROGRAM STOPS.'
         stop 1
      endif

      iret = 0
      call baopenr(kunit, gdasFileHK, iret)
      print*, "AFTER baopenr(",kunit,"), iret =", iret

      jpds = -1
      jgds = -1
      kpds = 0
      kpds = 0
      iret = 0

      call getgb(kunit, 0, mxR*myR, 28, jpds, jgds, 
     &              kf, knum, kpds, kgds, bitmap, r190_6hr, iret)
      print*, "AFTER getgb(",kunit,"), iret =", iret

      iret = 0
      call baclose(kunit, iret)

      return
      end
