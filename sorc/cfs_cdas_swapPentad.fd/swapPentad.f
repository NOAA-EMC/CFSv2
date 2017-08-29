        program swapPentad
!=============================================================================
!        PREPROCESS UTILITY FOR PENTAD DISAGGREGATE.
!        CPC PENTAD PRODUCT IS IN LITTLE_ENDIAN;
!        OTHER MODEL PRODUCT (E.G., GDAS) IS IN BIG_ENDIAN;
!        THIS SOFTWARE GENERATE AN ASCII PENTAD,
!        EACH OUTPUT FILE IS FOR 1 PENTAD
!
!        INPUT AND OUTPUT ARE DEFINED IN swapPentad.in
!
!        JESSE 20031112
!=============================================================================

      implicit none

      integer iargc
      character*80 infile
      character*4 year
      integer p1
      integer p2

      character*2 pp
      character*80 outfile
      real data(144,72)

      integer i
      integer j
      integer k
      integer p

      logical exists

!----------------------------------------------------------------------------

      if(iargc() .lt. 1) then
         print*, "Usage swapPentad cmapFile"
         stop
      endif

      call getarg(1,infile)

      p1 = 1
      p2 = 73

      inquire (file = infile, exist = exists)
      if ( .not. exists ) then
         print*,"INPUT DATA FILE NOT EXIST !!!"
         print*,infile
         print*,"PROGRAM STOP !!!"
         stop
      endif

      open( 12, file=infile,
     &  form="unformatted", access="direct", recl=144*72,
     &     status="old" )

      do p = p1, p2

         write( pp, '(I2.2)' ) p
         outfile = "cmap_pentad"//pp//".dat"

         open( 21, file=outfile, status="unknown")

         do j = 1, 72
         do i = 1, 144
            data(i,j) = 0
         end do
         end do

         read( 12, rec=p, err=99 ) data

         do j = 1, 72
         do i = 1, 144
            call swap4( data(i,j) )
            write(21,*) data(i,j)
         end do
         end do

         close(21)

      end do
   99 close(12)

      k = p
      do p = k, 73
         write( pp, '(I2.2)' ) p
         outfile = "cmap_pentad"//pp//".dat"

         open( 21, file=outfile, status="unknown")

         do j = 1, 72
         do i = 1, 144
            data(i,j) = -999.
           ! write(21,*) data(i,j)
            write(21,'(F8.2)') data(i,j)
         end do
         end do

         close(21)

      end do

      stop
      end

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE Swap4( A )
!
!       REVERSE ORDER OF BYTES IN INTEGER*4 WORD, or REAL*4
!
!       COPY FROM I.LASZLO
!
      INTEGER  * 4 A
      CHARACTER  Ktemp * 1
      INTEGER  Itemp
      CHARACTER  Jtemp( 4 ) * 1
      EQUIVALENCE ( Jtemp(1), Itemp )

      Itemp  = A
      Ktemp  = Jtemp( 4 )
      Jtemp( 4 ) = Jtemp( 1 )
      Jtemp( 1 ) = Ktemp
      Ktemp  = Jtemp( 3 )
      Jtemp( 3 ) = Jtemp( 2 )
      Jtemp( 2 ) = Ktemp
      A      = Itemp

      RETURN

      END

