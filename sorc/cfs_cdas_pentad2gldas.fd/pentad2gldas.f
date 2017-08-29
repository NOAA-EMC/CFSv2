	program pentad2gldas
C=====================================================================
C	DISAGGREGATTE CPC/CMAP PENTAD (2.5 DEG, 5 DAY, MM/DAY)
C       BASED ON      EMC/GDAS PRATE  (T62  1.9 DEG, 6 HR,  KG/M2/S)
C                                     (T170 0.7 DEG, 6 HR,  KG/M2/S)
C                                     (T254 0.5 DEG, 6 HR,  KG/M2/S)
C                                     (T382 0.3 DEG, 6 HR,  KG/M2/S)
C                                     (1 KG/M2/S = 86400 MM/DAY)
C
C	COMPILE: makefile
C	INPUT : pentad2gldas.in
C	OUTPUT: AS DESCRIBED IN output.files
C
C	METHOD:
C	1. GET A PENTAD CMAP <P> (2.5 DEG, 5 DAYS ACCUMULATE)
C	2. GET THE CORRESPONDING 5 DAYS GDAS <R> 
C	   (0.7 DEG, 6 HR)
C	3. IPOLATE(BUDGET) <R> TO 2.5 DEG, 6 HR, -> <A>
C	4. SUM <A> TO 2.5 DEG, 5 DAYS, -> <B>
C       5. EVALUATE <B> AND <P>, CREATE NEW POINTER ARRAYS
C	   <WI> AND <WJ>
C          
C	  +----------+-----------+------------------------------+
C         |          | <P> = 0   | <P> != 0                     | 
C         |----------+-----------+------------------------------|
C         | <B>  = 0 | <WI> = I  | <WI> = I OF NEAREST <B> != 0 |
C	  |          | <WJ> = J  | <WJ> = J OF NEAREST <B> != 0 |
C	  |----------+-----------+------------------------------|
C	  | <B> != 0 | <WI> = I  | <WI> = I                     |
C	  |          | <WJ> = J  | <WJ> = J                     |
C         +----------+-----------+------------------------------+
C	   
C	6. TEMPORALLY DISAGGREGATE <P> TO 2.5 DEG, 6 HR, -> <Q>
C		Q(I,J) = P(I,J) * A(WI,WJ) / B(WI,WJ)
C		WHICH AGREE WITH <A> IN TIME PHASE
C		AND AGREE WITH <P> IN 5 DAYS ACCUMULATE
C
C	7. IPOLATE(NEIGHBOR) <Q> TO 0.7 DEG, 6 HR, -> <QQ>
C          IPOLATE(NEIGHBOR) <A> TO 0.7 DEG, 6 HR, -> <AA>
C
C       8. GENERATE <G>, 0.7 DEG, 6 HR, THAT
C
C	  +-----------+-------------+---------------------------+
C         |           | <QQ> = 0    | <QQ> != 0                 | 
C         |-----------+-------------+---------------------------|
C         | <AA>  = 0 | <G>  = <QQ> | <G>   = <QQ>              |
C	  |-----------+-------------+---------------------------|
C	  | <AA> != 0 | <G>  = <QQ> | <G>   = <R> * <QQ> / <AA> |
C         +-----------+-------------+---------------------------+
C
C	9. OUTPUT <G> IN GRIB FORMAT
C
C	UPDATE:
C	20050519 JESSE MENG
C=====================================================================

      implicit none

      real,	parameter	:: undef = -999.

      integer,	parameter	:: maxt  = 24	!max possible 6 hourly
						!time intervals in 1 pentad
					   	!24 in leap year pantad 12
      integer,	parameter	:: mxP = 144	!dimension of P array
      integer,	parameter	:: myP =  72	!resolution = 2.5 deg

!      integer,	parameter	:: mxR = 192	!dimension of R array
!      integer,	parameter	:: myR =  94	!T62  resolution 1.9
!      integer,	parameter	:: mxR = 512	!dimension of R array
!      integer,	parameter	:: myR = 256	!T170 resolution 0.7
!      integer,	parameter	:: mxR = 768	!dimension of R array
!      integer,	parameter	:: myR = 384	!T254 resolution 0.5
      integer, parameter       :: mxR =1152    !dimension of R array
      integer, parameter       :: myR = 576    !T382 resolution 0.3

      integer			:: h
      integer			:: i
      integer			:: i2
      integer			:: j
      integer			:: j2
      integer			:: k
      integer			:: kday
      integer			:: nn

      integer			:: ioUnit

C---  INPUT PARAMETERS
     
      integer			:: mday		!days in current pentad
      integer			:: year
      integer                   :: iyear(maxt)
      integer			:: imonth(maxt)
      integer			:: iday(maxt)
      integer			:: ihour(maxt)
      character*100		:: gdasPath
      character*100		:: pentadPath
      character*100		:: outPath
      integer                   :: options(10)
C				    1(0) = PENTAD conserved unecessary
C				    1(1) = PENTAD conserved
C				    2(0) = OUTPUT ONLY FINAL PRODUCT
C				    2(1) = OUTPUT ALL INTERMEDIATE DATA

C---  CONVERTED INPUT PARAMETERS

      integer			:: mt		!total tsteps in current pentad
      character*4               :: cyear(maxt)
      character*2               :: cmonth(maxt)
      character*2               :: cday(maxt)
      character*2               :: chour(maxt)

C---  FILENAME CONTROL VARIABLES

      character*100		:: gdasFile(maxt)
      character*100		:: pentadFile

C---  ORIGINAL PRECIP DATA VARIABLES

      real			:: p250_5dy(mxP, myP)	!PentadCmap
      real			:: r190_6hr(mxR, myR)	!GDAS
      real                      :: land    (mxR, myR)  	!GDAS LMASK

C---  INTERMEDIATE CALCULATED VARIABLES

      real			:: a250_6hr(mxP, myP, maxt)!4files*6days	
      real			:: b250_5dy(mxP, myP)

      integer			:: i250_6hr(mxP, myP)	!count of a250_6hr
      
      integer			:: w
      integer			:: wi(mxP, myP)		!pointer array
      integer			:: wj(mxP, myP)		!pointer array
            
      real 			:: wd(mxP, myP)		!circles count of 
							!weighting pointer
							!to real grid
      real 			:: q250_6hr(mxP, myP)

      real			:: aa190_6hr(mxR, myR, maxt)!4files*6days	
      real 			:: qq190_6hr(mxR, myR)

C---  FINAL PRODUCT

      real			:: g190_6hr(mxR, myR)
      real                      :: g050_6hr(720, 360)

C---  GRIB AND IPOLATES PARAMETERS
      
      integer					:: kpds(200)
      integer					:: kgds(200)
      integer                                   :: jpds5

      integer 					:: ip
      integer 					:: ipopt(20)
      integer 					:: kgdsi(22)
      integer 					:: kgdso(22)
      integer					:: mi
      integer					:: mo
      integer,	parameter			:: km = 1
      integer					:: ibi
      logical*1,allocatable, dimension(:)	:: li	!li(mi)
      real,	allocatable, dimension(:)	:: hi	!hi(mi)

      integer					:: no
      real, 	allocatable, dimension(:)	:: rlat	!rlat(mo)
      real,	allocatable, dimension(:)	:: rlon	!rlon(mo)
      integer					:: ibo
      logical*1,allocatable, dimension(:)	:: lo	!lo(mo)
      real,	allocatable, dimension(:)	:: ho	!ho(mo)
      integer					:: iret

C     ----------------------------------------------------------------
C	0. READ INPUT FILE
C     ----------------------------------------------------------------

      print*
      print*,"0. READ INPUT FILE"

      call input( mday, year, iyear, imonth, iday, ihour,
     &            gdasPath, pentadFile, outPath, options )

      mt = mday * 4

      do h = 1, mt

         write( cyear(h),  '(I4.4)' ) iyear(h)
         write( cmonth(h), '(I2.2)' ) imonth(h)
         write( cday(h),   '(I2.2)' ) iday(h)
         write( chour(h),  '(I2.2)' ) ihour(h)

!-------
! BINARY
!         gdasFile(h) = 
!     &           trim(gdasPath)//cyear(h)//cmonth(h)//'/'//
!     &           'prate_gdas_'//cyear(h)//cmonth(h)//cday(h)//chour(h)//
!     &           '.grd'
!------
! GRIB
         gdasFile(h) = 
     &           trim(gdasPath)//
     &           'gdas.'//cyear(h)//cmonth(h)//cday(h)//'/'//
     &           'gdas1.t'//chour(h)//'z.sfluxgrbf06'
!-------

      end do

      do h = 1, mt
         print*, gdasFile(h)
      end do

      print*, pentadFile
      print*, outPath

C     ----------------------------------------------------------------
C	   GET GDAS LMASK
C     ----------------------------------------------------------------

         jpds5 = 81
         call getGDAS( mxR, myR, gdasFile(1), jpds5, land, kpds, kgds )

C     ----------------------------------------------------------------
C	1. GET A PENTAD CMAP <P>
C     ----------------------------------------------------------------

      print*,"1. GET A PENTAD CMAP <P>"

      call getPentad( mxP, myP, pentadFile, p250_5dy )
       
      if( options(2) .EQ. 1 ) then
        ioUnit = 21
        call output( outpath, ioUnit, mxP, myP, 1, p250_5dy )      
      end if

C     ----------------------------------------------------------------
C	2. GET 5 DAYS GDAS_PRATE <R>
C	3. IPOLATE(BUDGET) <R> TO 2.5 DEG, 6 HR, -> <A>
C     ----------------------------------------------------------------

      print*,"2. GET 5 DAYS GDAS_PRATE <R>"
      print*,"3. IPOLATE(BUDGET) <R> TO 2.5 DEG, 6 HR, -> <A>"

C     ----------------------------------------------------------------
C	3.1. INITIALIZE INPUT GDS (T382 GLOBAL GAUSSIAN)
C     ----------------------------------------------------------------

      kgdsi(1) = 4
      kgdsi(2) = mxR
      kgdsi(3) = myR
      kgdsi(4) =  89761
      kgdsi(5) =      0
      kgdsi(6) = 128
      kgdsi(7) = -89761 
      kgdsi(8) =  -3125
      kgdsi(9) =   3125
      kgdsi(10)= 288
      kgdsi(11)= 0
      kgdsi(12)= 0
      kgdsi(13)= 0
      kgdsi(14)= 0 
      kgdsi(15)= 0
      kgdsi(16)= 0
      kgdsi(17)= 0
      kgdsi(18)= 0
      kgdsi(19)= 0
      kgdsi(20)= 255
      kgdsi(21)= 0
      kgdsi(22)= 0

C     ----------------------------------------------------------------
C	3.2. INITIALIZE OUTPUT GDS (GLOBAL LAT/LON 2.5 DEG) 
C     ----------------------------------------------------------------

      kgdso(1) = 0
      kgdso(2) = mxP
      kgdso(3) = myP
      kgdso(4) =  -88750
      kgdso(5) =    1250
      kgdso(6) = 128
      kgdso(7) =   88750
      kgdso(8) =   -1250
      kgdso(9) =    2500
      kgdso(10)=    2500
      kgdso(11)= 64
      kgdso(12)= 0
      kgdso(13)= 0
      kgdso(14)= 0 
      kgdso(15)= 0
      kgdso(16)= 0
      kgdso(17)= 0
      kgdso(18)= 0
      kgdso(19)= 0
      kgdso(20)= 255
      kgdso(21)= 0
      kgdso(22)= 0

C     ----------------------------------------------------------------
C	3.3. MAIN LOOP START
C     ----------------------------------------------------------------

      do 400 h = 1, mt

         print*
         print*,"3.4. IPOLATE <R> -> <A>   TIMESTEP", h

         jpds5 = 59
         call getGDAS( mxR,myR,gdasFile(h),jpds5,r190_6hr,kpds,kgds )

         if( options(2) .EQ. 1 ) then
           ioUnit = 1000 + h
           call output( outpath, ioUnit, mxR, myR, 1, r190_6hr )
         endif

         where( r190_6hr .GT. 0. )
                r190_6hr = r190_6hr * 86400.
         endwhere

C     ----------------------------------------------------------------
C	3.4. ALLOCATE MEMORY
C     ----------------------------------------------------------------

      mi = mxR * myR

      allocate ( li(mi) )
      allocate ( hi(mi) )

      mo = mxP * myP

      allocate ( rlat(mo) )
      allocate ( rlon(mo) )
      allocate ( lo(mo) )
      allocate ( ho(mo) )

C     ----------------------------------------------------------------
C	3.5. IPOLATES
C     ----------------------------------------------------------------

      ip       = 3
      ipopt(1) = -1
      ipopt(2) = -1

      ibi = 1
      li  = .FALSE.

      k = 0
      do j = 1, myR
      do i = 1, mxR
         k = k + 1
         hi(k) = r190_6hr(i,j)
         li(k) = ( hi(k) .GE. 0. )
      end do
      end do

      print*,'IPOLATES'

      iret = 0
      call ipolates(ip, ipopt, kgdsi, kgdso, mi, mo, km, ibi, li, hi,
     &              no, rlat, rlon, ibo, lo, ho, iret)

      print*,'AFTER ipolates, iret = ',iret

      if(iret .NE. 0) then
         print*, "X IPOLATES ERROR (R->A) !! iret =", iret
         print*, "X h =", h
         print*, "X PROGRAM STOP !!"
         call exit(iret)
      endif

      k = 0
      do j = 1, myP
      do i = 1, mxP
         k = k + 1
         a250_6hr(i,j,h) = ho(k)
      end do
      end do

      if( options(2) .EQ. 1 ) then
        ioUnit = 2000 + h
        call output( outpath, ioUnit, mxP, myP, 1, 
     &               a250_6hr(:,:,h)         )
      endif

C     ----------------------------------------------------------------
C	3.6. DEALLOCATE MEMORY
C     ----------------------------------------------------------------

      deallocate ( li )
      deallocate ( hi )
      deallocate ( rlat )
      deallocate ( rlon )
      deallocate ( lo )
      deallocate ( ho )

C     ----------------------------------------------------------------
C	4. SUM <A> TO 2.5 DEG, 5 DAYS, -> <B>
C     ----------------------------------------------------------------

      do j = 1, myP
      do i = 1, mxP
        if ( a250_6hr(i,j,h) .GE. 0. ) then
           i250_6hr(i,j) = i250_6hr(i,j) + 1
           b250_5dy(i,j) = b250_5dy(i,j) + a250_6hr(i,j,h)
        end if
      end do
      end do

  400 continue

      print*
      print*,"4. SUM <A> TO 2.5 DEG, 5 DAYS, -> <B>"
      
      where ( i250_6hr .GT. 0 )
            b250_5dy = b250_5dy / i250_6hr
      elsewhere
            b250_5dy = undef
      end where

      if( options(2) .EQ. 1 ) then
        ioUnit = 2000
        call output( outpath, ioUnit, mxP, myP, 1, b250_5dy )
      end if

C     ----------------------------------------------------------------
C	5. SETUP WEIGHTING POINTERS WI, WJ FOR EACH 2.5 DEGREE BOX
C	   SEARCH FOR NEAREST POINT THAT <B> ALWAYS .GT. 0
C     ----------------------------------------------------------------

      print*
      print*,"5. SETUP WEIGHTING POINTERS"

      do j = 1, myP
         do i = 1, mxP
            wi(i,j) = i
            wj(i,j) = j
         end do
      end do

      if ( options(1) .EQ. 1 ) then

         print*,"-. SEARCH FOR NEAREST NONZERO GRID"
      
         do j = 1, myP
            do i = 1, mxP
               if ( p250_5dy(i,j) .GT. 0. .AND. 
     &              b250_5dy(i,j) .LE. 0.      ) then
                  call circle(mxP, myP, b250_5dy, i, j, i2, j2, nn)
!                  write(*,1001) i,j,i2,j2
                  wi(i,j) = i2
                  wj(i,j) = j2
                  wd(i,j) = nn
               end if 
            end do
         end do

      end if 

      print*

      if( options(2) .EQ. 1 ) then
        ioUnit = 91
        call output( outpath, ioUnit, mxP, myP, 1, wd )
      end if

C     ----------------------------------------------------------------
C	6. DISAGGREGATE <P> -> <Q>, 2.5 DEG, 6HR
C		Q(i,j) = P(i,j) * A(wi,wj) / B(wi,wj)
C       7. IPOLATES(NEIGHBOR) <Q> 2.5 DEG --> <QQ> 1.9 DEG
C          IPOLATES(NEIGHBOR) <A> 2.5 DEG --> <AA> 1.9 DEG
C     ----------------------------------------------------------------

      print*
      print*,"6. DISAGGREGATE <P> TO 2.5 DEG, 6HR, -> <Q>"

C     ----------------------------------------------------------------
C	6.1. INITIALIZE INPUT GDS (GLOBAL LAT/LON 2.5 DEG)
C     ----------------------------------------------------------------

      kgdsi(1) = 0
      kgdsi(2) = mxP
      kgdsi(3) = myP
      kgdsi(4) =  -88750
      kgdsi(5) =    1250
      kgdsi(6) = 128
      kgdsi(7) =   88750
      kgdsi(8) =   -1250
      kgdsi(9) =    2500
      kgdsi(10)=    2500
      kgdsi(11)= 64
      kgdsi(12)= 0
      kgdsi(13)= 0
      kgdsi(14)= 0 
      kgdsi(15)= 0
      kgdsi(16)= 0
      kgdsi(17)= 0
      kgdsi(18)= 0
      kgdsi(19)= 0
      kgdsi(20)= 255
      kgdsi(21)= 0
      kgdsi(22)= 0

C     ----------------------------------------------------------------
C	6.2. INITIALIZE OUTPUT GDS (GLOBAL GAUSSIAN 0.7 DEG)
C     ----------------------------------------------------------------

      kgdso(1) = 4
      kgdso(2) = mxR
      kgdso(3) = myR
      kgdso(4) =  89761
      kgdso(5) =      0
      kgdso(6) = 128
      kgdso(7) = -89761
      kgdso(8) =  -3125
      kgdso(9) =   3125
      kgdso(10)= 288
      kgdso(11)= 0
      kgdso(12)= 0
      kgdso(13)= 0
      kgdso(14)= 0 
      kgdso(15)= 0
      kgdso(16)= 0
      kgdso(17)= 0
      kgdso(18)= 0
      kgdso(19)= 0
      kgdso(20)= 255
      kgdso(21)= 0
      kgdso(22)= 0

C     ----------------------------------------------------------------
C	6.3. ALLOCATE MEMORY
C     ----------------------------------------------------------------

      mi = mxP * myP

      allocate ( li(mi) )
      allocate ( hi(mi) )

      mo = mxR * myR

      allocate ( rlat(mo) )
      allocate ( rlon(mo) )
      allocate ( lo(mo) )
      allocate ( ho(mo) )

      ip  = 3
      ipopt(1) = -1
      ipopt(2) = -1

      ibi = 1
      li  = .FALSE.

      do 900 h = 1, mt

C     ----------------------------------------------------------------
C	6.4. CALCULATE <Q>
C     ----------------------------------------------------------------

         print*
         print*,"6.4.",h," CALCULATE <Q>"

         k = 0

         do j = 1, myP
         do i = 1, mxP

            i2 = wi(i,j)
            j2 = wj(i,j)
            if ( p250_5dy(i,j) .GT. 0. ) then
               q250_6hr(i,j) = p250_5dy(i,j) * 
     &                         a250_6hr(i2,j2,h) / b250_5dy(i2,j2)
            else
               q250_6hr(i,j) = p250_5dy(i,j)
            end if

            if ( a250_6hr(i2,j2,h) .LT. 0 ) then
               q250_6hr(i,j) = undef
            endif
 
            k = k + 1
            hi(k) = q250_6hr(i,j)
            li(k) = ( hi(k) .GE. 0. )

         end do
         end do

         if( options(2) .EQ. 1 ) then
           ioUnit = 3000 + h
           call output( outpath, ioUnit, mxP, myP, 1, q250_6hr )
         endif

C     ----------------------------------------------------------------
C	7.1. IPOLATES(NEIGHBOR) <Q> 2.5 DEG --> <QQ> 1.9 DEG
C     ----------------------------------------------------------------

         print*
         print*,"7. IPOLATES <Q> TO 1.9 DEG -> <QQ>"
         print*,"   IPOLATES <A> TO 1.9 DEG -> <AA>"

         ip = 2
         iret = 0
         call ipolates(ip, ipopt, kgdsi, kgdso, mi, mo, km, ibi, li, hi,
     &                 no, rlat, rlon, ibo, lo, ho, iret)

         print*,"AFTER  ipolates(Q->QQ), iret =", iret         

         k = 0
         do j = 1, myR
         do i = 1, mxR
            k = k + 1
            qq190_6hr(i,j) = ho(k)
         end do
         end do

C     ----------------------------------------------------------------
C	7.2. IPOLATES <A> 2.5 DEG --> <AA> 1.9 DEG
C     ----------------------------------------------------------------

         k = 0
         do j = 1, myP
         do i = 1, mxP
            k = k + 1
            hi(k) = a250_6hr(i,j,h)
            li(k) = ( hi(k) .GE. 0. )
         end do
         end do

         ip = 2
         iret = 0
         call ipolates(ip, ipopt, kgdsi, kgdso, mi, mo, km, ibi, li, hi,
     &                 no, rlat, rlon, ibo, lo, ho, iret)

         print*,"AFTER  ipolates(A->AA), iret =", iret         

         k = 0
         do j = 1, myR
         do i = 1, mxR
            k = k + 1
            aa190_6hr(i,j,h) = ho(k)
         end do
         end do

C     ----------------------------------------------------------------
C	8. GENERATE <G>, 1.9 DEG, 6 HR
C	   SET UNIT = KG/M2/S
C     ----------------------------------------------------------------
      
         print*
         print*,"8. GENERATE <G>, 1.9 DEG, 6 HR"

         jpds5 = 59
         call getGDAS( mxR,myR,gdasFile(h),jpds5,r190_6hr,kpds,kgds )

         where( r190_6hr .GT. 0.)
              r190_6hr = r190_6hr * 86400.
         endwhere

         do j = 1, myR
         do i = 1, mxR

            if ( aa190_6hr(i,j,h) .GT. 0. ) then
               g190_6hr(i,j) = qq190_6hr(i,j) * 
     &                         r190_6hr(i,j) / aa190_6hr(i,j,h)
            else
               g190_6hr(i,j) = qq190_6hr(i,j)
            end if

          !  if ( r190_6hr(i,j).LT.0. .OR. land(i,j).LE.0.) then
            if ( r190_6hr(i,j).LT.0. ) then
               g190_6hr(i,j) = undef
            endif

            if ( g190_6hr(i,j) .GT. 0. ) then
               g190_6hr(i,j) = g190_6hr(i,j) / 86400. !RATE [KG/M2/S]
          !     g190_6hr(i,j) = g190_6hr(i,j) / 4.     !6 HR ACCUMULATE [KG/M2] = [MM]
            endif

         end do
         end do

      if( options(2) .EQ. 1 ) then

          ioUnit = 4000 + h
          call output( outpath, ioUnit, mxR, myR, 1, 
     &                                    aa190_6hr(:,:,h))
          ioUnit = 5000 + h
          call output( outpath, ioUnit, mxR, myR, 1, qq190_6hr )

          ioUnit = 6000 + h
          call output( outpath, ioUnit, mxR, myR, 1, g190_6hr )

      end if

C     ----------------------------------------------------------------
C       9. FINAL OUTPUT
C     ----------------------------------------------------------------

      ioUnit = 8000 + h
      call output( outpath, ioUnit, mxR, myR, 1, g190_6hr )
      ioUnit = 800 + h
      call outgrib( outpath, ioUnit, mxR, myR, g190_6hr, kpds, kgds )

 900  continue

C     ----------------------------------------------------------------
C	FORMAT
C     ----------------------------------------------------------------
 1001 format( '(',I4,I4,')->(',I4,I4,')' )
C     ----------------------------------------------------------------

      stop
      end
