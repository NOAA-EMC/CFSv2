! xlf90 precipLIS.f -L/nwprod/lib/ -lbacio_4 -lw3_4 -o precipLIS
! Blend CPC gauge and CMAP precip for CFS LIS forcing.
! Jesse Meng 20100823

  integer narg, iargc
  character*8 yyyymmdd
  integer year
  integer month
  integer day

  integer, parameter :: mx = 720
  integer, parameter :: my = 360
  real :: dump(mx,my)
  real :: eod(mx,my)
  real :: day0(mx,my)
  real :: day1(mx,my)
  real :: day2(mx,my)

!--- 6hr gauge data

  real :: p06(mx,my)
  real :: p12(mx,my)
  real :: p18(mx,my)
  real :: p24(mx,my)
  real :: pday(mx,my)

!--- CMAP

  real :: cmap06(mx,my)
  real :: cmap12(mx,my)
  real :: cmap18(mx,my)
  real :: cmap24(mx,my)
  real :: cmapday(mx,my)

!--- Merged

  real :: w(mx,my)
  real :: m06(mx,my)
  real :: m12(mx,my)
  real :: m18(mx,my)
  real :: m24(mx,my)
  real :: mday(mx,my)

!--- READ DATE

  narg=iargc()
  if(narg.lt.1) then
    print*, "Usage: precipLIS.x YYYYMMDD"
    stop
  endif
  call getarg(1,yyyymmdd)
  read(yyyymmdd,'(I4,I2,I2)') year, month, day
  print*, yyyymmdd, year, month, day

!--- READ EOD DATA

  open(20,file='PRCP_CU_GAUGE_V1.0GLB_0.50deg_EOD.lnx',form='unformatted',access='direct',&
       recl=mx*my,status='unknown')
  read(20,rec=1) dump
  close(20)

  do j = 1, my
  do i = 1, mx
  !   call swap4(dump(i,j),eod(i,j))
      eod(i,j)=dump(i,j)
  enddo
  enddo
!--- READ 3 DAYS GAUGE DATA
  open(10,form='unformatted',access='direct',&
          recl=mx*my,status='unknown')
  read(10,rec=1) dump
  close(10)
  do j = 1, my
  do i = 1, mx
  !   call swap4(dump(i,j),day0(i,j))
      day0(i,j)=dump(i,j)
  enddo
  enddo
  day0 = day0 * 0.1

  open(11,form='unformatted',access='direct',&
          recl=mx*my,status='unknown')
  read(11,rec=1) dump
  close(11)
  do j = 1, my
  do i = 1, mx
  !   call swap4(dump(i,j),day1(i,j))
      day1(i,j)=dump(i,j)
  enddo
  enddo
  day1 = day1 * 0.1

  open(12,form='unformatted',access='direct',&
          recl=mx*my,status='unknown')
  read(12,rec=1) dump
  close(12)
  do j = 1, my
  do i = 1, mx
  !   call swap4(dump(i,j),day2(i,j))
      day2(i,j)=dump(i,j)
  enddo
  enddo
  day2 = day2 * 0.1

!--- MAP 00Z-00Z GAUGE DATA

  do j = 1, my
  do i = 1, mx

     if(eod(i,j) .EQ. -999.) then
       p06(i,j) = -999.
       p12(i,j) = -999.
       p18(i,j) = -999.
       p24(i,j) = -999.

     elseif(eod(i,j) .EQ. 0 ) then
       p06(i,j) = day2(i,j) * 0.25
       p12(i,j) = day2(i,j) * 0.25
       p18(i,j) = day2(i,j) * 0.25
       p24(i,j) = day2(i,j) * 0.25

     elseif(eod(i,j) .EQ. 6 ) then
       p06(i,j) = day1(i,j) * 0.25
       p12(i,j) = day2(i,j) * 0.25
       p18(i,j) = day2(i,j) * 0.25
       p24(i,j) = day2(i,j) * 0.25

     elseif(eod(i,j) .EQ. 12) then
       p06(i,j) = day1(i,j) * 0.25
       p12(i,j) = day1(i,j) * 0.25
       p18(i,j) = day2(i,j) * 0.25
       p24(i,j) = day2(i,j) * 0.25

     elseif(eod(i,j) .EQ. 15) then
       p06(i,j) = day1(i,j) * 0.25
       p12(i,j) = day1(i,j) * 0.25
       p18(i,j) = day1(i,j) * 0.125 + day2(i,j) * 0.125
       p24(i,j) = day2(i,j) * 0.25

     elseif(eod(i,j) .EQ. 18) then
       p06(i,j) = day1(i,j) * 0.25
       p12(i,j) = day1(i,j) * 0.25
       p18(i,j) = day1(i,j) * 0.25
       p24(i,j) = day2(i,j) * 0.25

     elseif(eod(i,j) .EQ. 24) then
       p06(i,j) = day1(i,j) * 0.25
       p12(i,j) = day1(i,j) * 0.25
       p18(i,j) = day1(i,j) * 0.25
       p24(i,j) = day1(i,j) * 0.25

     elseif(eod(i,j) .EQ. 27) then
       p06(i,j) = day0(i,j) * 0.125 + day1(i,j) * 0.125
       p12(i,j) = day1(i,j) * 0.25
       p18(i,j) = day1(i,j) * 0.25
       p24(i,j) = day1(i,j) * 0.25

     elseif(eod(i,j) .EQ. 30) then
       p06(i,j) = day0(i,j) * 0.25
       p12(i,j) = day1(i,j) * 0.25
       p18(i,j) = day1(i,j) * 0.25
       p24(i,j) = day1(i,j) * 0.25

     else
!       print*, j, i, eod(i,j)
     endif

     if( p06(i,j) .LT. 0. ) p06(i,j) = -9999.
     if( p12(i,j) .LT. 0. ) p12(i,j) = -9999.
     if( p18(i,j) .LT. 0. ) p18(i,j) = -9999.
     if( p24(i,j) .LT. 0. ) p24(i,j) = -9999.

     pday(i,j) = p06(i,j) + p12(i,j) + p18(i,j) + p24(i,j)
     if( pday(i,j).LT. 0. ) pday(i,j)= -9999.

  enddo
  enddo

!--- READ CMAP DATA

  open(31,form='unformatted',access='direct',&
          recl=mx*my,status='unknown')
  read(31,rec=1) (cmap06(:,j),j=360,1,-1)
  where ( cmap06 .GE. 0. )
          cmap06 = cmap06 * 21600.
  endwhere
  close(31)
  open(32,form='unformatted',access='direct',&
          recl=mx*my,status='unknown')
  read(32,rec=1) (cmap12(:,j),j=360,1,-1)
  where ( cmap12 .GE. 0. )
          cmap12 = cmap12 * 21600.
  endwhere
  close(32)
  open(33,form='unformatted',access='direct',&
          recl=mx*my,status='unknown')
  read(33,rec=1) (cmap18(:,j),j=360,1,-1)
  where ( cmap18 .GE. 0. )
          cmap18 = cmap18 * 21600.
  endwhere
  close(33)
  open(34,form='unformatted',access='direct',&
          recl=mx*my,status='unknown')
  read(34,rec=1) (cmap24(:,j),j=360,1,-1)
  where ( cmap24 .GE. 0. )
          cmap24 = cmap24 * 21600.
  endwhere
  close(34)

  cmapday = cmap06 + cmap12 + cmap18 + cmap24

  where( cmapday .GT. 0. .AND. pday .GT. 0. )
         p06 = pday * cmap06 / cmapday
         p12 = pday * cmap12 / cmapday
         p18 = pday * cmap18 / cmapday
         p24 = pday * cmap24 / cmapday
  end where
!--- MERGE GAUGE AND CMAP
  
  open(50,file='weight_gage_cmap.bin',form='unformatted',access='direct',&
          recl=mx*my,status='unknown')
  read(50,rec=1) dump
  close(50)
  do j = 1, my
  do i = 1, mx
     call swap4(dump(i,j),w(i,j))
  enddo
  enddo

  m06 = p06
  m12 = p12
  m18 = p18
  m24 = p24

  where( pday+cmapday .GE. 0. )
    m06 = p06 * w + cmap06 * (1.-w)
    m12 = p12 * w + cmap12 * (1.-w)
    m18 = p18 * w + cmap18 * (1.-w)
    m24 = p24 * w + cmap24 * (1.-w)
  end where

  mday = m06 + m12 + m18 + m24
  where( m06  .LT. 0. ) m06 = -9999.
  where( m12  .LT. 0. ) m12 = -9999.
  where( m18  .LT. 0. ) m18 = -9999.
  where( m24  .LT. 0. ) m24 = -9999.
  where( mday .LT. 0. ) mday = -9999.
!--- OUTPUT
  
  call gribout(m06, year, month, day, 0)
  call gribout(m12, year, month, day, 6)
  call gribout(m18, year, month, day, 12)
  call gribout(m24, year, month, day, 18)

!---
  stop
  end


!----------------------------------------!
  SUBROUTINE Swap4( A, B )
      INTEGER  * 4 A, B
!C     ..
!C     .. Local Scalars ..
      CHARACTER  Ktemp * 1
      INTEGER  Itemp
!C     ..
!C     .. Local Arrays ..
      CHARACTER  Jtemp( 4 ) * 1
!C     ..
!C     .. Equivalences ..
      EQUIVALENCE ( Jtemp(1), Itemp )
!C     ..

      Itemp  = A
      Ktemp  = Jtemp( 4 )
      Jtemp( 4 ) = Jtemp( 1 )
      Jtemp( 1 ) = Ktemp
      Ktemp  = Jtemp( 3 )
      Jtemp( 3 ) = Jtemp( 2 )
      Jtemp( 2 ) = Ktemp
      B      = Itemp

      RETURN
      END


!--------------------------------------------------!
  subroutine gribout(data1, year, month, day, hour)   

  character*25 gribfile
  integer :: i, j1, j2
  integer, parameter :: mx = 720
  integer, parameter :: my = 360
  real               :: data1(mx,my) !S-N
  real               :: data2(mx,my) !N-S

  integer            :: year
  integer            :: month
  integer            :: day
  integer            :: hour
  integer            :: kpds(200)
  integer            :: kgds(200)
  logical*1          :: bitmap(mx,my)
  integer            :: iret

!---

  write(gribfile,'(A13,I4.4,3(I2.2))') 'precip.gldas.', year, month, day, hour  

  j2 = 1
  do j1 = my, 1, -1
     data2(:,j2) = data1(:,j1)
     j2 = j2 + 1
  enddo

  where ( data2 .GE. 0. )
    data2 = data2 / 6. / 3600.
  end where

  bitmap = .TRUE.
  where ( data2 .LT. 0. )
    bitmap = .FALSE.
  end where

  kpds = 0
      kpds(1) = 7
      kpds(2) = 141
      kpds(3) = 235
      kpds(4) = 192
      kpds(5) = 59
      kpds(6) = 1
      kpds(7) = 0
      kpds(8) = mod(year,100)
      kpds(9) = month
      kpds(10)= day
      kpds(11)= hour
      kpds(12)= 0
      kpds(13)= 1
      kpds(14)= 0
      kpds(15)= 6
      kpds(16)= 3
      kpds(17)= 1
      kpds(18)= 0
      kpds(19)= 130
      kpds(20)= 0
      kpds(21)= year/100 + 1
      kpds(22)= 8
      kpds(23)= 4
      kpds(24)= 0
      kpds(25)= 0

  kgds = 0
      kgds(1) = 0
      kgds(2) = mx
      kgds(3) = my
      kgds(4) =   89750
      kgds(5) =     250
      kgds(6) = 128
      kgds(7) =  -89750
      kgds(8) =  359750
      kgds(9) =     500
      kgds(10)=     500
      kgds(11)= 0
      kgds(12)= 0
      kgds(13)= 0
      kgds(14)= 0
      kgds(15)= 0
      kgds(16)= 0
      kgds(17)= 0
      kgds(18)= 0
      kgds(17)= 0
      kgds(18)= 0
      kgds(19)= 0
      kgds(20)= 255
      kgds(21)= 0
      kgds(22)= 0
      kpds(23)= 0
      kpds(24)= 0
      kpds(25)= 0

  iret = 0
  call baopen(12,gribfile,iret)
  iret = 0
  call putgb(12,mx*my,kpds,kgds,bitmap,data2,iret)
  iret = 0
  call baclose(12, iret)

  return
  end

