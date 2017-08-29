  program tread
!
  integer, parameter :: imx=720, jmx=410, kass=30
  integer ig, jg, kg, year, month, day
  real, allocatable, dimension(:,:,:) :: tvv
!
  allocate (tvv(imx,jmx,kass))
!
! open(11,file='tvv.mom',form='unformatted', access='sequential')
  open(11,file='svv.mom',form='unformatted', access='sequential')
!
  read (11) year, month, day
  print *, year, month, day
  read (11) ig, jg, kg
  print *, ig, jg, kg
  read (11) tvv
  i = imx/2
  j = jmx/2
  k = kass/2
  print *, i, j, k
  print *, tvv(i,j,k)
  close(11)
!
  end program tread
