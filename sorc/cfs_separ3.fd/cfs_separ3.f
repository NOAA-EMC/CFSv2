      program separ3
      implicit none
      character*2048 s
      character*80 in,out(0:2)
      integer npf(0:2),npl(0:2)
      integer i, io, iargc, j, k, n, narg

      narg=iargc()
      if (narg.ne.4) then
        print*,'separ3: N. of agruments != 4, terminated'
        call abort
      end if
      call getarg(1,in)
      open(7,file=in,status='old',form='formatted',iostat=io)
      if (io.ne.0) then
        print*,'separ3: error opening '//in
        call abort
      end if
      do i=0,2
        call getarg(i+2,out(i))
        open(i+8,file=out(i),status='unknown',form='formatted',
     &  iostat=io)
        if (io.ne.0) then
          print*,'separ3: error opening '//out(i)
          call abort
        end if
      end do

      read(5,*,iostat=io) npf,npl
      if (io.ne.0) then
        print*,'separ3: error reading stdin'
!       write(0,*)'separ3: error reading stdin'
        call abort
      end if

      n=0

      loop: do while (.true.)
        n=n+1
        read(7,'(a2048)',end=1,err=2) s
        if (len_trim(s) == 0) cycle  ! to ignore spurious blank lines
                                     ! resulting from system's bad work
        if (s(5:5).ne.':') then
          if (n == 1) cycle          ! system adds helpful info at beginning
          if (s(1:3) == 'Job') then  ! wcoss adds extra lines at the end

            print *,'separ3: Line with Job info encountered -'
     &,      ' remaining lines are not separable and ignored'
            stop
          else
            print*,
!           write(0,*)
     &'separ3: error, 5th symbol not ":" but "'//s(5:5)//'" in line ',n
            print*,s(1:len_trim(s))
            call abort
          end if
        end if
        read(s(1:4),*) k
        j=-1
        do i=0,2
          if (npf(i).le.k .and. k.le.npl(i)) then
            write(i+8,*) s(1:len_trim(s))
            cycle loop
          end if
        end do
!       write(0,*)'separ3: error, wrong beginning in line ',n
        print*,'separ3: error, wrong beginning in line ',n
        call abort
      end do loop

    1 continue
      stop

    2 continue
      print*,'separ3: error reading '//in
!     write(0,*)'separ3: error reading '//in
      call abort

      END
