!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
      program mpi_combine

      character(255) exec,parm,file

      include "mpif.h"

!--------------------------------------------------------------------------
      call mpi_init(ierr)
      call mpi_comm_rank(MPI_COMM_WORLD,myid,ierr)
      call mpi_comm_size(MPI_COMM_WORLD,nprc,ierr)
!--------------------------------------------------------------------------

      IF(IARGC()<2) print*,'mpi_combine <exec> <parm>'
      IF(IARGC()<2) stop 9999

      call getarg(1,exec); !exec = TRIM(exec)//CHAR(0)
      call getarg(2,parm); !parm = TRIM(parm)//CHAR(0)
      open(5,file=parm) ! open parm in lieu of stdin
      open(6,recl=120)

      irec=0;jrec=0

1     READ(5,'(a)',end=100) file
      if(myid==jrec) then
         write(6,'(i3,1x,a)') myid,trim(file)
         call system(trim(exec)//' '//trim(file))
      endif
      irec=irec+1;jrec=mod(irec,nprc)
      goto 1

100   call mpi_finalize(iret)

      stop
      end

