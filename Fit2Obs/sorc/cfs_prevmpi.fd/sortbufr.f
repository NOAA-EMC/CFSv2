C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      subroutine sortbufr(lunot)

      character(80) fili
      character(8)  subset

      character(8), ALLOCATABLE :: subs(:)
      integer     , ALLOCATABLE :: indx(:)
      integer     , ALLOCATABLE :: isrt(:)

      data lumpi/60/
      data lumpo/61/

      include "mpif.h"

!--------------------------------------------------------------------------
      call mpi_comm_rank(MPI_COMM_WORLD,myid,ierr)
      call mpi_comm_size(MPI_COMM_WORLD,nprc,ierr)
!--------------------------------------------------------------------------

      if(myid/=0) return

!  concatenate the mpi bufr files back into one

      do n=1,nprc
      write(fili,'("mpi.fort.",i2.2)')n          
      open(lumpi,file=fili,form='unformatted')
      call openbf(lumpi,'IN',lumpi)
      if(n==1) call openbf(lumpo,'OUT',lumpi)
       do while(ireadmg(lumpi,subset,idate)==0)
       call copymg(lumpi,lumpo)         
       enddo
      call closbf(lumpi)
      enddo

!  itemize the message contents by subsets and sort

      call closbf(lumpo)
      call ufbmem(lumpo,0,nret,lumpo)
      allocate(subs(nret),indx(nret),isrt(nret))
      
      do n=1,nret
      call rdmemm(n,subset,idate,iret)
      subs(n)=subset    
      indx(n)=n
      enddo

      call orders(0,isrt,subs,indx,nret,1,8,i2)
      
!  collate the subsets into the output file

      call openbf(lunot,'OUT',lumpo)
      do n=1,nret
      call rdmemm(indx(n),subset,idate,iret)
      call copymg(lumpo,lunot)
      enddo

!  closed and finished

      call closbf(lunot)
      return
      end
