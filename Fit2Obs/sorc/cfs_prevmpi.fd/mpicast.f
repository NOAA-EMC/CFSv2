C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      subroutine mpicast

      use gblevn_module

      include "mpif.h"

C-----------------------------------------------------------------------
      call mpi_comm_rank(MPI_COMM_WORLD,myid,ierr)
      call mpi_comm_size(MPI_COMM_WORLD,nprc,ierr)
      mcom = mpi_comm_world
      mre4 = mpi_real
      mre8 = mpi_real8
      mint = mpi_integer
      mchr = mpi_character
C-----------------------------------------------------------------------


      do kbak=1,nbak; kbam=kbak-1

      call mpi_barrier(mcom,ierr)

      if(myid==0)print*,'mpicast - background',kbak

!  broadcast background grid parameters 

      if(kbak==1) then
         call mpi_bcast(imax,1,mint,kbam,mcom,ierr)
         call mpi_bcast(jmax,1,mint,kbam,mcom,ierr)
         call mpi_bcast(kmax,1,mint,kbam,mcom,ierr)
         call mpi_bcast(dlat,1,mre4,kbam,mcom,ierr)
         call mpi_bcast(dlon,1,mre4,kbam,mcom,ierr)
         call suterps ! sets up the interpolation latitudes  
         if(myid>=nbak) call allobaks
      endif

!  broadcast background time levels

      ijmax=imax*jmax;ikmax=ijmax*kmax
      call mpi_bcast(iarps(1,1,kbak)  ,ijmax,mre4,kbam,mcom,ierr)
      call mpi_bcast(iarzs(1,1,kbak)  ,ijmax,mre4,kbam,mcom,ierr)
      call mpi_bcast(iarqq(1,1,1,kbak),ikmax,mre4,kbam,mcom,ierr)
      call mpi_bcast(iartt(1,1,1,kbak),ikmax,mre4,kbam,mcom,ierr)
      call mpi_bcast(iaruu(1,1,1,kbak),ikmax,mre4,kbam,mcom,ierr)
      call mpi_bcast(iarvv(1,1,1,kbak),ikmax,mre4,kbam,mcom,ierr)
      call mpi_bcast(iarpl(1,1,1,kbak),ikmax,mre4,kbam,mcom,ierr)
      call mpi_bcast(iarzl(1,1,1,kbak),ikmax,mre4,kbam,mcom,ierr)

      enddo ! broadcast finished

      return
      end
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      subroutine allobaks

      use gblevn_module

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      if(.not.allocated(iarps)) allocate (iarps(imax,jmax,nbak))
      if(.not.allocated(iarzs)) allocate (iarzs(imax,jmax,nbak))
      if(.not.allocated(iarqq)) allocate (iarqq(imax,jmax,kmax,nbak))
      if(.not.allocated(iartt)) allocate (iartt(imax,jmax,kmax,nbak))
      if(.not.allocated(iaruu)) allocate (iaruu(imax,jmax,kmax,nbak))
      if(.not.allocated(iarvv)) allocate (iarvv(imax,jmax,kmax,nbak))
      if(.not.allocated(iarpl)) allocate (iarpl(imax,jmax,kmax,nbak))
      if(.not.allocated(iarzl)) allocate (iarzl(imax,jmax,kmax,nbak))

      return
      end

