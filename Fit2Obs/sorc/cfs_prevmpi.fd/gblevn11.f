C***********************************************************************
C***********************************************************************
      subroutine gblevn11(imax,jmax,grid) ! formerly subroutine n_s_swap
      implicit none
      integer imax, jmax
      real grid(imax,jmax)
      real temp (imax)
      integer i, j, jj

      do j=1,jmax/2
        jj = jmax-j+1
        do i=1,imax
          temp(i)    = grid(i,j)
          grid(i,j)  = grid(i,jj)
          grid(i,jj) = temp(i)
        enddo
      enddo
      return
      end
