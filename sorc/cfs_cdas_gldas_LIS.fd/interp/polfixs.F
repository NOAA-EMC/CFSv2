!BOP
! 
! !ROUTINE: polfixs.F90
!
!
! !DESCRIPTION: 
! This subroutine averages multiple pole scalar values
! on a latitude/longitude grid.  bitmaps may be averaged too.
!        
! !REVISION HISTORY:
!   04-10-96  Mark Iredell; Initial Specification
!
!   INPUT ARGUMENT LIST:
!     no       - integer number of grid points
!     nx       - integer leading dimension of fields
!     km       - integer number of fields
!     rlat     - real (no) latitudes in degrees
!     rlon     - real (no) longitudes in degrees
!     ib       - integer (km) bitmap flags
!     lo       - logical*1 (nx,km) bitmaps (if some ib(k)=1)
!     go       - real (nx,km) fields
!
!   OUTPUT ARGUMENT LIST:
!     lo       - logical*1 (nx,km) bitmaps (if some ib(k)=1)
!     go       - real (nx,km) fields
!
! !INTERFACE:
subroutine polfixs(nm,nx,km,rlat,rlon,ib,lo,go)
!EOP
  implicit none
  real, PARAMETER :: rlatnp=89.9995, rlatsp=-89.9995
  integer         :: n, nm, k, km, nx
  real            :: tsp, gnp, gsp, wsp, tnp, wnp
  real            :: rlat(nm),rlon(nm)
  integer         :: ib(km)
  real            :: go(nx,km)
  logical*1       :: lo(nx,km)

  
  do k=1,km
     wnp=0.0
     gnp=0.0
     tnp=0.0
     wsp=0.0
     gsp=0.0
     tsp=0.0
     !  average multiple pole values
     do n=1,nm
        if(rlat(n).ge.rlatnp) then
           wnp=wnp+1
           if(ib(k).eq.0.or.lo(n,k)) then
              gnp=gnp+go(n,k)
              tnp=tnp+1
           endif
        elseif(rlat(n).le.rlatsp) then
           wsp=wsp+1
           if(ib(k).eq.0.or.lo(n,k)) then
              gsp=gsp+go(n,k)
              tsp=tsp+1
           endif
        endif
     enddo
     !  distribute average values back to multiple poles
     if(wnp.gt.1) then
        if(tnp.ge.wnp/2) then
           gnp=gnp/tnp
        else
           gnp=0.
        endif
        do n=1,nm
           if(rlat(n).ge.rlatnp) then
              if(ib(k).ne.0) lo(n,k)=tnp.ge.wnp/2
              go(n,k)=gnp
           endif
        enddo
     endif
     if(wsp.gt.1) then
        if(tsp.ge.wsp/2) then
           gsp=gsp/tsp
        else
           gsp=0.
        endif
        do n=1,nm
           if(rlat(n).le.rlatsp) then
              if(ib(k).ne.0) lo(n,k)=tsp.ge.wsp/2
              go(n,k)=gsp
           endif
        enddo
     endif
  enddo
end subroutine polfixs
