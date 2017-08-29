!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: noah_soiltype.F90
!
! !DESCRIPTION:
!  This subroutine uses the percentages of sand and clay   
!  derived from the global soils dataset of Reynolds, 
!  Jackson, and Rawls [1999], to convert to Zobler
!  soil class values to be used in NOAH LSM v2.5 in LDAS.
!   (Original code by Matt Rodell, 3/7/01)
!
! !DESCRIPTION:
!  28 Apr 2002, K Arsenault:  Added NOAH LSM to LDAS
!
! !INTERFACE:
subroutine soiltype(nc,nr,sand,clay,soiltyp)
!EOP
  implicit none
  
  integer :: NC,NR
  integer :: SOILTYP(NC,NR)
  integer :: I,J
  
  real :: SA,CL
  real :: sand(nc,nr),clay(nc,nr)
!BOC  
  do j=1,nr
     do i=1,nc
        if (clay(i,j) .lt. 0.00) then
           soiltyp(i,j) = -99
        else
           cl = clay(i,j)
           sa = sand(i,j)
        endif
!-----------------------------------------------------------------
!     identify texture class.
!-----------------------------------------------------------------
        if (cl .lt. 0.23) then
           if (sa .lt. 0.50) then
              soiltyp(i,j) = 8          ! loam
           else
              if (sa .lt. 0.75) then
                 soiltyp(i,j) = 4        ! sandy loam
              else
                 soiltyp(i,j) = 1        ! loamy sand
              end if
           end if
        else 
           if (cl .lt. 0.28) then
              if (sa .lt. 0.45) then
                 soiltyp(i,j) = 8        ! loam
              else
                 soiltyp(i,j) = 7        ! sandy clay loam
              endif
           else
              if (cl .lt. 0.37) then
                 if (sa .lt. 0.2) then
                    soiltyp(i,j) = 2      ! silty clay loam
                 else
                    if (sa .lt. 0.43) then
                       soiltyp(i,j) = 6    ! clay loam
                    else
                       soiltyp(i,j) = 7    ! sandy clay loam
                    end if
                 end if
              else
                 if (cl .lt. 0.41) then
                    if (sa .lt. 0.2) then
                       soiltyp(i,j) = 2   ! silty clay loam
                    else
                       if (sa .lt. 0.43) then
                          soiltyp(i,j) = 6    ! clay loam
                       else
                          soiltyp(i,j) = 5    ! sandy clay
                       end if
                    end if
                 else
                   if (sa .lt. 0.43) then
                      soiltyp(i,j) = 3      ! light clay
                   else
                      soiltyp(i,j) = 5      ! sandy clay
                   end if 
                end if
             end if
          end if
       end if
    end do
 end do
!EOC 
end subroutine soiltype

