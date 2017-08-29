subroutine llm(msg)

! !USES:
   use spmdMod, only : iam

   implicit none

!INPUT PARAMETERS:
   character(len=*), intent(in) :: msg

!LOCAL VARIABLES:
   character(len=8)  :: date
   character(len=10) :: time
   character(len=5)  :: zone
   integer, dimension(8) :: values
   character(len=24+len(msg)+8) :: full_msg
   character(len=3) :: ciam
!EOP

!BOC
   print*,'len of full_msg ',len(full_msg)
   call date_and_time(date,time,zone,values)

   write(ciam,'(i3)') iam
   full_msg = date(1:4)//'-'//date(5:6)//'-'//date(7:8)//'T'//  &
              time(1:2)//':'//time(3:4)//':'//time(5:10)//' '// &
              trim(msg)//' ( '//trim(adjustl(ciam))//' )'
   print*, trim(full_msg)
!EOC
end subroutine llm
