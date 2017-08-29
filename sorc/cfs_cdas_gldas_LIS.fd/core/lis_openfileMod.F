!-------------------------------------------------------------------------
! NASA Goddard Space Flight Center Land Information System (LIS) V3.0
! Released May 2004
!
! See SOFTWARE DISTRIBUTION POLICY for software distribution policies
!
! The LIS source code and documentation are in the public domain,
! available without fee for educational, research, non-commercial and
! commercial purposes.  Users may distribute the binary or source
! code to third parties provided this statement appears on all copies and
! that no charge is made for such copies.
!
! NASA GSFC MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE
! SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED AS IS WITHOUT EXPRESS OR
! IMPLIED WARRANTY.  NEITHER NASA GSFC NOR THE US GOVERNMENT SHALL BE
! LIABLE FOR ANY DAMAGES SUFFERED BY THE USER OF THIS SOFTWARE.
!
! See COPYRIGHT.TXT for copyright details.
!
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: lis_openfileMod.F90
! 
! !DESCRIPTION: 
!   This module contains interfaces and subroutines for opening data files.
!   
! !REVISION HISTORY: 
!  08Apr04    James Geiger Initial Specification
! 
!EOP
module lis_openfileMod
  use lisdrv_module, only : lis
  use lis_indices_module 

#if ( defined OPENDAP )
   logical, parameter :: use_opendap_server = .true.
#else
   logical, parameter :: use_opendap_server = .false.
#endif

contains
!BOP
! !ROUTINE: lis_set_filename 
! This routine overwrites the path for a GDS run
! 
! !INTERFACE:
subroutine lis_set_filename(file,time_offset)
#if ( defined OPENDAP )
   use opendap_module, only : opendap_data_prefix, ciam 
#endif
  character(len=*), intent(inout):: file
  character(len=*), optional :: time_offset
#if ( defined OPENDAP )
  if ( use_opendap_server ) then
     if ( PRESENT(time_offset) ) then
        file = trim(opendap_data_prefix)//'/'// &
             trim(adjustl(ciam))//'/'//"var_"//time_offset//".bin"
     else
        file = trim(opendap_data_prefix)//'/'// &
             trim(adjustl(ciam))//'/'//"var.bin"
     endif
  endif
#endif
end subroutine lis_set_filename
!BOP
! !ROUTINE: lis_open_file 
!
! !DESCRIPTION: 
! This routine is a generic open routine.  It parses its optional input
! arguments and builds an approriate open call.  It also determines
! whether or not data must be retrieve via a GraDS-DODS data server (GDS).
! If so, it calls the specified GDS script.
!
! !INTERFACE: 
subroutine lis_open_file(unit, file, form, status, access, recl, script, time_offset)

   implicit none

!INPUT PARAMETERS:
   integer,          intent(in) :: unit
   character(len=*), intent(in) :: file
   character(len=*), optional   :: form
   character(len=*), optional   :: status
   character(len=*), optional   :: access
   integer,          optional   :: recl
   character(len=*), optional   :: script
   character(len=*), optional   :: time_offset

!LOCAL VARIABLES:
   integer                      :: ios
   character(len=11)            :: form_use
   character(len=7)             :: status_use
   character(len=10)            :: access_use
   character(len=15)            :: script_use
   character(len=4)             :: cunit
!EOP
   ! If optional values are not assigned by caller, then set default values.
   if ( .not. PRESENT(form) ) then
      form_use ='unformatted'
   elseif ( trim(adjustl(form)) == 'unformatted' .or. &
            trim(adjustl(form)) == 'formatted'          ) then
      form_use = trim(adjustl(form))
   endif

   if ( .not. PRESENT(status) ) then
      status_use = 'old'
   elseif ( trim(adjustl(status)) == 'old'     .or. &
            trim(adjustl(status)) == 'new'     .or. &
            trim(adjustl(status)) == 'replace' .or. &
            trim(adjustl(status)) == 'unknown'        ) then
      status_use = trim(adjustl(status))
   endif
   if ( .not. PRESENT(access) ) then
!      if(lis%d%domain.eq.8) then 
      if(lis%d%gridDesc(9) .eq. 0.01) then 
         access_use = 'direct'
      else
         access_use = 'sequential'
      endif
   elseif ( trim(adjustl(access)) == 'sequential' .or. &
            trim(adjustl(access)) == 'direct'            ) then
      access_use = trim(adjustl(access))
   endif
!   if ( .not. PRESENT(recl) ) then
!      recl = 4
!   endif
   if ( .not. PRESENT(script) ) then
      script_use = 'none'
   else
      script_use = trim(adjustl(script))
   endif

   ! If script exists, retrieve data through GrADS-DODS server
   ! (if necessary)
   if ( use_opendap_server ) then
      if ( script_use /= 'none' ) then
         if(.not.PRESENT(time_offset)) then 
         call retrieve_data(file, script_use)
         else
            call retrieve_data(file, script_use, time_offset)
         endif
      endif
   endif
   ! Open the file
   call lis_log_msg('MSG: lis_open_file -- Opening '//trim(file))
   if ( access_use == 'sequential' ) then
      open(unit=unit, file=file, form=form_use, status=status_use, &
           access=access_use, IOSTAT=ios)
   else
      open(unit=unit, file=file, form=form_use, status=status_use, &
           access=access_use, recl=recl, IOSTAT=ios)
   endif

   ! Check the status of the open call
   write(cunit,'(i4)') unit
   if ( ios /= 0 ) then
      call lis_log_msg('ERR: lis_open_file -- Cannot open file '&
                       //trim(file)//' on unit '//adjustl(cunit))
      call endrun
   else
      call lis_log_msg('MSG: lis_open_file -- Successfully opened '&
                       //trim(file)//' on unit '//adjustl(cunit))
   endif
 
   return

end subroutine lis_open_file
!BOP
! !ROUTINE: lis_read_file 
!
! !DESCRIPTION: 
! This routine is a generic read routine.  It parses its optional input
! arguments and builds an approriate read call. 
!
! !INTERFACE: 
subroutine lis_read_file(unit, array)
  implicit none
  integer,          intent(in) :: unit
  real,          intent(inout) :: array(lis_nc_data, lis_nr_data)

  integer :: line1, line2, line
  integer :: c,r, glnc, glnr

!  if(lis%d%domain.eq.8) then 
!  if(lis%d%gridDesc(9) .eq. 0.01) then 
     line1 = nint((lis%d%gridDesc(4)-lis%d%gridDesc(44))/lis%d%gridDesc(9))+1
     line2 = nint((lis%d%gridDesc(5)-lis%d%gridDesc(45))/lis%d%gridDesc(10))+1
     do r=1,lis%d%lnr
        do c=1,lis%d%lnc
           glnc = line2+c-1
           glnr = line1+r-1
           line = (glnr-1)*nint(lis%d%gridDesc(42))+glnc
           read(unit,rec=line) array(c,r)
        enddo
     enddo
!  else
!     read(unit) array
!  endif
end subroutine lis_read_file

!BOP
! !ROUTINE: retrieve_data 
!
! !DESCRIPTION: 
! This routine retrieves data from a GDS.  It will make 3 attempts to
! retrieve data from the server.  If the data cannot be retrieved, this
! routine aborts by calling endrun.
!
! !INTERFACE: 
subroutine retrieve_data(file, script,time_offset)
!EOP

   character(len=*), intent(in) :: file
   character(len=*), intent(in) :: script
   character(len=*), optional :: time_offset
#if ( defined OPENDAP )

   logical :: exists
   integer :: try

   exists = .false.
   try = 1

   do
      if ( .not. exists .and. try < 4 ) then ! keep trying to retrieve file

         if(.not.PRESENT(time_offset)) then 
            call retrieve_script(file,script)
         else
            call retrieve_script(file, script, time_offset)
         endif

         inquire(FILE=file, EXIST=exists)
         try = try + 1
      else
         if ( .not. exists ) then ! error, could not retrieve the file
            call lis_log_msg('ERR: lis_open_file -- '// &
                             'Could not retrieve data file '//trim(file))
            call endrun
         else ! got it, break the do loop
            exit
         endif
      endif
   enddo

#endif

   return

end subroutine retrieve_data

!BOP
! !ROUTINE: retrieve_script 
!
! !DESCRIPTION: 
! This routine retrieves makes the system call that executes the
! GrADS script that retrieves data from a GDS.
!
! !INTERFACE: 
subroutine retrieve_script(file, script, time_offset)
!EOP
#if ( defined OPENDAP )
   use opendap_module, only : ciam, cdom, &
                              cparm_slat, cparm_nlat, cparm_wlon, cparm_elon
   use agrmetopendap_module, only : agrmet_time_index

   character(len=*), intent(in) :: file
   character(len=*), intent(in) :: script
      character(len=*), optional :: time_offset
   character(len=4) :: ctime_index

   call lis_log_msg('MSG: lis_open_file -- Retrieving data via '// &
                    trim(script)//' script')

   if ( trim(adjustl(script)) == "getagrmet_lw.pl" .or. &
        trim(adjustl(script)) == "getagrmet_sw.pl" ) then

      write(ctime_index, '(i4)') agrmet_time_index

      call system("opendap_scripts/"//trim(adjustl(script))//" "// &
                  ciam//" "//                                      &
                  trim(file)//" "//                                &
                  ctime_index)


   elseif(PRESENT(time_offset)) then 

      call system("opendap_scripts/"//trim(adjustl(script))//" "// &
                  ciam//" "//                                      &
                  trim(file)//" "//cdom//" "//time_offset//" "//  & 
                  cparm_slat//" "//cparm_nlat//" "//               &
                  cparm_wlon//" "//cparm_elon)
   else

      call system("opendap_scripts/"//trim(adjustl(script))//" "// &
                  ciam//" "//                                      &
                  trim(file)//" "//cdom//" "//                       &
                  cparm_slat//" "//cparm_nlat//" "//               &
                  cparm_wlon//" "//cparm_elon)
   endif
#endif
   return
end subroutine retrieve_script

end module lis_openfileMod
