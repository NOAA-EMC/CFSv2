program idealized_bc_driver
  !
  !-----------------------------------------------------------------------
  !                   GNU General Public License                        
  ! This file is a part of MOM.                                                                 
  !                                                                      
  ! MOM is free software; you can redistribute it and/or modify it and  
  ! are expected to follow the terms of the GNU General Public License  
  ! as published by the Free Software Foundation; either version 2 of   
  ! the License, or (at your option) any later version.                 
  !                                                                      
  ! MOM is distributed in the hope that it will be useful, but WITHOUT    
  ! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  
  ! or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public    
  ! License for more details.                                           
  !                                                                      
  ! For the full text of the GNU General Public License,                
  ! write to: Free Software Foundation, Inc.,                           
  !           675 Mass Ave, Cambridge, MA 02139, USA.                   
  ! or see:   http://www.gnu.org/licenses/gpl.html                      
  !-----------------------------------------------------------------------
  !
  !
  !<CONTACT EMAIL= "Zhi.Liang@noaa.gov">Z. Liang </CONTACT>
  !<REVIEWER EMAIL="Stephen.Griffies@noaa.gov"> S.M. Griffies </REVIEWER>
  !
  !<OVERVIEW>
  ! Driver for ideal surface boundary conditions
  !</OVERVIEW>
  !
  !<DESCRIPTION>
  ! The program drives the idealized boundary condition generation routines. 
  ! </DESCRIPTION>

  use fms_mod,          only : fms_init, fms_end
  use idealized_bc_mod, only : idealized_bc_init, write_idealized_bc_data, idealized_bc_end
  use constants_mod,    only : constants_init
  use mpp_mod,          only : mpp_pe, mpp_root_pe

  implicit none

  integer :: error

  call fms_init
  call constants_init

  call idealized_bc_init
  if(mpp_pe() == mpp_root_pe()) call write_idealized_bc_data
  call idealized_bc_end

  call fms_end

end program idealized_bc_driver
