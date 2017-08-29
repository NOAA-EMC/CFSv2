!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
!  !MODULE: lsm_pluginMod.F90 
!   This module contains the definition of the functions used for
!   land surface model initialization, execution, reading and 
!   writing of restart files and other relevant land surface
!   model computations, corresponding to each of the LSMs used in LIS.
! 
!  !DESCRIPTION: 
!   
!  !REVISION HISTORY: 
!  09 Oct 03    Sujay Kumar  Initial Specification
! 
!EOP
      module lsm_pluginMod

      implicit none
      
      contains
!BOPI 
!
! !DESCRIPTION:
!
! This is a custom-defined plugin point for introducing a new LSM. 
! The interface mandates that the following routines be implemented
! and registered for each of the LSM that is included in LIS. 
!  \begin{description}
!  \item[Initialization]
!      Definition of LSM variables 
!      (to be registered using registerlsmini)
!  \item[Setup] 
!      Initialization of parameters
!      (to be registered using registerlsmsetup)
!  \item[DynamicSetup]
!      Routines to setup time dependent parameters
!      (to be registered using registerlsmdynsetup)
!  \item[Run]
!      Routines to execute LSM on a single gridcell for single timestep
!      (to be registered using registerlsmrun)
!  \item[Read restart]
!      Routines to read a restart file for an LSM run
!      (to be registered using registerlsmrestart)
!  \item[Output]
!      Routines to write output
!      (to be registered using registerlsmoutput)
!  \item[Forcing transfer to model tiles]
!      Routines to transfer an array of given forcing to model tiles
!      (to be registered using registerlsmf2t)
!  \item[Write restart]
!      Routines to write a restart file
!      (to be registered using registerlsmwrst)
! Multiple LSMs can be 
! included as well, each distinguished in the function table registry
! by the associated LSM index assigned in the card file. 
! 
! !INTERFACE:
       subroutine lsm_plugin
!EOPI
         use clm_varder, only : clm_varder_ini
         use atmdrvMod, only : atmdrv

         external driver
         external clm2_setup
         external clm2_restart
         external clm2_output
         external clm2wrst
         external clm2_dynsetup
         
         call registerlsmini(2,clm_varder_ini)
         call registerlsmsetup(2,clm2_setup)
         call registerlsmdynsetup(2,clm2_dynsetup)
         call registerlsmrun(2,driver)
         call registerlsmrestart(2,clm2_restart)
         call registerlsmoutput(2,clm2_output)
         call registerlsmf2t(2,atmdrv)
         call registerlsmwrst(2,clm2wrst)
         
       end subroutine lsm_plugin
     end module lsm_pluginMod
