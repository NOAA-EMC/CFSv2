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
! !MODULE: lsm_pluginMod.F90  
! 
! !DESCRIPTION: 
!   This module contains the definition of the functions used for
!   land surface model initialization, execution, reading and 
!   writing of restart files and other relevant land surface
!   model computations, corresponding to each of the LSMs used in LIS.
!   
! !REVISION HISTORY: 
!  09 Oct 03    Sujay Kumar  Initial Specification
! 
! !INTERFACE:
module lsm_pluginMod
!EOP  
  implicit none
  
contains
!BOP
! !ROUTINE: lsm_plugin
!
! !DESCRIPTION:
!
! This is a custom-defined plugin point for introducing a new LSM. 
! The interface mandates that the following routines be implemented
! and registered for each of the LSM that is included in LIS. 
!
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
!  \end{description}
! Multiple LSMs can be 
! included as well, each distinguished in the function table registry
! by the associated LSM index assigned in the card file. 
! 
! !INTERFACE:
  subroutine lsm_plugin
! !USES:
!    use template_varder, only : template_varder_ini
    use noah_varder, only : noah_varder_ini
!    use clm_varder, only : clm_varder_ini
!    use vic_varder, only : vic_varder_ini
!    use atmdrvMod, only : atmdrv
!    use mos_varder, only : mos_varder_ini
!    use hyssib_varder, only : hyssib_varder_ini
!    use ssib_varder, only : ssib_varder_ini
!EOP
 
    external template_main
    external template_setup
    external templaterst
    external template_output
    external template_f2t
    external template_writerst
    external template_dynsetup

!    external mos_main
!    external mos_setup
!    external mosrst
!    external mos_output
!    external mos_f2t
!    external mos_writerst
!    external mos_dynsetup

!    external hyssib_main
!    external hyssib_setup
!    external hyssibrst
!    external hyssib_output
!    external hyssib_f2t
!    external hyssib_writerst
!    external hyssib_dynsetup

!    external ssib_main
!    external ssib_setup
!    external ssibrst
!    external ssib_output
!    external ssib_f2t
!    external ssib_writerst
!    external ssib_dynsetup

    external noah_main 
    external noah_setup
    external noahrst
    external noah_output
    external noah_f2t
    external noah_writerst
    external noah_dynsetup

!    external driver
!    external clm2_setup
!    external clm2_restart
!    external clm2_output
!    external clm2wrst
!    external clm2_dynsetup
         
!    external vic_main
!    external vic_setup
!    external vic_readrestart
!    external vic_output
!    external vic_atmdrv
!    external vic_writerestart
!    external vic_dynsetup

!BOC
!    call registerlsmini(0,template_varder_ini)
    call registerlsmini(1,noah_varder_ini)
!    call registerlsmini(2,clm_varder_ini)
!    call registerlsmini(3,vic_varder_ini)
!    call registerlsmini(4,mos_varder_ini)
!    call registerlsmini(5,hyssib_varder_ini)
!    call registerlsmini(6,ssib_varder_ini)
    
!    call registerlsmsetup(0,template_setup)
    call registerlsmsetup(1,noah_setup)
!    call registerlsmsetup(2,clm2_setup)
!    call registerlsmsetup(3,vic_setup)
!    call registerlsmsetup(4, mos_setup)
!    call registerlsmsetup(5, hyssib_setup)
!    call registerlsmsetup(6, ssib_setup)

!    call registerlsmdynsetup(0,template_dynsetup)
    call registerlsmdynsetup(1,noah_dynsetup)
!    call registerlsmdynsetup(2,clm2_dynsetup)
!    call registerlsmdynsetup(3,vic_dynsetup)
!    call registerlsmdynsetup(4, mos_dynsetup)
!    call registerlsmdynsetup(5,hyssib_dynsetup)
!    call registerlsmdynsetup(6,ssib_dynsetup)
    
!    call registerlsmrun(0,template_main)
    call registerlsmrun(1,noah_main)
!    call registerlsmrun(2,driver)
!    call registerlsmrun(3,vic_main)
!    call registerlsmrun(4, mos_main)
!    call registerlsmrun(5, hyssib_main)
!    call registerlsmrun(6, ssib_main)
    
!    call registerlsmrestart(0,templaterst)
    call registerlsmrestart(1,noahrst)
!    call registerlsmrestart(2,clm2_restart)
!    call registerlsmrestart(3,vic_readrestart)
!    call registerlsmrestart(4, mosrst)
!    call registerlsmrestart(5,hyssibrst)
!    call registerlsmrestart(6,ssibrst)
    
!    call registerlsmoutput(0,template_output)
    call registerlsmoutput(1,noah_output)
!    call registerlsmoutput(2,clm2_output)
!    call registerlsmoutput(3,vic_output)
!    call registerlsmoutput(4, mos_output)
!    call registerlsmoutput(5,hyssib_output)
!    call registerlsmoutput(6,ssib_output)
    
!    call registerlsmf2t(0,template_f2t)
    call registerlsmf2t(1,noah_f2t)
!    call registerlsmf2t(2,atmdrv)
!    call registerlsmf2t(3,vic_atmdrv)
!    call registerlsmf2t(4, mos_f2t)
!    call registerlsmf2t(5,hyssib_f2t)
!    call registerlsmf2t(6,ssib_f2t)
        
!    call registerlsmwrst(0,template_writerst)
    call registerlsmwrst(1,noah_writerst)
!    call registerlsmwrst(2,clm2wrst)
!    call registerlsmwrst(3,vic_writerestart)
!    call registerlsmwrst(4, mos_writerst)
!    call registerlsmwrst(5,hyssib_writerst)
!    call registerlsmwrst(6,ssib_writerst)
!EOC         
  end subroutine lsm_plugin
end module lsm_pluginMod
