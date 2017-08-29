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
!  !ROUTINE: agrlwdn.F90
!  
! 
!  !DESCRIPTION: 
!
!     to compute the net downward longwave radiation at the
!     earth's surface.
!
!     method:  \\
!     ====== \\
!     - calculate the emissivity of the clear sky.\\
!     - calculate downwelling longwave radiation of the clear sky.\\
!     - add contribution of low, middle and high clouds to the\\
!       clear sky portion.
!
!     process narrative:  flux3 - located in the flux3 sdf in dnxm\\
!     =================\\
!      
!     references: \\
!     ==========
!     dr idso's paper in the j. of geophys. research,   \\
!     no 74, pp 5397-5403.  \\
!  
!     dr r.f.wachtmann's paper in the digest of preprints,  
!     topical meeting on remote sensing of the atmosphere,  
!     anaheim,ca, optical society of america, entitled, 
!     "expansion of atmospheric temperature-moisture
!     profiles in empirical orthogonal functions for remote 
!     sensing applications", 1975   
!  
! !INTERFACE:
      subroutine agrlwdn( sfctmp, e, iclamt, rldown )

! !REVISION HISTORY:
!     15 may 1988  initial version........................capt rice/sddc  
!     07 sep 1999  ported to ibm sp-2.  added intent attributes to
!                  arguments.  updated prolog..............mr gayno/dnxm
!     25 oct 2001  implement in LDAS.....................jesse meng/ncep

      implicit none
! !INPUT PARAMETERS:
      real,     intent(in)         :: iclamt  ( 3 ) 
      real,     intent(in)         :: e
      real,     intent(in)         :: sfctmp
! !OUTPUT PARAMETERS:   
      real,     intent(out)        :: rldown   
!EOP
      real                         :: cldfrt  ( 3 )
      real                         :: clrsky   

      real                         :: emb  
      real                         :: emissa   
      real                         :: emissb   
      real                         :: hcterm   
      real                         :: lcterm   
      real                         :: mcterm   
      real,     parameter          :: sigma = 5.67e-08
      real,     parameter          :: zh    = 8.0
      real,     parameter          :: zl    = 1.3
      real,     parameter          :: zm    = 3.1  
!BOC
!     ------------------------------------------------------------------
!     executable code starts here...compute the cloud amount 
!     in fraction of overcast (.1 to 1.0). 
!     ------------------------------------------------------------------

      cldfrt(1) =  iclamt(1)  / 100.0
      cldfrt(2) =  iclamt(2)  / 100.0
      cldfrt(3) =  iclamt(3)  / 100.0

!     ------------------------------------------------------------------
!     convert vapor pressure units from pascals to millibars for use
!     in determining emissivities.  
!     ------------------------------------------------------------------

      emb = e * 0.01

!     ------------------------------------------------------------------
!     compute the effective clr sky emissivity for all wavelengths  
!     (emissa) using idso's equation.   
!     ------------------------------------------------------------------

      emissa = 0.700 + (5.95e-5 * emb * exp(1500 / sfctmp))

!     ------------------------------------------------------------------
!     use emissa in wachtmann's model for sky irradiance to calc a  
!     resultant longwave downward radiation value.  first calc a sasc   
!     emmisivity (emissb), which is an adjusted idso emmisivity.
!     then use emissb to calculate the blackbody irradiance of the  
!     clear sky (the 1st term of wachtmann's equation).
!     ------------------------------------------------------------------

      emissb = -0.792 + (3.161 * emissa) - (1.573 * emissa * emissa) 
      clrsky =  emissb * sigma * ( sfctmp * sfctmp * sfctmp * sfctmp )

!     ------------------------------------------------------------------
!     now compute the irradiance contribution from the low, middle, 
!     and hi cloud layers (the 2nd thru 4th terms in wachtmann' eqn).
!     ------------------------------------------------------------------

      lcterm = (80.0 - (5.0 * zl)) * cldfrt(1) 
      mcterm = (80.0 - (5.0 * zm)) * (1.0 - cldfrt(1)) * cldfrt(2)  
      hcterm = (80.0 - (5.0 * zh)) * (1.0 - cldfrt(1)) * &
     &          (1.0 - cldfrt(2)) * cldfrt(3)   

!     ------------------------------------------------------------------
!     put it all together to get a resultant downwrd longwave irrad.
!     ------------------------------------------------------------------

      rldown = clrsky + hcterm + mcterm + lcterm

      return
!EOC
    end subroutine agrlwdn
