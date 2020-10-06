C***********************************************************************
C***********************************************************************
C  GBLEVN03 - INTERPOLATE MODEL DATA (FIRST GUESS OR ANALYSIS) TO OB
C             LOCATIONS
C-----------------------------------------------------------------------
      SUBROUTINE GBLEVN35(SUBSET) ! FORMERLY SUBROUTINE GETFC

      USE GBLEVN_MODULE
      USE READSF_MODULE

      COMMON /GBEVAA/ SID,OBS(15,255),QMS(12,255),BAK(12,255),XOB,
     $ YOB,DHR,TYP,NLEV
      COMMON /GBEVEE/PSG01,ZSG01,THE_REST(500,9)


      CHARACTER*8  SUBSET,CID
      real(4)      plev(kmax),zlev(kmax),prof(kmax)
      REAL(8)      SID,OBS,QMS,BAK
      equivalence  (sid,cid)
      logical      newxyt,windp,windz,geomz

      DATA FMISS / 10E10 /
      DATA TZERO / 273.15 /
      DATA BETAP / .0552  /
      DATA BETA  / .00650 /
      DATA ROG   / 29.261 /

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------


C  SETUP FOR INTERPOLATING TO THIS OBSERVATION
C  -------------------------------------------

      CALL HTERPS(xob,yob,dhr,iarps,psi);psg01=psi

      xdx=bmiss; ydx=bmiss; tdx=bmiss

      BAK = BMISS
      OBX = BMISS; OBL = .false.
      S10 = 1. ! default value

C  INTERPOLATE GUESS PROFILES TO OB PRESSURES
C  ------------------------------------------

      IF(NLEV.GT.0)  THEN

      DO 10 L=1,NLEV

         POB  = OBS( 1,L)
         QOB  = OBS( 2,L) 
         TOB  = OBS( 3,L) 
         ZOB  = OBS( 4,L)
         UOB  = OBS( 5,L)
         VOB  = OBS( 6,L)
         PWO  = OBS( 7,L)
         PW1O = OBS( 8,L)
         PW2O = OBS( 9,L)
         PW3O = OBS(10,L)
         PW4O = OBS(11,L)
         CAT  = OBS(12,L)
         TVO  = OBS(14,L)

         pfc=bmiss;qfc=bmiss;tfc=bmiss;zfc=bmiss;ufc=bmiss;vfc=bmiss

c  x,y,t may be stored as profile values, or not

         IF(POB.LE.0. .OR. POB.GE.BMISS) GOTO 10
         xdr=xyt(1,l); ydr=xyt(2,l); tdr=xyt(3,l)
         if(max(xdr,ydr,tdr)>=fmiss) then
            xdr=xob;ydr=yob;tdr=dhr
         endif

         newxyt=xdr/=xdx.or.ydr/=ydx.or.tdr/=tdx
         xdx=xdr; ydx=ydr; tdx=tdr

c  given x,y,t generate some interpolated coordinates needed for the process

         if(newxyt) then
            CALL HTERPT(xdr,ydr,tdr,iarpl,kmax,plev) ! interpolated mid layer pressures
            CALL HTERPS(xdr,ydr,tdr,iarps,ps)        ! interpolated surface pressure
            CALL HTERPS(xdr,ydr,tdr,iarzs,zs)        ! interpolated surface height
            CALL HTERPT(xdr,ydr,tdr,iartt,1,t1)      ! interpolated sigma 1 temperature

            P1 = PLEV(1)
            TM = T1+(PS-P1)*BETAP*.5          ! mid layer temp between sigma1 and surface
            Z1 = ZS-ROG*TM*LOG(P1/PS)         ! height of sigma 1 (lowest sigma level)
            TS = T1+(Z1-ZS)*BETA              ! surface temperature lapsed from sigma 1

            P10 = PS*EXP(-10./((TS-(5.*beta))*ROG))     ! 10 meter pressure
            if(rsfc) CALL HTERPSF(xdr,ydr,tdr,f10,s10)  ! 10 meter wind factor
         endif

C  SURFACE PRESSURE

         IF(CAT.EQ.0 .AND. ZOB.LT.FMISS) THEN 
            DZ = ZOB-ZS
            TO = TS-DZ*BETA
            IF(TOB<BMISS) THEN
               TM=.5*(TOB+TZERO+TS)
            ELSE
               TM=.5*(TO+TS)
               IF(DZ<0) TM=TM-.5*DZ*BETA
            ENDIF
            PFC = PS*EXP(-DZ/(TM*ROG))
         ENDIF

C  CALCULATE POB LOCATION IN SIGMA MID LAYER PRESSURE PROFILE

         pwt=pilnlnpw(pob,plev,kmax) ! pob location in profile

C  SPECIFIC HUMIDITY - (QFC NEEDED BY SYNDATA PROGRAM BUT ONLY FOR REPORT TYPE 111)

         IF(QOB.LT.BMISS.OR.TOB.LT.BMISS.OR.TYP.EQ.111) THEN
            CALL HTERPTZ(xdr,ydr,tdr,pwt,iarqq,qfc)
         ENDIF

C  TEMPERATURE - (TFC NEEDED BY CQCVAD AND SYNDATA PROGRAMS, LATTER ONLY FOR REPORT TYPE 111)

         IF(TOB.LT.BMISS.OR.SUBSET.EQ.'VADWND'.OR.TYP.EQ.111) THEN 
            CALL HTERPTZ(xdr,ydr,tdr,pwt,iartt,tfc); tfc=tfc-tzero
            if(pob>p1)tfc=tfc+(pob-p1)*betap ! lapse temp below sigma 1 if need be
            if(tob/=tvo.and.fits) then ! convert background tv >> ts for ts ob !
               call hterptz(xdr,ydr,tdr,pwt,iarqq,qft)
               p=pob; e=esph(p,qft*1.e-6); tfc=sent(p,e,tfc)
            endif
         ENDIF

C  U AND V COMPONENTS

         IF(max(uob,vob)<bmiss) then

         ! adjust some reported surface heights to the real world
         kx=typ; oelev=zob; selev=stnelev 
         if(kx >= 280.and.kx < 300 )then
            oelev=10+selev
            if (kx == 280) oelev=20+selev
            if (kx == 282) oelev=20+selev
            if (kx == 285 .or. kx == 289 .or. kx == 290) then
               oelev=selev
               selev=0.0  
            endif
         else if(kx >= 221.and.kx <= 229) then
            if(selev >= oelev) oelev=10+selev
         end if; zob=oelev

         ! decide whether to interpolate in height (geop or geom) or pressure
         windz = (typ>=221.and.typ<=229).or.(typ>=280.and.typ<300)
         geomz = (typ>=223.and.typ<=228).or.(typ>=280.and.typ<300)
         windz = windz .and. zob<bmiss
         windp = .not. windz; swt=1. 

         !if(cid=='94638')print*,cid,pob,zob,uob,vob

         IF(max(uob,vob)<bmiss .and. windp ) THEN ! interpolate in pressure
            CALL HTERPTZ(xdr,ydr,tdr,pwt,iaruu,ufc)
            CALL HTERPTZ(xdr,ydr,tdr,pwt,iarvv,vfc)
            if(pob>plev(1))then ! interpolate between sig01 and the surface if need be
               !print*,pob,ps,t1,p1,s10
               P10 = PS*EXP(-10./(T1*ROG))  ! 10 meter pressure
               po=min(pob,p10); swt=log(po/p1)/log(p10/p1)
               swt=1.+(s10-1.)*swt
            endif
         ELSEIF(max(uob,vob)<bmiss .and. windz ) THEN ! interpolate in height
            CALL HTERPT(xdr,ydr,tdr,iarzl,kmax,zlev)
            if(geomz)call geometric(xdr,ydr,kmax,zlev)

            ! Subtract off combination of surface station elevation and
            ! model elevation depending on how close to surface
            fact = 0.0   
            if(zob-selev > 10.)then
               if(zob-selev > 1000)then
                  fact = 1.0 
               else
                  fact=(zob-selev)*.001  
               end if
            end if
            zob=zob-(selev+fact*(zs-selev)) ! zob and zlev are now height above the surface

            zwt=zilnlnpw(zob,zlev,kmax)
            CALL HTERPTZ(xdr,ydr,tdr,zwt,iaruu,ufc)
            CALL HTERPTZ(xdr,ydr,tdr,zwt,iarvv,vfc)

            CALL HTERPZ(zob,zwt,zlev,ps,plev,pob) ! store any pressure adjustments
            obx(1,l)=pob;obx(2,l)=qms(1,l)        ! to write into the output file 
            obl=.true.

            if(zob<zlev(1)) then ! interpolate between sig01 and the surface if need be
               if (zob <= 10.) then
                  term = max(zob,0.)/10.   
                  swt=s10*term            
               else
                  term = (zlev(1)-zob)/(zlev(1)-10.)        
                  swt=1.+(s10-1.)*term 
               end if
            endif

         endif

         ufc=ufc*swt; vfc=vfc*swt ! apply sfc wind reduction factor

         endif

C  HEIGHT

         IF(ZOB.LT.BMISS) THEN
            IF(POB.GT.PLEV(1)) THEN
               TM  = T1 + (Z1-ZOB)*BETA*.5
               ZFC = Z1 - ROG*TM*LOG(POB/P1)
            ELSE
               CALL HTERPTZ(XDR,YDR,TDR,PWT,IARZL,ZFC)
               ZFC = ZFC+ZS ! add zs to height above surface
            ENDIF
         ENDIF

C  PRECIPITABLE WATER

         PWO  = BMISS
         PW1O = BMISS
         PW2O = BMISS
         PW3O = BMISS
         PW4O = BMISS

C  RELATIVE HUMIDITY

         RHO = BMISS

C  SCATTER THE PROPER FIRST GUESS/ANALYSIS VALUES
C  ----------------------------------------------

         BAK(1,L)  = PFC ! MB
         BAK(2,L)  = QFC ! G/MG    
         BAK(3,L)  = TFC ! C     
         BAK(4,L)  = ZFC ! M
         BAK(5,L)  = UFC ! M/S
         BAK(6,L)  = VFC ! M/S
         BAK(7,L)  = PWO
         BAK(8,L)  = PW1O
         BAK(9,L)  = PW2O
         BAK(10,L) = PW3O
         BAK(11,L) = PW4O
         BAK(12,L) = RHO

   10 ENDDO
      ENDIF

      !print*,'out of getfc'
      RETURN
      END
C-----------------------------------------------------------------------
!  GEOMETRIC - convert geopotential height profile to geometric height
!
!         Convert geopotential height at layer midpoints to geometric
!         height using equations (17, 20, 23) in MJ Mahoney's note
!         "A discussion of various measures of altitude" (2001).
!         Available on the web at
!         http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!         termg = equation 17
!         termr = equation 21
!         z(k)  = equation 23
!
! Parameters below from WGS-84 model software inside GPS receivers.
! real(r_kind),parameter::  semi_major_axis = 6378.1370e3_r_kind     (m)
! real(r_kind),parameter::  semi_minor_axis = 6356.7523142e3_r_kind  (m)
! real(r_kind),parameter::  grav_polar      = 9.8321849378_r_kind    (m/s2)
! real(r_kind),parameter::  grav_equator    = 9.7803253359_r_kind    (m/s2)
! real(r_kind),parameter::  earth_omega     = 7.292115e-5_r_kind     (rad/s)
! real(r_kind),parameter::  grav_constant   = 3.986004418e14_r_kind  (m3/s2)

! Derived geophysical constants:
! real(r_kind),parameter::  flattening = (semi_major_axis-semi_minor_axis)/semi_major_axis
! real(r_kind),parameter::  somigliana = (semi_minor_axis/semi_major_axis) * (grav_polar/grav_equator) - one
! real(r_kind),parameter::  grav_ratio = (earth_omega*earth_omega * semi_major_axis*semi_major_axis * semi_minor_axis) / grav_constant
!
C-----------------------------------------------------------------------
      subroutine geometric(x,y,nz,z)                          

      dimension z(nz)

      parameter (pi      = acos(-1.))
      parameter (grav    = 9.81)                                       
      parameter (deg2rad = pi/180.)
      parameter (smaxis  = 6378.1370e3)
      parameter (smixis  = 6356.7523142e3)
      parameter (grpol   = 9.8321849378)   
      parameter (grequ   = 9.7803253359)   
      parameter (eartho  = 7.292115e-5)    
      parameter (grcon   = 3.986004418e14) 
      parameter (flatn   = (smaxis-smixis)/smaxis)
      parameter (somig   = (smixis/smaxis)*(grpol/grequ)-1.)
!!!   parameter (somig   = (smaxis/smixis)*(grpol/grequ)-1.) !!! wrong
      parameter (grrat   = (eartho**2*smaxis**2*smixis)/grcon)
      parameter (eccen   = sqrt(smaxis**2-smixis**2)/smaxis)    

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      sin2  = sin(y*deg2rad)**2
      termg = grequ  * (1.+somig*sin2)/sqrt(1.-eccen*eccen*sin2)
      termr = smaxis / (1.+ flatn+grrat-2.*flatn*sin2)
      do k=1,nz   
      z(k) = termr*z(k)/((termg*termr/grav)-z(k))  
      end do

      return
      end
c------------------------------------------------------------------
c------------------------------------------------------------------
      function pilnlnp(p,paray,qaray,kmax)

      dimension paray(kmax),qaray(kmax)

      do 10 la=1,kmax
      if(p.ge.paray(la)) goto 11
10    continue
11    if(la.gt.kmax) then
         la = kmax
         lb = kmax-1
      else if(la.eq.1) then
         la = 1
         lb = 1
      else
         lb = la-1
      endif
      pa = paray(la)
      pb = paray(lb)
      if(pa.ne.pb) then
         wk = log(p/pb) / log(pa/pb)
      else
         wk = 0.
      endif
      pilnlnp = qaray(lb) + (qaray(la)-qaray(lb)) * wk

      return
      end
c------------------------------------------------------------------
c------------------------------------------------------------------
      function zilnlnp(z,zaray,qaray,kmax)

      dimension zaray(kmax),qaray(kmax)

      do 10 la=1,kmax
      if(z<zaray(la)) goto 11
10    continue
11    if(la.gt.kmax) then
         la = kmax
         lb = kmax-1
      else if(la.eq.1) then
         la = 1
         lb = 1
      else
         lb = la-1
      endif
      za = zaray(la)
      zb = zaray(lb)
      if(za.ne.zb) then
         wk = (z-zb)/(za-zb)
      else
         wk = 0.
      endif
      zilnlnp = qaray(lb) + (qaray(la)-qaray(lb)) * wk

      return
      end
