C-----------------------------------------------------------------------
C  SUBROUTINE HTERPTZ - 4D LINEAR INTERPOLATION IN X,Y,Z,T
C-----------------------------------------------------------------------
C  GRID IS EXPECTED to be ORIENTED EAST/WEST AND SOUTH/NORTH.
C-----------------------------------------------------------------------
      SUBROUTINE HTERPTZ(XOB,YOB,DHR,DZ,GRID,XTRP)

      USE GBLEVN_MODULE
 
      INTEGER  IBK
      REAL     GRID(IMAX,JMAX,KMAX,NBAK)
      REAL     XOB,YOB,DHR,DZ,BAK1,BAK2,ATRP,BTRP,XTRP
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CALCULATE interpolation parameters for x,y,z,t
C  ----------------------------------------------

      WX = XOB/DLON + 1.0
      I0 = WX
      I1 = MOD(I0,IMAX) + 1
      WX = WX-I0

      WY=YOB;CALL GRDCRD(WY,1,SLATS,JMAX,1)
      J0 = WY
      J1 = MIN(J0+1,JMAX)
      WY = WY-J0

      L1=DZ;L2=MIN(L1+1,KMAX);WZ=DZ-L1

      CALL TTERP(DHR,IBK,TWT)

c  interpolate from grid in x,y,t,z       
c  --------------------------------

      DO L=L1,L2 
      DO I=IBK,IBK+1; N=MIN(I,NBAK)
      P1 = GRID(I0,J0,L,N)
      P2 = GRID(I0,J1,L,N)
      P3 = GRID(I1,J0,L,N)
      P4 = GRID(I1,J1,L,N)
      P5 = P1+(P2-P1)*WY
      P6 = P3+(P4-P3)*WY
      IF(I==IBK) BAK1 = P5 + (P6-P5)*WX
      IF(I/=IBK) BAK2 = P5 + (P6-P5)*WX
      ENDDO    
      IF(L==L1) ATRP=BAK1+(BAK2-BAK1)*TWT
      IF(L/=L1) BTRP=BAK1+(BAK2-BAK1)*TWT
      ENDDO    

      XTRP = ATRP+(BTRP-ATRP)*WZ

      RETURN
      END
c------------------------------------------------------------------
c------------------------------------------------------------------
      function pilnlnpw(p,paray,kmax)

      dimension paray(kmax)

      do 10 la=1,kmax
      if(p>=paray(la)) goto 11
10    continue
11    if(la>kmax) then
         la = kmax
         lb = kmax
      else if(la==1) then
         la = 1
         lb = 1
      else
         lb = la-1
      endif
      pa = paray(la)
      pb = paray(lb)
      if(pa/=pb) then
         wk = log(p/pb) / log(pa/pb)
      else
         wk = 0.
      endif
      pilnlnpw = float(lb)+wk

      return
      end
c------------------------------------------------------------------
c------------------------------------------------------------------
      function zilnlnpw(z,zaray,kmax)

      dimension zaray(kmax)

      do 10 la=1,kmax
      if(z<=zaray(la)) goto 11
10    continue
11    if(la>kmax) then
         la = kmax
         lb = kmax
      else if(la==1) then
         la = 1
         lb = 1
      else
         lb = la-1
      endif
      za = zaray(la)
      zb = zaray(lb)
      if(za/=zb) then
         wk = (z-zb)/(za-zb)
      else
         wk = 0.
      endif
      zilnlnpw = float(lb)+wk                              

      return
      end
C-----------------------------------------------------------------------
C  SUBROUTINE HTERPZ - interpolate diagnostic pressure  linearly in z
C-----------------------------------------------------------------------
      subroutine hterpz(zob,zwt,zlev,ps,plev,pob) 

      use gblevn_module

      real plev(kmax),zlev(kmax),lnp

! Compute observation pressure (only used for diagnostics)
! Set indices of model levels below (k1) and above (k2) observation.


      if(zwt<=1.) then
         z1=0.0    ; p1=ps         
         z2=zlev(1); p2=plev(1)
      elseif (zwt>=kmax) then
         z1=zlev(kmax-1); p1=plev(kmax-1)
         z2=zlev(kmax);   p2=plev(kmax)
      else
         k=zwt  
         k1=min(max(1,k),kmax)
         k2=max(1,min(k+1,kmax))
         z1=zlev(k1); p1=plev(k1)
         z2=zlev(k2); p2=plev(k2)
      endif

      dz21     = z2-z1
      dlnp21   = log(p2/p1)
      dz       = zob-z1
      lnp      = log(p1)+(dlnp21/dz21)*dz
      pob      = exp(lnp)

      return
      end
