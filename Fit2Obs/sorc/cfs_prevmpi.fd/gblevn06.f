C***********************************************************************
C***********************************************************************
C  SUBROUTINE GBLEVN06 - 2D LINEAR HORIZONTAL INTERPOLATION
C-----------------------------------------------------------------------
      SUBROUTINE GBLEVN06(XOB,YOB) ! FORMERLY SUBROUTINE HTERP

      USE GBLEVN_MODULE

      COMMON /GBEVEE/ PSI,ZSI,TI(500),UI(500),VI(500),QI(500),
     x     zint(500),pint(500),pintlog(500),plev(500),plevlog(500)

      DATA ROG   / 29.261 /


C  CALCULATE HORIZONTAL WEIGHTS AND INTERPOLATE
C  --------------------------------------------

      WX = XOB/DLON + 1.0
      I0 = WX
      I1 = MOD(I0,IMAX) + 1
      WX = WX-I0

      WY = (YOB+90.)/DLAT + 1.0
      J0 = WY
      J1 = MIN(J0+1,JMAX)
      WY = WY-J0

C  HTERP FOR SURFACE HEIGHT
C  ------------------------

      P1  = iar12z(I0,J0)
      P2  = iar12z(I0,J1)
      P3  = iar12z(I1,J0)
      P4  = iar12z(I1,J1)
      P5  = P1+(P2-P1)*WY
      P6  = P3+(P4-P3)*WY
      ZSI = P5+(P6-P5)*WX

C  HTERP FOR SURFACE PRESSURE
C  --------------------------

      P1  = iar13p(I0,J0)
      P2  = iar13p(I0,J1)
      P3  = iar13p(I1,J0)
      P4  = iar13p(I1,J1)
      P5  = P1+(P2-P1)*WY
      P6  = P3+(P4-P3)*WY
      PSI = P5+(P6-P5)*WX

C  HTERP FOR UPA T,U,V,Q
C  ---------------------

      DO K=1,KMAX

         P1 = iar14t(I0,J0,K)
         P2 = iar14t(I0,J1,K)
         P3 = iar14t(I1,J0,K)
         P4 = iar14t(I1,J1,K)
         P5 = P1+(P2-P1)*WY
         P6 = P3+(P4-P3)*WY
         TI(K) = P5+(P6-P5)*WX

         P1 = iar15u(I0,J0,K)
         P2 = iar15u(I0,J1,K)
         P3 = iar15u(I1,J0,K)
         P4 = iar15u(I1,J1,K)
         P5 = P1+(P2-P1)*WY
         P6 = P3+(P4-P3)*WY
         UI(K) = P5+(P6-P5)*WX

         P1 = iar16v(I0,J0,K)
         P2 = iar16v(I0,J1,K)
         P3 = iar16v(I1,J0,K)
         P4 = iar16v(I1,J1,K)
         P5 = P1+(P2-P1)*WY
         P6 = P3+(P4-P3)*WY
         VI(K) = P5+(P6-P5)*WX

         P1 = iar17q(I0,J0,K)
         P2 = iar17q(I0,J1,K)
         P3 = iar17q(I1,J0,K)
         P4 = iar17q(I1,J1,K)
         P5 = P1+(P2-P1)*WY
         P6 = P3+(P4-P3)*WY
         QI(K) = P5+(P6-P5)*WX

C  Layer Pressure
C  --------------

         P1 = iarpsl(I0,J0,K)
         P2 = iarpsl(I0,J1,K)
         P3 = iarpsl(I1,J0,K)
         P4 = iarpsl(I1,J1,K)
         P5 = P1+(P2-P1)*WY
         P6 = P3+(P4-P3)*WY
         PLEV(K) = P5+(P6-P5)*WX

C  Interface Pressure
C  ------------------

         P1 = iarpsi(I0,J0,K+1)
         P2 = iarpsi(I0,J1,K+1)
         P3 = iarpsi(I1,J0,K+1)
         P4 = iarpsi(I1,J1,K+1)
         P5 = P1+(P2-P1)*WY
         P6 = P3+(P4-P3)*WY
         PINT(K+1) = P5+(P6-P5)*WX

      ENDDO

C  Compute interface heights
C  -------------------------

      zint(1) = zsi
      pint(1) = psi
      pintlog(1) = log(pint(1))
      do k=2,kmax
         k0 = k-1
         zint(k) = zint(k0) - rog*ti(k0)*log(pint(k)/pint(k0))
         pintlog(k) = log(pint(k))
      enddo
      pint(kmax+1) = 0.0

C  Compute log(pressure) at layer midpoints
C  ----------------------------------------

      do k=1,kmax
         plevlog(k) = log(plev(k))
      enddo

ccccc print *,' pint=',pint(1:kmax)
ccccc print *,' zint=',zint(1:kmax)

      RETURN
      END
