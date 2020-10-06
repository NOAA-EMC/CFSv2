C-----------------------------------------------------------------------
C  SUBROUTINE HTERPT - 3D LINEAR INTERPOLATION IN X,Y,T
C-----------------------------------------------------------------------
C  GRID IS EXPECTED to be ORIENTED EAST/WEST AND SOUTH/NORTH.
C-----------------------------------------------------------------------
      SUBROUTINE HTERPT(XOB,YOB,DHR,GRID,LEVS,TERP)

      USE GBLEVN_MODULE
 
      integer levs
      REAL    GRID(IMAX,JMAX,KMAX,NBAK),terp(levs)
      REAL    xob,yob,dhr,bak1,bak2
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CALCULATE interpolation parameters for x,y,t
C  --------------------------------------------

      WX = XOB/DLON + 1.0
      I0 = WX
      I1 = MOD(I0,IMAX) + 1
      WX = WX-I0


      WY=YOB;CALL GRDCRD(WY,1,SLATS,JMAX,1) 
      J0 = WY
      J1 = MIN(J0+1,JMAX)
      WY = WY-J0

      call tterp(dhr,ibk,twt)

C  loop over vertical levels apply space and time interpolation 
C  ------------------------------------------------------------

      DO L=1,LEVS
      DO I=IBK,IBK+1; N=MIN(I,NBAK)
      P1 = GRID(I0,J0,L,N)
      P2 = GRID(I0,J1,L,N)
      P3 = GRID(I1,J0,L,N)
      P4 = GRID(I1,J1,L,N)
      P5 = P1+(P2-P1)*WY
      P6 = P3+(P4-P3)*WY
      IF(I==IBK) BAK1 = P5+(P6-P5)*WX
      IF(I/=IBK) BAK2 = P5+(P6-P5)*WX
      ENDDO
      TERP(L)=BAK1+(BAK2-BAK1)*TWT 
      ENDDO 

      RETURN
      END
C-----------------------------------------------------------------------
C  SUBROUTINE HTERPS - 2D LINEAR INTERPOLATION IN X,Y,T
C-----------------------------------------------------------------------
C  GRID IS EXPECTED to be ORIENTED EAST/WEST AND SOUTH/NORTH.
C-----------------------------------------------------------------------
      SUBROUTINE HTERPS(XOB,YOB,DHR,GRID,TERP)

      USE GBLEVN_MODULE
 
      REAL      GRID(IMAX,JMAX,NBAK),terp
      REAL      xob,yob,dhr,bak1,bak2
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      !print*,'in hterps'

C  CALCULATE interpolation parameters for x,y,t
C  --------------------------------------------

      WX = XOB/DLON + 1.0
      I0 = WX
      I1 = MOD(I0,IMAX) + 1
      WX = WX-I0

      WY=YOB;CALL GRDCRD(WY,1,SLATS,JMAX,1) 
      J0 = WY
      J1 = MIN(J0+1,JMAX)
      WY = WY-J0

      call tterp(dhr,ibk,twt)

C  apply space and time interpolation to grid
C  ------------------------------------------

      DO I=IBK,IBK+1; N=MIN(I,NBAK)
      P1 = GRID(I0,J0,N)                 
      P2 = GRID(I0,J1,N) 
      P3 = GRID(I1,J0,N) 
      P4 = GRID(I1,J1,N) 
      P5 = P1+(P2-P1)*WY
      P6 = P3+(P4-P3)*WY
      IF(I==IBK) BAK1 = P5+(P6-P5)*WX
      IF(I/=IBK) BAK2 = P5+(P6-P5)*WX
      ENDDO

      TERP=BAK1+(BAK2-BAK1)*TWT

      RETURN
      END
C-----------------------------------------------------------------------
C  SUBROUTINE tterp - calculate time interpolation parameters
C-----------------------------------------------------------------------
      SUBROUTINE tterp(dhr,ibk,twt)                  

      USE GBLEVN_MODULE

      REAL dhr,twt,rhr                              
      integer ibk

C-----------------------------------------------------------------------
      rhr(i)=tspan*(i-(nbak+1)/2)
C-----------------------------------------------------------------------

C  find the location wrt time levels
C  ---------------------------------

      if(dhr<=rhr(1)) then
         ibk=1;twt=0
      elseif(dhr>=rhr(nbak)) then
         ibk=nbak;twt=0
      else
         do ibk=1,nbak-1
         if(dhr>=rhr(ibk).and.dhr<rhr(ibk+1)) then
            twt=(dhr-rhr(ibk))/tspan
            return
         endif
         enddo
         call bort('tterp should not get here')
      endif

      return
      end
