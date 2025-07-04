C***********************************************************************
C***********************************************************************
      SUBROUTINE GBLEVN04 ! FORMERLY SUBROUTINE GETOE

      DIMENSION OEMIN(2:6)
      REAL(8)   OBS,QMS,BAK,SID,BMISS

      COMMON /GBEVAA/ SID,OBS(15,255),QMS(12,255),BAK(12,255),XOB,
     $ YOB,DHR,TYP,NLEV

      DATA BMISS /10E10/

      DATA OEMIN /0.5,0.1,1.0,0.5,1.0/

C  CLEAR THE EVENT ARRAY
C  ---------------------

      BAK = BMISS

C  LOOP OVER LEVELS LOOKING UP THE OBSERVATION ERROR
C  -------------------------------------------------

      IF(NLEV.GT.0)  THEN
      DO L=1,NLEV
         POB  = OBS( 1,L)
         QOB  = OBS( 2,L)
         TOB  = OBS( 3,L)
         WOB  = MAX(OBS(5,L),OBS(6,L))
         PWO  = OBS( 7,L)
         PW1O = OBS( 8,L)
         PW2O = OBS( 9,L)
         PW3O = OBS(10,L)
         PW4O = OBS(11,L)
         CAT  = OBS(12,L)

         IF(CAT .EQ.0    ) BAK( 1,L) = OEFG01(POB,TYP,5,OEMIN(5))
         IF(QOB .LT.BMISS) BAK( 2,L) = OEFG01(POB,TYP,3,OEMIN(3))
         IF(TOB .LT.BMISS) BAK( 3,L) = OEFG01(POB,TYP,2,OEMIN(2))
         IF(WOB .LT.BMISS) BAK( 5,L) = OEFG01(POB,TYP,4,OEMIN(4))
         IF(PWO .LT.BMISS) BAK( 6,L) = OEFG01(POB,TYP,6,OEMIN(6))
         IF(PW1O.LT.BMISS) BAK( 7,L) = OEFG01(POB,TYP,6,OEMIN(6))
         IF(PW2O.LT.BMISS) BAK( 8,L) = OEFG01(POB,TYP,6,OEMIN(6))
         IF(PW3O.LT.BMISS) BAK( 9,L) = OEFG01(POB,TYP,6,OEMIN(6))
         IF(PW4O.LT.BMISS) BAK(10,L) = OEFG01(POB,TYP,6,OEMIN(6))
      ENDDO
      ENDIF

      RETURN
      END
