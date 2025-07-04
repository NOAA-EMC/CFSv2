C***********************************************************************
C***********************************************************************
      SUBROUTINE GBLEVN01(IUNITE) ! FORMERLY SUBROUTINE ETABLE

      COMMON /GBEVDD/ ERRS(300,33,6)

C  READ THE OBSERVATION ERROR TABLES
C  ---------------------------------

      REWIND IUNITE

      IREC = 0

   10 CONTINUE
      READ(IUNITE,'(1X,I3)',END=100) KX
      IREC = IREC + 1
      DO K=1,33
         READ(IUNITE,'(1X,6E12.5)') (ERRS(KX,K,M),M=1,6)
      ENDDO
      GO TO 10

  100 CONTINUE
      IF(IREC.LE.0) THEN
         PRINT *, '##GBLEVENTS/GBLEVN01 - OBS. ERROR TABLE EMPTY OR ',
     $    'DOES NOT EXIST - STOP 60'
         CALL ERREXIT(60)
      ENDIF

      RETURN

      END
