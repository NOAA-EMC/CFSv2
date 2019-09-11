       INTEGER FUNCTION nfill(C)
       CHARACTER*(*) C
       NFILL = LEN(C)
       DO J=1,NFILL
         IF(C(J:J).EQ.' ') THEN
           NFILL = J - 1
           RETURN
         ENDIF
       ENDDO
       RETURN
       END
