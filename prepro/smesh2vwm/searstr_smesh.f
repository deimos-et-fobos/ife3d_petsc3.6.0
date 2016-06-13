C     ------------------------------------------------------------------
      SUBROUTINE SEARSTR_SMESH(LU, STR)
C     ------------------------------------------------------------------
      LOGICAL REW
      CHARACTER *(*) STR
      CHARACTER *80 REC
      INTEGER STRLEN
      REW = .FALSE.
      STRLEN = LEN(STR)
   10 READ (LU, '(A80)', END=20) REC
      IF (REC(1:STRLEN) .EQ. STR) RETURN
      IF (REC(1:3) .EQ. 'End') THEN
         IF (REW) THEN
            GOTO 20
         ELSE
            REW =.TRUE.
            REWIND (LU)
            GOTO 10
         ENDIF
      END IF
      GOTO 10
   20 WRITE(*,*)STR
      STOP 'Can''t find Keyword' 
      END
