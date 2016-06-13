
C     ------------------------------------------------------------------
      INTEGER FUNCTION IFINDKEY (LU, STR)
C     ------------------------------------------------------------------
      LOGICAL REW
      CHARACTER *(*) STR
      CHARACTER *80 REC
      INTEGER STRLEN
C
      REW = .FALSE.
      STRLEN = LEN(STR)
C
C Reads Record
C
   10 READ (LU, '(A80)', END=20) REC
      IF (REC(1:1) .NE. '*') GOTO 10
      IF (REC(2:STRLEN+1) .EQ. STR) THEN
         IF(REC(STRLEN+2:STRLEN+2) .EQ. ' ') THEN
            IFINDKEY = 1
            RETURN
         END IF
      END IF
      IF (REC(2:4) .EQ. 'END') GO TO 20
      GOTO 10
C
C End of file or Keyword END found, try rewind and repeat search
C
   20 CONTINUE
      IF (REW) THEN
         REWIND (LU)
         GOTO 30
      ELSE
         REW =.TRUE.
         REWIND (LU)
         GOTO 10
      ENDIF
C
C End of file or END Keyword and already tried rewind
C
   30 IFINDKEY = 0
      RETURN
      END
