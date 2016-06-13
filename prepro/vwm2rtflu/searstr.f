C     ------------------------------------------------------------------
      SUBROUTINE SEARSTR(LU, STR)
C     ------------------------------------------------------------------
      LOGICAL REW
      CHARACTER *(*) STR
      CHARACTER *80 REC
      INTEGER STRLEN
      REW = .FALSE.
      STRLEN = LEN(STR)
   10 READ (LU, '(A80)', END=20) REC
      IF (REC(1:1) .NE. '*') GOTO 10
      IF (REC(2:STRLEN+1) .EQ. STR) RETURN
      IF (REC(2:4) .EQ. 'END') THEN
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
C     ------------------------------------------------------------------
      FUNCTION ISEARSTR(LU, STR)
C     ------------------------------------------------------------------
      LOGICAL REW
      CHARACTER *(*) STR
      CHARACTER *80 REC
      INTEGER STRLEN
      REW = .FALSE.
      STRLEN = LEN(STR)
      ISEARSTR = 1
   10 READ (LU, '(A80)', END=15) REC
      IF (REC(1:1) .NE. '*') GOTO 10
      IF (REC(2:STRLEN+1) .EQ. STR) RETURN
      IF (REC(2:4) .EQ. 'END') GOTO 15
      GOTO 10
C         Fin de archivo o *END
   15 IF (REW) THEN
         REWIND (LU)
         GOTO 20
      ELSE
         REW =.TRUE.
         REWIND (LU)
         GOTO 10
      END IF
C
   20 ISEARSTR = 0
      RETURN
      END
