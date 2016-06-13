C     -----------------------------------------------------------------
      SUBROUTINE CONEL3 (JE,IET,JET,JEE,NEL)
C     -----------------------------------------------------------------
C
      DIMENSION JE(1), IET(1), JET(1), JEE(1)
C
      IJE = 1
      DO 90 I = 1,NEL
         N1 = JE(IJE)
         N2 = JE(IJE+1)
         N3 = JE(IJE+2)
         IEV1 = 0
         IEV3 = 0
C
         DO 10 J = IET(N1),IET(N1+1)-1
            IEV = JET(J)
            IF (IEV.NE.I) THEN
               IN = 3*IEV-2
               IF (JE(IN  ).EQ.N2) THEN
                  IEV1 = IEV
               ELSE IF (JE(IN).EQ.N3) THEN
                  IEV3 = IEV
               END IF
               IF (JE(IN+1).EQ.N2) THEN
                  IEV1 = IEV
               ELSE IF (JE(IN+1).EQ.N3) THEN
                  IEV3 = IEV
               END IF
               IF (JE(IN+2).EQ.N2) THEN
                  IEV1 = IEV
               ELSE IF (JE(IN+2).EQ.N3) THEN
                  IEV3 = IEV
               END IF
            END IF
   10    CONTINUE
C
         DO 40 J = IET(N2),IET(N2+1)-1
            IEV = JET(J)
            IF (IEV.NE.I) THEN
               IN = 3*IEV-2
               IF (JE(IN  ).EQ.N3) GO TO 50
               IF (JE(IN+1).EQ.N3) GO TO 50
               IF (JE(IN+2).EQ.N3) GO TO 50
            END IF
   40    CONTINUE
         JEE (IJE+1) = 0
         GO TO 60
   50    JEE(IJE+1) = IEV
C
   60    JEE(IJE)   = IEV1
         JEE(IJE+2) = IEV3
C
         IJE = IJE + 3
   90 CONTINUE
C
      RETURN
      END
