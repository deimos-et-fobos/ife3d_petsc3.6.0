C     -----------------------------------------------------------------
      SUBROUTINE DGMAP34 (X,Y,IE,JE,JEE,LP,IIN,NOD,NEL,NNO)
C     -----------------------------------------------------------------
C
	implicit real(8) (a-h, o-z)
      LOGICAL SWS
      DIMENSION X(*),Y(*),IE(*),JE(*),JEE(*),LP(*),IIN(*)
C
      DO 10 I = 1, 3*NEL
         LP (I) = 0
   10 CONTINUE
C
      NNO = NOD
      IU  = 1
   20 SWS = .TRUE.
      DO 30 I = 1,NEL
         IF (IIN (I) .NE. IU) GOTO 30
C
C      Busqueda de arista mas larga
C
         N1 = JE(IE(I))
         N2 = JE(IE(I)+1)
         N3 = JE(IE(I)+2)
         AL2M = (X(N1)-X(N2))**2 + (Y(N1)-Y(N2))**2
         K = -2
         AL2  = (X(N2)-X(N3))**2 + (Y(N2)-Y(N3))**2
         IF (AL2 .GT. AL2M) THEN
            K = -1
            AL2M = AL2
         END IF
         AL2  = (X(N1)-X(N3))**2 + (Y(N1)-Y(N3))**2
         IF (AL2 .GT. AL2M) THEN
            K = 0
         END IF
C
C      Marca arista a dividir en LP
C
         I3 = 3*I
         IF (IU.EQ.1) THEN
            IF (LP(I3-2).EQ.0) NNO = NNO + 1
            IF (LP(I3-1).EQ.0) NNO = NNO + 1
            IF (LP(I3  ).EQ.0) NNO = NNO + 1
            LP (I3-2) = 1
            LP (I3-1) = 1
            LP (I3  ) = 1
         END IF
         IF (LP(I3+K).EQ.0) NNO = NNO + 1
         LP (I3+K) = -1

         IF (IU.EQ.1) THEN
            DO 25 K = -2,0
               IV = JEE (I3+K)
               IF (IV.EQ.0) GO TO 25
               IV3 = 3*IV
               IF (JEE(IV3-2) .EQ. I) THEN
                  IF(LP(IV3-2).EQ.0) LP(IV3-2)  = 1
               ELSE IF (JEE(IV3-1) .EQ. I) THEN
                  IF(LP(IV3-1).EQ.0) LP(IV3-1)  = 1
               ELSE 
                  IF(LP(IV3).EQ.0) LP(IV3)  = 1
               END IF
C
C            Marca los elementos vecinos para dividirlos
C
               IF (IIN (IV) .EQ. 0) THEN
                  IIN (IV) = IU + 1
                  SWS = .FALSE.
               END IF
   25       CONTINUE
         ELSE
            IV = JEE (I3+K)
            IF (IV.EQ.0) GO TO 30
            IV3 = 3*IV
            IF (JEE(IV3-2) .EQ. I) THEN
               IF(LP(IV3-2).EQ.0) LP(IV3-2)  = 1
            ELSE IF (JEE(IV3-1) .EQ. I) THEN
               IF(LP(IV3-1).EQ.0) LP(IV3-1)  = 1
            ELSE 
               IF(LP(IV3).EQ.0) LP(IV3)  = 1
            END IF
C
C            Marca el elemento vecino para dividirlo
C
            IF (IIN (IV) .EQ. 0) THEN
               IIN (IV) = IU + 1
               SWS = .FALSE.
            END IF
         END IF
   30 CONTINUE
      IU = IU + 1
      IF (.NOT. SWS) GO TO 20
C
      RETURN
      END
