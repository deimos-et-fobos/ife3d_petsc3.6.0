C     ------------------------------------------------------------------
      SUBROUTINE BORDEG (NOD,NEL,IE,JE,IA,IB)
C     ------------------------------------------------------------------
C    ******************************************************************
C    * ELIMINAMOS LA VARIABLE NO UTILIZDAD MAT DE LA VERSION ORIGINAL *
C    ******************************************************************
C
C     Programador = VENERE Marcelo Javier
C     Fecha       = Marzo 1988
C     Funcion     = Busca los lados del borde de una red con cualquier
C                   tipo de elemento
C     Modificada 22/3/95 x E.A.Dari, evita calculo de cuadrados
C             (posible overflow)
C
C     -----------------------------------------------------------------
C
      DIMENSION IE(*),JE(*),IA(*),IB(*)
C
      DO 10 I = 1,NOD
         IA(I) = 0
         IB(I) = 0
   10 CONTINUE
C
      DO 20 I = 1,NEL
         IN   = IE(I)
         LA   = IE(I+1)-1
         IF (LA-IN.NE.8) THEN
            NO = JE(LA)
            DO 15 J = IN,LA
               N1 = NO
               NO = JE(J)
               IF(J.EQ.LA) THEN
                  N2 = JE(IN)
               ELSE
                  N2 = JE(J+1)
               END IF
               IA(NO) = IA(NO) + N2 - N1
   15       CONTINUE
         ELSE
            N1 = JE(IN)
            N2 = JE(IN+4)
            N3 = JE(IN+1)
            N4 = JE(IN+5)
            N5 = JE(IN+2)
            N6 = JE(IN+6)
            N7 = JE(IN+3)
            N8 = JE(IN+7)
            IA(N1) = IA(N1) + N2 - N8
            IA(N2) = IA(N2) + N3 - N1
            IA(N3) = IA(N3) + N4 - N2
            IA(N4) = IA(N4) + N5 - N3
            IA(N5) = IA(N5) + N6 - N4
            IA(N6) = IA(N6) + N7 - N5
            IA(N7) = IA(N7) + N8 - N6
            IA(N8) = IA(N8) + N1 - N7
         END IF
   20 CONTINUE
C   
      ICHANGE = 1
      DO WHILE (ICHANGE .NE. 0)
       ICHANGE = 0
       DO 30 I = 1,NEL
         IN   = IE(I)
         LA   = IE(I+1)-1
         IF (LA-IN.NE.8) THEN
            NO = JE(LA)
            DO 25 J = IN,LA
               N1 = NO
               NO = JE(J)
               IF(J.EQ.LA) THEN
                  N2 = JE(IN)
               ELSE
                  N2 = JE(J+1)
               END IF
               IF (IA(NO) .NE. 0) THEN
                  IF (IB(NO).EQ.0 .OR. IB(NO).EQ.N2) THEN
                     ICHANGE = 1
                     IB(NO) = N1
                  END IF
               END IF
   25       CONTINUE
         ELSE
            N1 = JE(IN)
            N2 = JE(IN+4)
            N3 = JE(IN+1)
            N4 = JE(IN+5)
            N5 = JE(IN+2)
            N6 = JE(IN+6)
            N7 = JE(IN+3)
            N8 = JE(IN+7)
            IF (IA(N1) .NE. 0) THEN
               IF (IB(N1).EQ.0 .OR. IB(N1).EQ.N8) THEN
                  ICHANGE = 1
                  IB(N1) = N2
               END IF
            END IF
            IF (IA(N2) .NE. 0) THEN
               IF (IB(N2).EQ.0 .OR. IB(N2).EQ.N1) THEN
                  ICHANGE = 1
                  IB(N2) = N3
               END IF
            END IF
            IF (IA(N3) .NE. 0) THEN
               IF (IB(N3).EQ.0 .OR. IB(N3).EQ.N2) THEN
                  ICHANGE = 1
                  IB(N3) = N4
               END IF
            END IF
            IF (IA(N4) .NE. 0) THEN
               IF (IB(N4).EQ.0 .OR. IB(N4).EQ.N3) THEN
                  ICHANGE = 1
                  IB(N4) = N5
               END IF
            END IF
            IF (IA(N5) .NE. 0) THEN
               IF (IB(N5).EQ.0 .OR. IB(N5).EQ.N4) THEN
                  ICHANGE = 1
                  IB(N5) = N6
               END IF
            END IF
            IF (IA(N6) .NE. 0) THEN
               IF (IB(N6).EQ.0 .OR. IB(N6).EQ.N5) THEN
                  ICHANGE = 1
                  IB(N6) = N7
               END IF
            END IF
            IF (IA(N7) .NE. 0) THEN
               IF (IB(N7).EQ.0 .OR. IB(N7).EQ.N6) THEN
                  ICHANGE = 1
                  IB(N7) = N8
               END IF
            END IF
            IF (IA(N8) .NE. 0) THEN
               IF (IB(N8).EQ.0 .OR. IB(N8).EQ.N7) THEN
                  ICHANGE = 1
                  IB(N8) = N1
               END IF
            END IF
         END IF
   30  CONTINUE    
      END DO
C
      DO 40 I2 = 1,NOD
         I1 = IB(I2)
         IF (I1.GE.1 .AND. I1.LE.NOD) THEN
            IA(I1) = I2
         ELSE
            IB(I2) = 0
         END IF
   40 CONTINUE
      RETURN
      END
