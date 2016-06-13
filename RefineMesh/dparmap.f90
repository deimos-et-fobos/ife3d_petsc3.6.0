!--------------------------------------------------------------------------
 SUBROUTINE DPARMAP (X,Y,IE,JE,NOD,NEL_,JEE,LP,IIN,IS, iElmSon, jElmSon)
!--------------------------------------------------------------------------
!
! Los elementos que no se parten mantienen su numeración. Los que se parten, lo hacen en 2, 3, o 4 elemento hijos.
! Uno de ellos mantiene la numeración del padre. A los otros se le asigna un número mayor que el nel viejo.
! Por lo tanto, la información de los hijos que tuvo cada elemento, es un conjunto de nel listas de 1, 2, 3 o 4
! números (el primero igual a nel y los otros mayores que nel) cada una. 
!
! EN ESTA VERSION ALMACENAMOS EL NÚMERO DEL ELEMENTO QUE LO HEREDA DEL PADRE. LO HACEMOS PARA NO TENER QUE
! EXTRAERLO FUERA DE LOS LOOPS DE hpAdapt, Y PARA DISTINGUIR CONCEPTUALMENTE UN ELEMENTO QUE NO SE PARTIÓ 
! DE SU HEREDERO EN EN LA NUEVA MALLA, QUE CONSIDERAMOS COMO SU HIJO, AUNQUE MANTENIENDO SU NUMERACIÓN ORIGINAL.
!
! Esta información puede almacenarse como una matriz rala booleana ElmSon de nel_viejo x nel_nuevo, tal que
!  ElmSon(i, j) = 1 si el elemento j es hijo del elemento i
!  ElmSon(i, j) = 0 si el elemento j no es hijo del elemento i.
!  ElmSon(i, j) = 0 si i < j.
!  ElmSon(i, i) = 1
!  Almacenamos TODOS los elementos.
!   
!---------------------Agregado--------------------------------------
   implicit real(8) (a-h, o-z)
   !Archivo de escritura de Elm$Son. Se pasa como argumento a enredo.
   dimension iElmSon(NEL_ + 1), jElmSon(*)
!-----------------------End-----------------------------------------
   DIMENSION X(*),Y(*),IE(*),JE(*),JEE(*),LP(*),IIN(*)
!
!     Mapa de particiones generado, comienzan particiones
!
!---------------------Agregado--------------------------------------
   iESi = 1 !Puntero a jElmSon
   iElmSon(1) = 1
   NEL = NEL_
!-----------------------End-----------------------------------------
   IJE = -2
!---------------------------------------- Modificación----------------------------------------------
!   En la versión original LA venía por COMMON y apuntaba al comienzo del primer elento nuevo en JE
!   LA = LAS + 1
   LA = 3 * NEL + 1
!---------------------------------------------End---------------------------------------------------

   do i = 1, NEL_
!
!------- Incluimos el hijo que hereda la numeración del padre -----
      jElmSon(iESi) = i
      iESi = iESi + 1
!-----------------------End-----------------------------------------
!
!     Generacion de nodos
!
      IJE = IJE + 3
      if (iin (i) .ne. 0) then
         N1 = JE(IJE)
         N2 = JE(IJE+1)
         N3 = JE(IJE+2)
         N12 = LP(IJE)
         N23 = LP(IJE+1)
         N13 = LP(IJE+2)
   ! N12
         IF (ABS (N12) .EQ. 1) THEN
            NOD = NOD + 1
            N12 = NOD * N12
            X (NOD) = (X(N1) + X(N2)) * .5d0
            Y (NOD) = (Y(N1) + Y(N2)) * .5d0
            IV = JEE (3*I-2)
            IF (IV.NE.0) THEN
               IV3 = IV*3
               IF (JEE(IV3-2) .EQ. I) THEN
                  LP (IV3-2) = NOD * LP (IV3-2)
               ELSE IF (JEE(IV3-1) .EQ. I) THEN
                  LP (IV3-1) = NOD * LP (IV3-1)
               ELSE 
                  LP (IV3) = NOD * LP (IV3)
               END IF
            END IF
         END IF
   ! N23
         IF (ABS(N23) .EQ. 1) THEN
            NOD = NOD + 1
            N23 = NOD * N23
            X (NOD) = (X(N2) + X(N3)) * .5d0
            Y (NOD) = (Y(N2) + Y(N3)) * .5d0
            IV = JEE (3*I-1)
            IF (IV.NE.0) THEN
               IV3 = IV*3
               IF (JEE(IV3-2) .EQ. I) THEN
                  LP (IV3-2) = NOD * LP (IV3-2)
               ELSE IF (JEE(IV3-1) .EQ. I) THEN
                  LP (IV3-1) = NOD * LP (IV3-1)
               ELSE 
                  LP (IV3) = NOD * LP (IV3)
               END IF
            END IF
         END IF
   ! N13
         IF (ABS(N13) .EQ. 1) THEN
            NOD = NOD + 1
            N13 = NOD * N13
            X (NOD) = (X(N1) + X(N3)) * .5d0
            Y (NOD) = (Y(N1) + Y(N3)) * .5d0
            IV = JEE (3*I)
            IF (IV.NE.0) THEN
               IV3 = IV*3
               IF (JEE(IV3-2) .EQ. I) THEN
                  LP (IV3-2) = NOD * LP (IV3-2)
               ELSE IF (JEE(IV3-1) .EQ. I) THEN
                  LP (IV3-1) = NOD * LP (IV3-1)
               ELSE 
                  LP (IV3) = NOD * LP (IV3)
               END IF
            END IF
         END IF
   !
         IF (N12.EQ.0.OR.N23.EQ.0.OR.N13.EQ.0.OR.IS.NE.0) THEN
   !
   !     Reordenamiento p/q' N1-N2 sea la arista + larga
   !
            IF (N23.LT.0) THEN
               NAUX = N1
               N1 = N2
               N2 = N3
               N3 = NAUX
               NAUX = N12
               N12 = ABS(N23)
               N23 = N13
               N13 = NAUX
            END IF
            IF (N13.LT.0) THEN
               NAUX = N3
               N3 = N2
               N2 = N1
               N1 = NAUX
               NAUX = ABS(N13)
               N13 = N23
               N23 = N12
               N12 = NAUX
            END IF
            N12 = ABS(N12)
   !
   !     Lado N1-N2 partido Genera los nuevos elementos
   !
            IF (N13.EQ.0) THEN
               JE (IJE)   = N1
               JE (IJE+1) = N12
               JE (IJE+2) = N3
            ELSE
               JE (IJE)   = N1
               JE (IJE+1) = N12
               JE (IJE+2) = N13
               JE (LA)    = N12
               JE (LA+1)  = N3
               JE (LA+2)  = N13
               NEL = NEL + 1
               jElmSon(iESi) = nel
               iESi = iESi + 1
               LA = LA + 3
               IE(NEL+1) = LA
            ENDIF
   !
            IF (N23.EQ.0) THEN
               JE (LA)    = N12
               JE (LA+1)  = N2
               JE (LA+2)  = N3
               NEL = NEL + 1
               jElmSon(iESi) = nel
               iESi = iESi + 1
               LA = LA + 3
               IE(NEL+1) = LA
            ELSE
               JE (LA)    = N12
               JE (LA+1)  = N2
               JE (LA+2)  = N23
               NEL = NEL + 1
               jElmSon(iESi) = nel
               iESi = iESi + 1
               LA = LA + 3
               IE(NEL+1) = LA
               JE (LA)    = N12
               JE (LA+1)  = N23
               JE (LA+2)  = N3
               NEL = NEL + 1
               jElmSon(iESi) = nel
               iESi = iESi + 1
               LA = LA + 3
               IE(NEL+1) = LA
            ENDIF
         ELSE
   !
   !     Particion en triangulos semejantes
   !
            N12 = ABS(N12)
            N23 = ABS(N23)
            N13 = ABS(N13)
            JE (IJE)   = N12
            JE (IJE+1) = N23
            JE (IJE+2) = N13
            JE (LA)    = N12
            JE (LA+1)  = N2
            JE (LA+2)  = N23
            NEL = NEL + 1
            jElmSon(iESi) = nel
            iESi = iESi + 1
            LA = LA + 3
            IE(NEL+1) = LA
            JE (LA)    = N23
            JE (LA+1)  = N3
            JE (LA+2)  = N13
            NEL = NEL + 1
            jElmSon(iESi) = nel
            iESi = iESi + 1
            LA = LA + 3
            IE(NEL+1) = LA
            JE (LA)    = N13
            JE (LA+1)  = N1
            JE (LA+2)  = N12
            NEL = NEL + 1
            jElmSon(iESi) = nel
            iESi = iESi + 1
            LA = LA + 3
            IE(NEL+1) = LA
         END IF
      end if
      iElmSon(i + 1) = iESi
!
   end do

END

