************              PLACA              ************
******subrutina para calculo de la matriz elemental******
****** del segundo  miembro,corresp. al elemento k ******
************                                 ************

      subroutine matep2(det,Me,tauk,dens,thin)
      implicit double precision (a-h,m,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"

      dimension  tauk(2,3),aux(3,3),Mp1(3,3),Mpb(3,3),
     &           Mp(3,3),Mp1b(3,3,2)
      double precision det,dens,thin
      
      PetscScalar    Me(18,18)

*     la matriz elemental es simetrica

******calculo de los coeficientes distintos de cero*******

      do 10 i=1,18
         do 11 j=1,18
            Me(i,j)=0.d0
 11      continue
 10   continue

*     Matriz debida a los polinomios de base de P1 (matriz p_i*p_j)
      
      do 20 i=1,3
         do 21 j=1,3
            Mp1(i,j)=1.d0/288.d0
 21      continue
         Mp1(i,i)=2.d0/288.d0
 20   continue

*     Matriz debida a los polinomios burbuja (matriz ((tau_i*qi)*(tau_j*q_j))
      
      do 30 i=1,3
         do 31 j=1,3
            aux(i,j)=tauk(1,i)*tauk(1,j)+tauk(2,i)*tauk(2,j)
 31      continue
 30   continue
 
      do 32 i=1,3
         do 33 j=1,3
            Mpb(i,j)=1.d0
 33      continue
         Mpb(i,i)=2.d0
 32   continue
      do 34 i=1,3
         do 35 j=1,3
            Mpb(i,j)=Mpb(i,j)*aux(i,j)/270.d0
 35      continue
 34   continue

*     Matriz debida a los productos de polinomios P1 y burbuja (matriz ((tau_i*qi)*pj))

      do 40 i=1,3
         do 41 j=1,3
            if (j.eq.(1+mod(i,3))) then
               Mp(i,j)=1.d0/360.d0
            else
               Mp(i,j)=2.d0/360.d0
            endif
 41      continue
 40   continue

      do 42 i=1,3
         do 43 j=1,3
            Mp1b(j,i,1)=tauk(1,i)*Mp(j,i)
            Mp1b(j,i,2)=tauk(2,i)*Mp(j,i)
 43      continue
 42   continue

*     En la matriz de masa elemental aparecen rotaciones (2 componentes)
*     y desplazamientos transversales:

      do 50 i=1,3
         do 51 j=i,3
            Me(i,j)=Mp1(i,j)
            Me(i+3,j+3)=Mp1(i,j)
            Me(i+6,j+6)=Mpb(i,j)
            Me(i+9,j+9)=12.d0/(thin**2)*Mp1(i,j)
            Me(i+12,j+12)=12.d0/(thin**3)*Mp1(i,j)
            Me(i+15,j+15)=12.d0/(thin**3)*Mp1(i,j)
 51      continue
 50   continue
 
      do 52 i=1,3
         do 53 j=1,3
            Me(i,j+6)=Mp1b(i,j,1)
            Me(i+3,j+6)=Mp1b(i,j,2)
 53      continue
 52   continue

*     tengo en cuenta la densidad y el cambio de variables

      do 54 i=1,18
         do 55 j=i,18
            Me(i,j)=Me(i,j)*dens*det
 55      continue
 54   continue

*     copio la parte simetrica de la matriz

      do 60 i=1,18
         do 61 j=i,18
            Me(j,i)=Me(i,j)
 61      continue
 60   continue
      return
      end
