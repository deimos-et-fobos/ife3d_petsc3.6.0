******                 PLACA                 ******
****** calcula los coeficientes de la matriz ******
******          elemental de rigidez         ******


      subroutine matep1(KRe,CA,tauk,med,det,Kappa,facR,facA,facInPlane,
     &                              ver,thin,DN,DQ,pgauss,DkInv)
     
      implicit double precision (a-h,o-z)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"

      dimension CA(4,4),DN(2,3),DQ(2,3,3),pgauss(3,2),xg(2),
     &      A1(2,2,3),A2(2,2,3),A3(2,2,3),tauk(2,3),GB(4,9,3),GDk(4,4),
     &      sig(3,3),ver(2,3),CP(2,3,2),CPL(2,3),RG(2,12),DkInv(2,2)
      double precision Kappa,facR,facA,facInPlane,med(3),KA(9,9),
     &                 KIP(6,6),KR(12,12),L(2,3),det,thin
      
      PetscScalar    KRe(18,18)
      
      do 10 i=1,9
         do 11 j=1,9
            KA(i,j)=0.d0
 11      continue     
 10   continue
 
      do 12 i=1,12
         do 13 j=1,12
            KR(i,j)=0.d0
 13      continue     
 12   continue
 
      do 20 i=1,18
         do 21 j=1,18
            KRe(i,j)=0.d0
 21      continue     
 20   continue
 
      do 14 i=1,2
         do 15 j=1,2
               GDk(i,j)=DkInv(i,j)
               GDk(i+2,j+2)=DkInv(i,j)
               GDk(i+2,j)=0
               GDk(i,j+2)=0
 15      continue            
 14   continue

************************************************************************

*     Evaluacion dos polinomios burbulla nos puntos de Gauss

      do 23 i=1,2
         do 24 j=1,2
            do 25 k=1,3
               A1(i,j,k)=tauk(i,1)*DQ(j,1,k)
               A2(i,j,k)=tauk(i,2)*DQ(j,2,k)
               A3(i,j,k)=tauk(i,3)*DQ(j,3,k)
 25         continue
 24      continue
 23   continue

*     Calculo de la formula de interpolacion de 3 puntos

      do 30 i=1,4
         do 31 j=1,9
            do 32 k=1,3
               GB(i,j,k)=0.d0
 32         continue
 31      continue
 30   continue
      
      do 33 i=1,2
         do 34 j=1,3
            do 35 k=1,3
               GB(i,j,k)=DN(i,j)
               GB(i+2,j+3,k)=DN(i,j)
 35         continue
 34      continue
 33   continue
      
      do 40 i=1,2
         do 41 k=1,3
            GB(i,7,k)=A1(1,i,k)
            GB(i,8,k)=A2(1,i,k)
            GB(i,9,k)=A3(1,i,k)
            GB(i+2,7,k)=A1(2,i,k)
            GB(i+2,8,k)=A2(2,i,k)
            GB(i+2,9,k)=A3(2,i,k)
 41      continue
 40   continue
 
      do 42 i=1,9
         do 43 kk=1,9
            do 44 j=1,4
               do 45 k=1,4
                  do 46 ii=1,4
                     do 50 jj=1,4
                        do 51 ll=1,3
                           KA(i,kk)=KA(i,kk)+ GB(j,i,ll)*GDk(j,k)*
     &                              CA(k,ii)*GDk(jj,ii)*GB(jj,kk,ll)
 51                     continue
 50                  continue
 46               continue
 45            continue
 44         continue
 43      continue
 42   continue

      !Cambio desde la integral en el elemento de referencia al real 
      do 55 i=1,9
         do 56 j=1,9
            KA(i,j)=KA(i,j)*det/6.d0      
 56      continue 
 55   continue

      do 53 i=1,6
         do 54 j=1,6
            KIP(i,j)=KA(i,j)
 54      continue 
 53   continue

************************************************************************

*     Calculo las componentes de cada lado del triangulo (matriz L)
      
      sig(1,1)= 1; sig(1,2)= 1; sig(1,3)= 0
      sig(2,1)=-1; sig(2,2)= 0; sig(2,3)= 1
      sig(3,1)= 0; sig(3,2)=-1; sig(3,3)=-1

      do 100 i=1,2
         do 101 j=1,3
            L(i,j)=0.d0
            do 102 k=1,3
               L(i,j)=L(i,j)+ver(i,k)*sig(k,j)
 102        continue
 101     continue
 100  continue

*     Tendo en cuenta la definicion local del operador de reducion R calculando
*     los coeficientes de los polinomios de Raviart-Thomas evaluados en los 
*     puntos de interpolacion (formula de cuadratura de 3 puntos)

      do 103 i0=1,3
         do 104 j=1,2
            xg(j)=pgauss(i0,j)
 104     continue
         do 105 j=1,2   
         CP(1,1,j)=(-L(j,1)-L(j,3)*xg(2))/2.d0 
         CP(1,2,j)=(-L(j,1)+L(j,2)*xg(2))/2.d0         
         CP(1,3,j)=(-L(j,1)*xg(2))/2.d0
         CP(2,1,j)=(-L(j,2)+L(j,3)*xg(1))/2.d0          
         CP(2,2,j)=(-L(j,2)*xg(1))/2.d0   
         CP(2,3,j)=(-L(j,2)+L(j,1)*xg(1))/2.d0 
 105     continue
         CPL(1,1)=(2.d0/3.d0)*med(1)*(1-xg(2)) 
         CPL(1,2)=(2.d0/3.d0)*(-med(2)*xg(2)) 
         CPL(1,3)=(2.d0/3.d0)*(-med(3)*xg(2))
         CPL(2,1)=(2.d0/3.d0)*med(1)*xg(1)
         CPL(2,2)=(2.d0/3.d0)*med(2)*xg(1)
         CPL(2,3)=(2.d0/3.d0)*med(3)*(xg(1)-1)
         
         do 106 k=1,2
            do 110 j=1,3
               RG(k,j)=CP(k,j,1) 
               RG(k,j+3)=CP(k,j,2) 
               RG(k,j+6)=CPL(k,j) 
               RG(k,j+9)=DN(k,j)       
 110        continue     
 106     continue
         
         do 200 i=1,12
            do 201 j=1,12
               do 202 k=1,2
                  do 203 kk=1,2
                     do 204 ii=1,2
                           KR(i,j)=KR(i,j)+RG(k,i)*DkInv(k,kk)*
     &                                    DkInv(ii,kk)*RG(ii,j) 
 204                 continue
 203              continue
 202           continue
 201        continue
 200     continue
               
 103  continue
      
      !Cambio desde la integral en el elemento de referencia al real
      do 210 i=1,12
         do 211 j=1,12
            KR(i,j)=KR(i,j)*det/6.d0
 211     continue 
 210  continue

*     Sumamos KR*facR, KA*facA y KIP*facInPlane para conformar la matriz final KRe 
      do 310 i=1,12
         do 311 j=1,12
            KRe(i,j)=KRe(i,j)+KR(i,j)*facR
 311     continue
 310  continue

      do 312 i=1,9
         do 313 j=1,9
            KRe(i,j)=KRe(i,j)+KA(i,j)*facA
 313     continue
 312  continue

      do 314 i=1,6
         do 315 j=1,6
            KRe(i+12,j+12)=KRe(i+12,j+12)+KIP(i,j)*facInPlane
 315     continue
 314  continue

      return
      end
