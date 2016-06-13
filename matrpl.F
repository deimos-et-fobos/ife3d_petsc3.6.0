*************************************************************
*     calculo de la matriz
*************************************************************

      subroutine matrpl(z,mm,nn,ng,nel,cnum,nsd,coefp,
     &                  nu,thin,M,KR,vec_perm)

********************definicion de parametros********************

**********                                           ***********

      implicit double precision(a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"

      dimension z(2,*),nsd(*),mm(3,*),nn(6,*),
     &       bar(2),ver(2,3),tauk(2,3),a1k(2),Dk(2,2),DkInv(2,2),
     &       C(3,3),H(4,4),CA(4,4),DN(2,3),DQ(2,3,3),ng(3,*),nu(3,*),
     &       pgauss(3,2),xg(2),thin(*)
      double precision med(3),young,pois,Kacorr,Kappa,
     &                 facInPlane,facR,facA,det
      integer cnum(*),vec_perm(*)

      PetscScalar    Me(18,18),KRe(18,18)
      Mat            M,KR

*     Calculo das derivadas dos polinomios de base para as rotacions
*     puntos de Gauss en coordenadas baricentricas

      pgauss(1,1)=1/2.d0; pgauss(1,2)= 0.d0
      pgauss(2,1)= 0.d0;  pgauss(2,2)=1/2.d0
      pgauss(3,1)=1/2.d0; pgauss(3,2)=1/2.d0
      
*     Derivadas de los polinomios de base de P1
      
      DN(1,1)=-1.d0; DN(1,2)=1.d0; DN(1,3)=0.d0
      DN(2,1)=-1.d0; DN(2,2)=0.d0; DN(2,3)=1.d0

*     Derivadas de los polinomios burbuja evaluadas 
*     en los puntos de cuadratura

      do 300 i=1,3
         do 301 j=1,2
            xg(j)=pgauss(i,j)
 301     continue
         DQ(1,1,i)= 4*(1-2*xg(1)-xg(2))
         DQ(1,2,i)= 4*xg(2)
         DQ(1,3,i)=-4*xg(2)
         DQ(2,1,i)=-4*xg(1)
         DQ(2,2,i)= 4*xg(1)
         DQ(2,3,i)= 4*(1-xg(1)-2*xg(2))
 300  continue

************************************************************************

********bucle en elementos para calculo de matrices y ensamblado********
      write(*,98) ' NELP = ',nel
 98   format(A9,I8)
      iaux=int(nel/100)
      if(iaux-nel*100.ne.0) iaux=iaux+1

      do 2000 k=1,nel

************************************************************************

      young=coefp(1,0,0,nsd(k))
      pois=coefp(2,0,0,nsd(k))
      Kacorr=coefp(3,0,0,nsd(k))
      
*     Calculo de coeficientes y matrices auxiliares de la placa
*     Modulo de corte

      Kappa=young*Kacorr/(2.d0*(1.d0+pois))
      facR=Kappa/(thin(k)**2)
      facA=young/(12.d0*(1.d0-(pois**2)))
      facInPlane=(young/(1.d0-(pois**2)))/thin(k)**3;

*     Matriz auxiliar usada para el calculo de la matriz elemental KR
      
      C(1,1)=1; C(1,2)=pois; C(1,3)=0
      C(2,1)=pois; C(2,2)=1; C(2,3)=0
      C(3,1)=0; C(3,2)=0; C(3,3)=(1-pois)/2.d0
      
      do 100 i=1,3
         do 110 j=1,4
            H(i,j)=0.d0
 110      continue     
 100   continue
      H(1,1)=1.d0
      H(2,4)=1.d0
      H(3,2)=1.d0
      H(3,3)=1.d0 
      
      do 120 i=1,4
         do 130 j=1,4
            CA(i,j)=0.d0
 130      continue     
 120   continue
 
      do 200 i=1,4
         do 210 l=1,4
            do 220 j=1,3
               do 230 kk=1,3
                  CA(i,l)=CA(i,l)+H(j,i)*C(j,kk)*H(kk,l)
 230           continue
 220        continue
 210     continue
 200  continue

************************************************************************


*     abreviatura en la notacion

         mm1=mm(1,k)
         mm2=mm(2,k)
         mm3=mm(3,k)

*     calculo de las tangentes de los lados

         do 10 i=1,3
            ver(1,i)=z(1,mm(i,k))
            ver(2,i)=z(2,mm(i,k))
!            write(55,*) ver(1,i),ver(2,i)
 10      continue

         do 11 i=1,3
            tauk(1,i)=ver(1,1+mod(i,3))-ver(1,i)
            tauk(2,i)=ver(2,1+mod(i,3))-ver(2,i)
!            write(56,*) tauk(1,i),tauk(2,i)
 11      continue

         do 12 i=1,3
            med(i)=dsqrt(tauk(1,i)**2+tauk(2,i)**2)
 12      continue
!         write(57,*) med(1),med(2),med(3)
 
         do 14 i=1,3
            do 15 j=1,2
               tauk(j,i)=tauk(j,i)/med(i)
 15         continue
!            write(58,*) tauk(1,i),tauk(2,i)
 14      continue

*     matrices auxiliares para calculo la matriz de masa elem
         
         do 20 i=1,2
            Dk(i,1)=z(i,mm2)-z(i,mm1)
            Dk(i,2)=z(i,mm3)-z(i,mm1)
 20      continue
!         write(159,*) Dk(1,1),Dk(1,2),Dk(2,1),Dk(2,2)
         
         det=dabs(Dk(1,1)*Dk(2,2)-Dk(1,2)*Dk(2,1))
!         write(60,*) det
         
         DkInv(1,1)= Dk(2,2)/det
         DkInv(1,2)=-Dk(1,2)/det
         DkInv(2,1)=-Dk(2,1)/det
         DkInv(2,2)= Dk(1,1)/det
!         write(61,*) DkInv(1,1),DkInv(1,2),DkInv(2,1),DkInv(2,2)

*     calculo del baricentro del triangulo

         do 30 j=1,2
            bar(j)=(z(j,mm1)+z(j,mm2)+z(j,mm3))/3.d0
 30      continue

******subrut. para calculo de matriz elem.de masa

         dens=densip(bar(1),bar(2),nsd(k))
         call matep1(KRe,CA,tauk,med,det,Kappa,facR,facA,facInPlane,
     &               ver,thin(k),DN,DQ,pgauss,DkInv)
         call matep2(det,Me,tauk,dens,thin(k))

         call ensp(M,Me,cnum,nn(1,k),ng(1,k),thin(k),vec_perm,k)
         call ensp(KR,KRe,cnum,nn(1,k),ng(1,k),thin(k),vec_perm,k)
      
         if(mod(k,iaux*10).eq.0) write(*,99) k/iaux, '% calculado'
 99      format(2x,I2,A11)

 2000 continue
    
      write(*,*) '100% calculado'
    
      return
      end
