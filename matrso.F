*************************************************************
*     calculo de la matriz
*************************************************************

      subroutine matrso(z,mm,nel,NGLP,nsd,coefs,vol,
     &                  M,KR,vec_perm)

********************definicion de parametros********************

**********                                           ***********

      implicit double precision(a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"

      dimension z(3,*),nsd(*),mm(4,*),bar(3),vol(*),
     &       C(6,6),Dk(3,3),adj(3,3)
      double precision lambda,mu,det
      double precision InvDk(3,3)
      integer vec_perm(*)
      
      PetscScalar    Me(12,12),KRe(12,12)
      Mat            M,KR
!*
!******     Bucle en elementos para calculo de matrices y ensamblado.
!*
      write(*,98) ' NELS = ',nel
 98   format(A9,I8)
      iaux=int(nel/100)
      if(iaux-nel*100.ne.0) iaux=iaux+1

      do 2000 k=1,nel
!*         
!******     Calculo de la matriz auxiliar C
!*   
         do 12 i=1,3
            bar(i)=0.d0
            do 13 j=1,4
               bar(i)=bar(i)+z(i,mm(j,k))
 13         continue
            bar(i)=bar(i)/4.d0
 12      continue
 
         lambda=coefs(1,0,0,0,nsd(k))
         mu=coefs(2,0,0,0,nsd(k))
         dens=densis(bar(1),bar(2),bar(3),nsd(k))
         
         do 20 i=1,3
            do 21 j=1,3
               C(i,j)= lambda
               C(i+3,j)= 0.d0
               C(i,j+3)= 0.d0
               C(i+3,j+3)= 0.d0
 21         continue
         C(i,i)=C(i,i)+2*mu
         C(i+3,i+3)= mu
 20      continue
         
         do 22 i=1,3
            do 23 j=1,3
               Dk(i,j)=z(i,mm(j+1,k))-z(i,mm(1,k))
 23         continue
 22      continue

         det=6*vol(k)
         do 24 i=1,3
            do 25 j=1,3
               adj(i,j)=Dk(1+mod(i,3),1+mod(j,3))*Dk(1+mod(i+1,3),
     &                  1+mod(j+1,3))-Dk(1+mod(i,3),1+mod(j+1,3))*
     &                  Dk(1+mod(i+1,3),1+mod(j,3))
               InvDk(j,i)=adj(i,j)/det
 25         continue
 24      continue        
 
         call mates1(KRe,C,InvDk,vol(k))
         call mates2(Me,dens,det)

         call enss(KR,KRe,mm(1,k),NGLP,vec_perm)
         call enss( M,Me,mm(1,k),NGLP,vec_perm)
      
         if(mod(k,iaux*10).eq.0) then
           write(*,99) k/iaux, '% calculado'
         endif
 99      format(2x,I2,A11)
 
 2000 continue
      
      write(*,*) '100% calculado'
      
      return
      end
