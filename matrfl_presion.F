!******************************************************************
!*
!*     CALCULO DE LAS MATRICES DE RIGIDEZ Y MASA EN EL FLUIDO
!*     CON LA PRESION COMO INCOGNITA     
!*
!******************************************************************


      subroutine matrfl_presion(z,mm,nel,NGLP,NGLS,vol,nsd,densif,
     &                  velso,nrc,nrrf,irefrf,rk,rm,M,KR,vec_perm)

      implicit double precision(a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      
      dimension z(3,*),nsd(*),mm(4,*),bar(3),vol(*),nrc(4,*),
     &          irefrf(*),rk(*),rm(*),Dk(3,3),adj(3,3)
      double precision InvDk(3,3)
      integer vec_perm(*)

      PetscScalar    Me(4,4),KRe(4,4)
      Mat            M,KR

!*
!******     Bucle en elementos para calculo de matrices y ensamblado.
!*
      write(*,98) ' NELF = ',nel
 98   format(A9,I8)
      iaux=int(nel/100)
      if(iaux-nel*100.ne.0) iaux=iaux+1

      do 2000 k=1,nel
         do 12 i=1,3
            bar(i)=0.d0
            do 13 j=1,4
               bar(i)=bar(i)+z(i,mm(j,k))
 13         continue
            bar(i)=bar(i)/4.d0
 12      continue
      
         do 22 i=1,3
            do 23 j=1,3
               Dk(i,j)=z(j,mm(i+1,k))-z(j,mm(1,k))
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


!*         
!******     Calculo de matrices elementales de rigidez y de masa.
!*
         dens=densif(bar(1),bar(2),bar(3),nsd(k))
         vels=velso(bar(1),bar(2),bar(3),nsd(k))
       
         call matef1_presion(KRe,InvDk,det,dens,vels)
         call matef2_presion(Me ,det,dens,vels)
        
!*
!******     Condiciones tipo Robin
!*
!         do 6 j=1,4
!            if (nrrf.gt.0) then
!               do 7 ir=1,nrrf
!                  if (nrc(j,k).eq.irefrf(ir)) then
!                     rkk=rk(ir)
!                     rmm=rm(ir)
!                     call robin(j,Me,KRe,area(j),rkk,rmm)
!                  endif
! 7             continue
!            endif
! 6       continue

!*
!******     Llamada a subrutina de ensamblado de las matrices elementales.
!*
         call ensf(M,Me,mm(1,k),NGLP,NGLS,vec_perm)
         call ensf(KR,KRe,mm(1,k),NGLP,NGLS,vec_perm)
      
         if(mod(k,iaux*10).eq.0) write(*,99) k/iaux, '% calculado'
 99      format(2x,I2,A11)
   
 2000 continue
 
      write(*,*) '100% calculado'
 
      return
      end

