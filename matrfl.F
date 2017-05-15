!******************************************************************
!*
!*     CALCULO DE LAS MATRICES DE RIGIDEZ Y MASA EN EL FLUIDO
!*
!******************************************************************


      subroutine matrfl(z,nn,mm,ng,nel,NGLP,NGLS,vol,nsd,densif,
     &                  velso,nrc,nrrf,irefrf,rk,rm,M,KR,vec_perm)

      implicit double precision(a-h,o-z)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"
      
      dimension z(3,*),nn(4,*),nsd(*),
     &          mm(4,*),bar(3),ng(4,*),vol(*),u(3),v(3),nrc(4,*),
     &          irefrf(*),rk(*),rm(*),p(3,4),area(4),auxM(12,12)
      integer cn(4,4),sigm(4),vec_perm(*)

      PetscScalar    Me(4,4),KRe(4,4)
      Mat            M,KR

      cn(1,1)=2; cn(1,2)=3; cn(1,3)=4; cn(1,4)=1
      cn(2,1)=1; cn(2,2)=3; cn(2,3)=4; cn(2,4)=2
      cn(3,1)=1; cn(3,2)=2; cn(3,3)=4; cn(3,4)=3
      cn(4,1)=1; cn(4,2)=2; cn(4,3)=3; cn(4,4)=4

!*
!******     Creacion de la matriz auxiliar de masa.
!*
      do 11 i=1,12
         do 12 j=1,12
            auxM(i,j)=0.d0
 12      continue
 11   continue
 
      do 13 i=1,12
         auxM(i,i)=2.d0
         auxM(i,1+mod(i+2,12))=1.d0
         auxM(i,1+mod(i+5,12))=1.d0
         auxM(i,1+mod(i+8,12))=1.d0
 13   continue
 
!      do 14 i=1,12
!         write(70,*) (auxM(i,j),j=1,12)
! 14   continue
!      do 15 i=1,nel
!         write(701,*) (nn(j,i),j=1,4)
!         write(702,*) (mm(j,i),j=1,4)
!         write(703,*) (ng(j,i),j=1,4)
!         do 16 k=1,3
!            write(704,*) (z(k,mm(j,i)),j=1,4)
! 16      continue            
! 15   continue

!*
!******     Bucle en elementos para calculo de matrices y ensamblado.
!*
      write(*,98) ' NELF = ',nel
 98   format(A9,I8)
      iaux=int(nel/100)
      if(iaux-nel*100.ne.0) iaux=iaux+1

      do 20 k=1,nel
         do 21 i=1,3
            bar(i)=0.d0
            do 22 j=1,4
               p(i,j)=z(i,mm(j,k))
               bar(i)=bar(i)+z(i,mm(j,k))
 22         continue
            bar(i)=bar(i)/4.d0
 21      continue
      
         do 23 j=1,4
            do 24 i=1,3
               u(i)=p(i,cn(j,2))-p(i,cn(j,1))
               v(i)=p(i,cn(j,3))-p(i,cn(j,1))
 24         continue  
            sigm(j)=ng(j,k)
            area(j)=dabs(dsqrt((u(2)*v(3)-u(3)*v(2))**2+(u(3)*v(1)-
     &                  u(1)*v(3))**2+(u(1)*v(2)-u(2)*v(1))**2))/2.d0
 23      continue
         
!         do 100 i=1,4
!            write(71,*) sigm(i),area(i)
!            write(72,*) (p(j,i),j=1,3)
! 100     continue

!*         
!******     Calculo de matrices elementales de rigidez y de masa.
!*
         dens=densif(bar(1),bar(2),bar(3),nsd(k))
         vels=velso(bar(1),bar(2),bar(3),nsd(k))
         coef=dens*vels*vels
         


         call matef1(Me,auxM,p,vol(k),dens,area,sigm)
         call matef2(KRe,vol(k),coef,area,sigm)
        
!         do 110 i=1,4
!            write(77,*) (M(i,j),j=1,4)
!            write(78,*) (KR(i,j),j=1,4)
! 110     continue

!*
!******     Condiciones tipo Robin
!*
         do 6 j=1,4
            if (nrrf.gt.0) then
               do 7 ir=1,nrrf
                  if (nrc(j,k).eq.irefrf(ir)) then
                     rkk=rk(ir)
                     rmm=rm(ir)
                     call robin(j,Me,KRe,area(j),rkk,rmm)
                  endif
 7             continue
            endif
 6       continue

!*
!******     Llamada a subrutina de ensamblado de las matrices elementales.
!*
         call ensf(M,Me,nn(1,k),NGLP,NGLS,vec_perm)
         call ensf(KR,KRe,nn(1,k),NGLP,NGLS,vec_perm)
      
         if(mod(k,iaux*10).eq.0) write(*,99) k/iaux, '% calculado'
 99      format(2x,I2,A11)
   
 20   continue
 
      write(*,*) '100% calculado'
 
      return
      end

