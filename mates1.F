      subroutine mates1(KRe,C,InvDk,vol)
      
      implicit double precision(a-h,o-z)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"

      double precision InvDk(3,3),C(6,6),Grad(4,3),R(6,12)     
      integer aux(9)
      
      PetscScalar    KRe(12,12)
      
      aux(1)=1;aux(2)=4;aux(3)=5;aux(4)=4;aux(5)=2;aux(6)=6;
      aux(7)=5;aux(8)=6;aux(9)=3;

      do 10 i=1,12
         do 11 j=1,12
            KRe(i,j)=0.d0
 11      continue
 10   continue
 
      do 12 i=1,6
         do 13 j=1,12
            R(i,j)=0.d0
 13      continue
 12   continue

      do 30 i=1,3
         Grad(1,i) = -(InvDk(1,i)+InvDk(2,i)+InvDk(3,i))
         Grad(2,i) =   InvDk(1,i)
         Grad(3,i) =   InvDk(2,i)
         Grad(4,i) =   InvDk(3,i)
 30   continue
 
      do 33 i=1,3
         do 34 j=1,4
            R(aux(i),1+(j-1)*3)=Grad(j,i)
            R(aux(i+3),2+(j-1)*3)=Grad(j,i)
            R(aux(i+6),j*3)=Grad(j,i)
 34      continue
 33   continue

      do 40 i=1,12
         do 41 j=i,12
            do 42 k=1,6
               do 43 l=1,6
                  KRe(i,j)=KRe(i,j)+ R(k,i)*C(k,l)*R(l,j)
 43            continue                  
 42         continue
            KRe(i,j)=vol*KRe(i,j)
 41      continue
 40   continue
 
      do 50 i=1,12
         do 51 j=i+1,12
            KRe(j,i)=KRe(i,j)
 51      continue
 50   continue
 
      return
      end
