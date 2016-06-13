      subroutine matef1_presion(KRe,InvDk,det,dens,vels)
      
      implicit double precision(a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"

      double precision InvDk(3,3),Grad(4,3),det,coef,dens,vels 
      
      PetscScalar    KRe(4,4)
      
      do 10 i=1,4
         do 11 j=1,4
            KRe(i,j)=0.d0
 11      continue
 10   continue
 
      do 30 i=1,3
         Grad(1,i) = -(InvDk(i,1)+InvDk(i,2)+InvDk(i,3))
         Grad(2,i) =   InvDk(i,1)
         Grad(3,i) =   InvDk(i,2)
         Grad(4,i) =   InvDk(i,3)
 30   continue
 
      coef=det/6/dens
      do 40 i=1,4
         do 41 j=i,4
            do 42 k=1,3
                  KRe(i,j)=KRe(i,j) + Grad(i,k)*Grad(j,k)
 42         continue
            KRe(i,j)=KRe(i,j)*coef
 41      continue
 40   continue
 
      do 50 i=1,4
         do 51 j=i+1,4
            KRe(j,i)=KRe(i,j)
 51      continue
 50   continue
 
      return
      end
