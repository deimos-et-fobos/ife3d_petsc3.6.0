      subroutine mates2(Me,dens,det)
      
      implicit double precision(a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      
      double precision N(4,4)
      integer aux(4)    
      
      PetscScalar    Me(12,12)
       
      do 1 i=1,12
         do 2 j=1,12
            Me(i,j)=0.d0
 2       continue
 1    continue
      
      val=dens*det/120.d0
      do 10 i=1,4
         do 11 j=1,4
            N(i,j)=val
 11      continue
         N(i,i)=N(i,i)*2
 10   continue

      do 20 i=1,4
         aux(i)=3*(i-1)+1
 20   continue

      do 21 i=1,4
         do 22 j=1,4
            Me(aux(i),aux(j))=N(i,j)
            Me(aux(i)+1,aux(j)+1)=N(i,j)
            Me(aux(i)+2,aux(j)+2)=N(i,j)
 22      continue
 21   continue

      return
      end
