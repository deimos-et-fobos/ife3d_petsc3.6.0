      subroutine matef2(KRe,vol,coef,area,sigm)
      
      implicit double precision(a-h,o-z)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"

      dimension area(4)
      integer sigm(4)
      
      PetscScalar    KRe(4,4)
      
      do 10 i=1,4
         do 20 j=1,4
            KRe(i,j)=coef*sigm(i)*sigm(j)*area(i)*area(j)/vol
 20      continue
 10   continue

      return
      end
