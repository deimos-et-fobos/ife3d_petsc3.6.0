      subroutine matef2_presion(Me,det,dens,vels)
      
      implicit double precision(a-h,o-z)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"
      
      double precision N(4,4)
      
      PetscScalar    Me(4,4)
      
      val=det/120.d0/vels/vels/dens
      do 1 i=1,4
         do 2 j=1,4
            Me(i,j)=val
 2       continue
         Me(i,i)=Me(i,i)*2
 1    continue
      
      return
      end
