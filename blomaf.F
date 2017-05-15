      subroutine blomaf(KR,nnf,nrcf,nelf,NGP,NGS,vec_perm)
***************************************************************
*                       subrutina blomat                      *
***************************************************************
*     objetivo:                                               *
*     ========  bloqueo de las condiciones de contorno        *
*               del fluido en interaccion fluido-estr.        *
*                                                             *
*-------------------------------------------------------------*
*               parametros de la subrutina                    *
*               ==========================                    *
*-------------------------------------------------------------*
*                                                             *
      implicit double precision (a-h,o-z)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"
      
      dimension nnf(4,*),nrcf(4,*)
      integer vec_perm(*)

      PetscScalar V
      PetscInt    IGL
      Mat         KR

      V=1.D50

      do 1 k=1,nelf
         do 2 j=1,4
            if(nrcf(j,k).eq.1) then
               IGL = vec_perm(nnf(j,k) + NGP + NGS) !(LOS INDICES EN PETSC VAN DESDE 0)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
 2       continue
 1    continue
      return
      end
