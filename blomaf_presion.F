      subroutine blomaf_presion(KR,mmf,nrcf,nelf,NGP,NGS,vec_perm)
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
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      
      dimension mmf(4,*),nrcf(4,*)
      integer vec_perm(*)

      PetscScalar V
      PetscInt    IGL
      Mat         KR

      V=1.D50

      do 1 k=1,nelf
        do 2 j=1,4
          if(nrcf(j,k).eq.2) then
            do 3 i=1,4
              if(i.ne.j)then
                IGL = vec_perm(mmf(i,k) + NGP + NGS) !(LOS INDICES EN PETSC VAN DESDE 0)
                CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
              endif
 3          continue
          endif
 2      continue
 1    continue
      return
      end
