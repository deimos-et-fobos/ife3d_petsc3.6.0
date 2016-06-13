************************************************************************
*                                                                      *
*                 Ensamblado de las matrices elementales               *
*                                                                      *
*        VARIABLES:                                                    *
*                                                                      *
*          ENTRADA:                                                    *
*            a -------> Matriz elemental                               *
*            mu ------> Puntero de almacenamiento de la matriz         *
*            n -------> Numeros de los nodos del elemento              *
*                                                                      *
*          ENTRADA/SALIDA:                                             *
*            c -------> Vector que contiene la matriz del sistema      *
*                                                                      *
*                                                                      *
************************************************************************

      subroutine ensf(A,Ae,n,NGLP,NGLS,vec_perm)

      implicit double precision(a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      common/p_fluido/varf

      dimension n(*)
      integer vec_perm(*)
      character*20 varf

      PetscInt       ID4(4)
      PetscScalar    Ae(4,4)
      Mat            A

      do 1 i=1,4
         ID4(i) =vec_perm( n(i) + NGLP + NGLS ) !(LOS INDICES EN PETSC VAN DESDE 0)
 1    continue

      if(varf(1:7).eq.'presion')then 
        do 12 i=1,4
          do 13 j=1,4
            CALL MatSetValue(A,ID4(i),ID4(j),Ae(i,j),ADD_VALUES,IERR)
 13       continue
 12     continue
      else
        do 2 i=1,4
          do 3 j=1,4
            if(ID4(j).ge.ID4(i))then
              CALL MatSetValue(A,ID4(i),ID4(j),Ae(i,j),ADD_VALUES,IERR)
            endif
 3        continue
 2      continue
      endif

      return
      end
