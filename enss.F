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

      subroutine enss(A,Ae,n,NGLP,vec_perm)

      implicit double precision(a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      common/p_fluido/varf

      dimension n(*)
      integer vec_perm(*)
      character*20 varf

      PetscInt    ID12(12)
      PetscScalar Ae(12,12)
      Mat         A

      do 1 i=1,4
         do 2 j=1,3
            ID12(3*(i-1)+j)=vec_perm( 3*(n(i)-1) + j + NGLP ) !(LOS INDICES EN PETSC VAN DESDE 0)
 2       continue
 1    continue
      
      if(varf(1:7).eq.'presion')then
        do 14 i=1,12
         do 15 j=1,12
           CALL MatSetValue(A,ID12(i),ID12(j),Ae(i,j),ADD_VALUES,IERR)
 15      continue
 14     continue
      else
        do 4 i=1,12
         do 5 j=1,12
           if(ID12(j).ge.ID12(i))then
            CALL MatSetValue(A,ID12(i),ID12(j),Ae(i,j),ADD_VALUES,IERR)
           endif
 5       continue
 4      continue
      endif

      return
      end
