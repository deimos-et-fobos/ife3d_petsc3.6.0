       subroutine coifmo(icaco,ncaco,xa,NGLP,NGLS,
     &                   DOF,KR,vec_perm)

*************************************************************************
*	By Ronia 							                  *
*	Subrutina que realiza el calculo de la interfase 		      *
*	fluido-cara de contacto y almacena el resultado en un vector	*
*	factorizado tipo morse						            *
*									                  *
*	Variables:							                  *
*	  								                  *
*	   icaco    --->  Numeracion de los nodos en las caras de	      *
*			      contacto (del fluido)				      *
*	   ncaco    --->  Numero de caras de contacto			      *
*	   xa	      --->  Superficie de las caras de contacto		      *
*									                  *
*	   mua1d    --->  Primer puntero para almacenamiento morse		*
*	   mua2d    --->  Segundo puntero para almacenamiento morse		*
*	   dco      --->  Vector donde almacenaremos los resultados		*
*									                  *
*************************************************************************

      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"

      dimension icaco(*),xa(*)
      integer vec_perm(*)

      PetscInt    COL,ROW,DOF
      PetscScalar VAL
      Mat         KR

      do 1 i=1,ncaco                ! loop sobre las caras de contacto
         COL = vec_perm(DOF - NCACO + i)  !(LOS INDICES EN PETSC VAN DESDE 0)
         ROW = vec_perm(icaco(i) + NGLP + NGLS )
         VAL = -xa(i)               ! vector solucion
      
         if(COL.ge.ROW)then
           CALL MATSETVALUE(KR,ROW,COL,VAL,ADD_VALUES,IERR)
         else
           CALL MATSETVALUE(KR,COL,ROW,VAL,ADD_VALUES,IERR)
         endif
 1    continue

      return
      end
