*******************************************************************
*
*     MODIFICACION DE LAS MATRICES ELEMENTALES
*     POR CONDICIONES DE BORDE TIPO ROBIN
*     
*******************************************************************

      subroutine robin (j,Me,KRe,area,rk,rm)

      implicit double precision(a-h,k-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      
      PetscScalar    Me(4,4),KRe(4,4)

      Me(j,j)=Me(j,j)+rm*area
      KRe(j,j)=KRe(j,j)+rk*area

      return
      end
