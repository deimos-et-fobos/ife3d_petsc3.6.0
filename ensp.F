******                                                 ******
******esta subrutina efectua el ensamblado de matrices ******
******                                                 ******

      subroutine ensp(A,Ae,cnum,nn,ng,thin,vec_perm,k)
      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      common/p_fluido/varf

      dimension nn(*),ng(3)
      integer cnum(*),aux(18),vec_perm(*)
      character*20 varf

      PetscInt       ID18(18)
      PetscScalar    Ae(18,18)
      Mat            A

*     Tenemos en cuenta el signo definido para cada lado del elemento

      do 10 i=1,18
         aux(i)=1
 10   continue

      do 11 i=1,3
         aux(i+6)=ng(i)
 11   continue

      do 12 i=1,18
         do 13 j=1,18
            Ae(i,j)=Ae(i,j)*aux(i)*aux(j)*(thin**3)
 13      continue
 12   continue 

************************************************************************

*     EN LA MATRIZ ELEMENTAL EL ORDEN DE LAS INCOGNITAS ES:
*     [RX1,RX2,RX3,RY1,RY2,RY3,B1,B2,B3,W1,W2,W3]

      do 15 i=1,3
         !(LOS INDICES EN PETSC VAN DESDE 0) 
         ID18(i)   = vec_perm( cnum(nn(i))+2 )  !rot_x 
         ID18(i+3) = vec_perm( cnum(nn(i))+3 )  !rot_y
         ID18(i+6) = vec_perm( cnum(nn(i+3))+1 )!beta
         ID18(i+9) = vec_perm( cnum(nn(i))+1 )  !desp_z
         ID18(i+12)= vec_perm( cnum(nn(i))+4 )  !desp_x
         ID18(i+15)= vec_perm( cnum(nn(i))+5 )  !desp_y
 15   continue 
************************************************************************
      if(varf(1:7).eq.'presion')then
        do 24 i=1,18
          do 25 j=1,18
            CALL MatSetValue(A,ID18(i),ID18(j),Ae(i,j),ADD_VALUES,IERR)
 25       continue
 24     continue
      else
        do 4 i=1,18
         do 5 j=1,18
          if(ID18(j).ge.ID18(i))then
            CALL MatSetValue(A,ID18(i),ID18(j),Ae(i,j),ADD_VALUES,IERR)
          endif
 5       continue
 4     continue
      endif

      return
      end
