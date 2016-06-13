      subroutine ccoipmos(KR,COL,row_x,row_y,row_z,psi,hz,dirp)

      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      common/p_fluido/varf
      
      integer dirp
      character*20 varf

      PetscInt    COL,row_x(2,2)
      PetscInt    row_y(2,2),row_z(2,2)
      PetscScalar VAL(2)
      Mat         KR

      if(varf(1:7).eq.'presion')then
************************************************************************
*     Desplazamiento transversal
************************************************************************
      val(1)= 1 - psi
      val(2)= psi
      do 21 j=1,2      
        if(dirp.eq.1)then
            CALL MatSetValue(KR,row_x(j,1),COL,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL,row_x(j,1),VAL(j),ADD_VALUES,IERR)
        endif
        if(dirp.eq.2)then
            CALL MatSetValue(KR,row_y(j,1),COL+1,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL+1,row_y(j,1),VAL(j),ADD_VALUES,IERR)
        endif
        if(dirp.eq.3)then
            CALL MatSetValue(KR,row_z(j,1),COL+2,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL+2,row_z(j,1),VAL(j),ADD_VALUES,IERR)
        endif
 21   continue

************************************************************************
*     Rotaciones
************************************************************************
      val(1)= hz*(1 - psi)
      val(2)= hz*psi
      do 22 j=1,2      
        if(dirp.eq.1)then
            CALL MatSetValue(KR,row_y(j,1),COL+1,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL+1,row_y(j,1),VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,row_z(j,1),COL+2,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL+2,row_z(j,1),VAL(j),ADD_VALUES,IERR)
        endif
        if(dirp.eq.2)then
            CALL MatSetValue(KR,row_z(j,1),COL+2,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL+2,row_z(j,1),VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,row_x(j,1),COL,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL,row_x(j,1),VAL(j),ADD_VALUES,IERR)
        endif
        if(dirp.eq.3)then
            CALL MatSetValue(KR,row_x(j,1),COL,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL,row_x(j,1),VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,row_y(j,1),COL+1,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL+1,row_y(j,1),VAL(j),ADD_VALUES,IERR)
        endif
 22   continue

************************************************************************
*     Desplazamientos in-plane
************************************************************************
      val(1)= 1 - psi
      val(2)= psi
      do 23 j=1,2      
        if(dirp.eq.1)then
            CALL MatSetValue(KR,row_y(j,2),COL+1,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL+1,row_y(j,2),VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,row_z(j,2),COL+2,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL+2,row_z(j,2),VAL(j),ADD_VALUES,IERR)
        endif
        if(dirp.eq.2)then
            CALL MatSetValue(KR,row_z(j,2),COL+2,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL+2,row_z(j,2),VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,row_x(j,2),COL,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL,row_x(j,2),VAL(j),ADD_VALUES,IERR)
        endif
        if(dirp.eq.3)then
            CALL MatSetValue(KR,row_x(j,2),COL,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL,row_x(j,2),VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,row_y(j,2),COL+1,VAL(j),ADD_VALUES,IERR)
            CALL MatSetValue(KR,COL+1,row_y(j,2),VAL(j),ADD_VALUES,IERR)
        endif
 23   continue

      else
************************************************************************
*     Desplazamiento transversal
************************************************************************
      val(1)= 1 - psi
      val(2)= psi
      do 11 j=1,2      
        if(dirp.eq.1)then
          if(COL.ge.row_x(j,1))then
            CALL MatSetValue(KR,row_x(j,1),COL,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL,row_x(j,1),VAL(j),ADD_VALUES,IERR)
          endif
        endif
        if(dirp.eq.2)then
          if(COL+1.ge.row_y(j,1))then
            CALL MatSetValue(KR,row_y(j,1),COL+1,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL+1,row_y(j,1),VAL(j),ADD_VALUES,IERR)
          endif
        endif
        if(dirp.eq.3)then
          if(COL+2.ge.row_z(j,1))then
            CALL MatSetValue(KR,row_z(j,1),COL+2,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL+2,row_z(j,1),VAL(j),ADD_VALUES,IERR)
          endif
        endif
 11   continue

************************************************************************
*     Rotaciones
************************************************************************
      val(1)= hz*(1 - psi)
      val(2)= hz*psi
      do 12 j=1,2      
        if(dirp.eq.1)then
          if(COL+1.ge.row_y(j,1))then
            CALL MatSetValue(KR,row_y(j,1),COL+1,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL+1,row_y(j,1),VAL(j),ADD_VALUES,IERR)
          endif
          if(COL+2.ge.row_z(j,1))then
            CALL MatSetValue(KR,row_z(j,1),COL+2,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL+2,row_z(j,1),VAL(j),ADD_VALUES,IERR)
          endif
        endif
        if(dirp.eq.2)then
          if(COL+2.ge.row_z(j,1))then
            CALL MatSetValue(KR,row_z(j,1),COL+2,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL+2,row_z(j,1),VAL(j),ADD_VALUES,IERR)
          endif
          if(COL.ge.row_x(j,1))then
            CALL MatSetValue(KR,row_x(j,1),COL,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL,row_x(j,1),VAL(j),ADD_VALUES,IERR)
          endif
        endif
        if(dirp.eq.3)then
          if(COL.ge.row_x(j,1))then
            CALL MatSetValue(KR,row_x(j,1),COL,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL,row_x(j,1),VAL(j),ADD_VALUES,IERR)
          endif
          if(COL+1.ge.row_y(j,1))then
            CALL MatSetValue(KR,row_y(j,1),COL+1,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL+1,row_y(j,1),VAL(j),ADD_VALUES,IERR)
          endif
        endif
 12   continue

************************************************************************
*     Desplazamientos in-plane
************************************************************************
      val(1)= 1 - psi
      val(2)= psi
      do 13 j=1,2      
        if(dirp.eq.1)then
          if(COL+1.ge.row_y(j,2))then
            CALL MatSetValue(KR,row_y(j,2),COL+1,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL+1,row_y(j,2),VAL(j),ADD_VALUES,IERR)
          endif
          if(COL+2.ge.row_z(j,2))then
            CALL MatSetValue(KR,row_z(j,2),COL+2,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL+2,row_z(j,2),VAL(j),ADD_VALUES,IERR)
          endif
        endif
        if(dirp.eq.2)then
          if(COL+2.ge.row_z(j,2))then
            CALL MatSetValue(KR,row_z(j,2),COL+2,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL+2,row_z(j,2),VAL(j),ADD_VALUES,IERR)
          endif
          if(COL.ge.row_x(j,2))then
            CALL MatSetValue(KR,row_x(j,2),COL,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL,row_x(j,2),VAL(j),ADD_VALUES,IERR)
          endif
        endif
        if(dirp.eq.3)then
          if(COL.ge.row_x(j,2))then
            CALL MatSetValue(KR,row_x(j,2),COL,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL,row_x(j,2),VAL(j),ADD_VALUES,IERR)
          endif
          if(COL+1.ge.row_y(j,2))then
            CALL MatSetValue(KR,row_y(j,2),COL+1,VAL(j),ADD_VALUES,IERR)
          else      
            CALL MatSetValue(KR,COL+1,row_y(j,2),VAL(j),ADD_VALUES,IERR)
          endif
        endif
 13   continue
      endif

      return     
      end
