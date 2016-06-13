       subroutine coismos(nvercos,ivercos,ielps,dirp,NGLP,DOF,
     &                    KR,ncacof,vec_perm)

      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      common/p_fluido/varf

      dimension ivercos(*),ielps(*)
      integer dirp(*),vec_perm(*)
      character*20 varf

      PetscInt    COL,ROWS(3),DOF
      PetscScalar VAL
      Mat         KR

      do 1 i=1,nvercos                    ! loop sobre los vertices de contacto
        if(varf(1:7).eq.'presion')then
          COL = vec_perm(DOF - 3*nvercos + 3*(i-1) + 1)  !(LOS INDICES EN PETSC VAN DESDE 0)
        else
          COL = vec_perm(DOF - ncacof - 3*nvercos + 3*(i-1) + 1)  !(LOS INDICES EN PETSC VAN DESDE 0)
        endif        
        ns = ivercos(i)
        k = ielps(i)

        ROWS(1)= vec_perm( 3*(ns-1) + NGLP + 1 )  ! desp x
        ROWS(2)= vec_perm( 3*(ns-1) + NGLP + 2 )  ! desp y     
        ROWS(3)= vec_perm( 3*(ns-1) + NGLP + 3 )  ! desp z

        VAL=-1
        
        if(varf(1:7).eq.'presion')then
          CALL MATSETVALUE(KR,ROWS(1),COL,VAL,ADD_VALUES,IERR)
          CALL MATSETVALUE(KR,COL,ROWS(1),VAL,ADD_VALUES,IERR)
          CALL MATSETVALUE(KR,ROWS(2),COL+1,VAL,ADD_VALUES,IERR)
          CALL MATSETVALUE(KR,COL+1,ROWS(2),VAL,ADD_VALUES,IERR)
          CALL MATSETVALUE(KR,ROWS(3),COL+2,VAL,ADD_VALUES,IERR)
          CALL MATSETVALUE(KR,COL+2,ROWS(3),VAL,ADD_VALUES,IERR)
          VAL=-1.d-8
!          CALL MATSETVALUE(KR,COL,COL,VAL,ADD_VALUES,IERR)
!          CALL MATSETVALUE(KR,COL+1,COL+1,VAL,ADD_VALUES,IERR)
!          CALL MATSETVALUE(KR,COL+2,COL+2,VAL,ADD_VALUES,IERR)
        else
          if(COL.ge.ROWS(1))then
            CALL MATSETVALUE(KR,ROWS(1),COL,VAL,ADD_VALUES,IERR)
          else
            CALL MATSETVALUE(KR,COL,ROWS(1),VAL,ADD_VALUES,IERR)
          endif  
          if(COL+1.ge.ROWS(2))then
            CALL MATSETVALUE(KR,ROWS(2),COL+1,VAL,ADD_VALUES,IERR)
          else
            CALL MATSETVALUE(KR,COL+1,ROWS(2),VAL,ADD_VALUES,IERR)
          endif  
          if(COL+2.ge.ROWS(3))then
            CALL MATSETVALUE(KR,ROWS(3),COL+2,VAL,ADD_VALUES,IERR)
          else
            CALL MATSETVALUE(KR,COL+2,ROWS(3),VAL,ADD_VALUES,IERR)
          endif  
        endif
 1    continue

      return
      end
