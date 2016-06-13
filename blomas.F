      subroutine blomas(KR,mm,nrv,nel,NGLP,vec_perm)
***************************************************************
*                       subrutina blomat                      *
***************************************************************
*     objetivo:                                               *
*     ========  bloqueo de las condiciones de contorno.       *
*               solo se efectua bloqueo de la matriz c       *
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

      dimension mm(4,*),nrv(4,*)
      integer vec_perm(*)

      Mat         KR
      PetscScalar V
      PetscInt    IGL

      V=1.D50

      do 1 k=1,nel
         do 2 j=1,4
            ! Nodo rígido: No hay desplazamiento
            if(nrv(j,k).eq. 1) then
               do 3 i=1,3
                  igl=vec_perm(3*(mm(j,k)-1)+i + NGLP)
                  CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
 3             continue
            endif
            ! Nodo sin desplazamiento de x
            if(nrv(j,k).eq. 2) then
               igl=vec_perm(3*(mm(j,k)-1)+1 + NGLP)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Nodo sin desplazamiento de y
            if(nrv(j,k).eq. 3) then
               igl=vec_perm(3*(mm(j,k)-1)+2 + NGLP)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Nodo sin desplazamiento de z
            if(nrv(j,k).eq. 4) then
               igl=vec_perm(3*(mm(j,k)-1)+3 + NGLP)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Nodo sin desplazamiento en xy
            if(nrv(j,k).eq. 5) then
               igl=vec_perm(3*(mm(j,k)-1)+1 + NGLP)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               IGL = IGL + 1
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Nodo sin desplazamiento en yz
            if(nrv(j,k).eq. 6) then
               igl=vec_perm(3*(mm(j,k)-1)+2 + NGLP)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               IGL = IGL + 1
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Nodo sin desplazamiento en zx
            if(nrv(j,k).eq. 7) then
               igl=vec_perm(3*(mm(j,k)-1)+3 + NGLP)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               IGL = IGL - 2
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
!            if(nrv(j,k).ge.101.and.nrv(j,k).le.121) then
!               igl=vec_perm(3*(mm(j,k)-1)+3 + NGLP)
!               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
!            endif
 2       continue
 1    continue

      return
      end
