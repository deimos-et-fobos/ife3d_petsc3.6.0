      subroutine blomap(KR,cnum,nn,nrv,nra,nel,vec_perm)
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

      dimension nn(6,*),nrv(3,*),nra(3,*)
      integer cnum(*),vec_perm(*)

      PetscScalar V
      PetscInt    IGL
      Mat         KR

      V=1.D50

      do 1 k=1,nel
         do 2 j=1,3
            ! Nodo empotrado: No hay desplazamientos, ni rotaciones 
            ! Ux, Uy, Uz, Rx, Ry = 0
            if(nrv(j,k).eq. 1) then
               do 3 i=1,5
                  igl=vec_perm(cnum(nn(j,k))+i)
                  CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
 3             continue
            endif
            ! Lado empotrado: No hay rotaciones
            if(nra(j,k).eq. 1) then
               igl=vec_perm(cnum(nn(j+3,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Nodo simplemente apoyado en x:  No hay desplazamiento transversal, ni rotación en x 
            ! Uz, Rx = 0
            if(nrv(j,k).eq. 2) then
               igl=vec_perm(cnum(nn(j,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+2)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Lado simplemente apoyado en x:  No hay rotación en x 
            if(nra(j,k).eq. 2) then
               igl=vec_perm(cnum(nn(j+3,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Nodo simplemente apoyado en y:  No hay desplazamiento transversal, ni rotación en y 
            ! Uz, Ry = 0
            if(nrv(j,k).eq. 3) then
               igl=vec_perm(cnum(nn(j,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+3)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Lado simplemente apoyado en y:  No hay rotación en y 
            if(nra(j,k).eq. 3) then
               igl=vec_perm(cnum(nn(j+3,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Simetria respecto al eje y: No hay desplazamiento ni rotacion en direccion x
            ! Ux, Rx = 0
            if(nrv(j,k).eq. 4) then
               igl=vec_perm(cnum(nn(j,k))+2)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+4)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            if(nra(j,k).eq. 4) then
               igl=vec_perm(cnum(nn(j+3,k))+1)
!               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Simetria respecto al eje x: No hay desplazamiento ni rotacion en direccion y
            ! Uy, Ry = 0
            if(nrv(j,k).eq. 5) then
               igl=vec_perm(cnum(nn(j,k))+3)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+5)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            if(nra(j,k).eq. 5) then
               igl=vec_perm(cnum(nn(j+3,k))+1)
!               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Antisimetria respecto al eje y: No hay desplazamiento ni rotacion en direccion y
            !                                 ni desplazamiento transversal
            ! Uy, Uz, Ry = 0
            if(nrv(j,k).eq. 6) then
               igl=vec_perm(cnum(nn(j,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+3)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+5)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            if(nra(j,k).eq. 6) then
               igl=vec_perm(cnum(nn(j+3,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Antisimetria respecto al eje x: No hay desplazamiento ni rotacion en direccion x
            !                                 ni desplazamiento transversal
            ! Ux, Uz, Rx = 0
            if(nrv(j,k).eq. 7) then
               igl=vec_perm(cnum(nn(j,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+2)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+4)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            if(nra(j,k).eq. 7) then
               igl=vec_perm(cnum(nn(j+3,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Simplemente apoyado sin restringir rotaciones            
            ! Uz = 0
            if(nrv(j,k).eq. 8) then
               igl=vec_perm(cnum(nn(j,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Uz, Rx, Ry = 0
            if(nrv(j,k).eq. 9) then
               igl=vec_perm(cnum(nn(j,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+2)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+3)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            if(nra(j,k).eq. 9) then
               igl=vec_perm(cnum(nn(j+3,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Ux, Uz, Rx, Ry = 0
            if(nrv(j,k).eq. 10) then
               igl=vec_perm(cnum(nn(j,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+2)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+3)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+4)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            if(nra(j,k).eq. 10) then
               igl=vec_perm(cnum(nn(j+3,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Uy, Uz, Rx, Ry = 0
            if(nrv(j,k).eq. 11) then
               igl=vec_perm(cnum(nn(j,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+2)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+3)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+5)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            if(nra(j,k).eq. 11) then
               igl=vec_perm(cnum(nn(j+3,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Ux, Uy, Rx, Ry = 0
            if(nrv(j,k).eq. 12) then
               igl=vec_perm(cnum(nn(j,k))+2)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+3)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+4)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
               igl=vec_perm(cnum(nn(j,k))+5)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            if(nra(j,k).eq. 12) then
               igl=vec_perm(cnum(nn(j+3,k))+1)
               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            endif
            ! Sin desplazamiento transversal
!            igl=vec_perm(cnum(nn(j,k))+1)
!            CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
            ! Sin desplazamiento InPlane
!            do 4 i=4,5
!               igl=vec_perm(cnum(nn(j,k))+i)
!               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
! 4          continue
            ! Sin rotaciones
!            do 5 i=1,3
!               igl=vec_perm(cnum(nn(j,k))+i)
!               CALL MatSetValue(KR,IGL,IGL,V,INSERT_VALUES,IERR)
! 5          continue
 2       continue
 1    continue

      return
      end
