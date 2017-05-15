      subroutine ensflpl_presion(KR,M,row,col,nu,csi_p,eta_p,
     &           csi_f,eta_f,area,dirp,thin)

      implicit double precision (a-h,o-z)
      common/coeffl/densf,velsq,ndsd,isd
      dimension isd(30),densf(30),velsq(30)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"

      double precision nu(*),normal,csi_p(3),eta_p(3),csi_f(3),eta_f(3),
     &                 aux_p(3),aux_f(3),thin
      integer dirp,ndsdf

      PetscInt    col(3),row(3)
      PetscScalar val(3,3)
      Mat         KR,M

      do 1 i=1,3
        do 2 j=1,3
          val(i,j)=0.d0
 2      continue
 1    continue

      normal = 1
      if(dirp.eq.1) normal=nu(1)
      if(dirp.eq.2) normal=nu(2)
      if(dirp.eq.3) normal=nu(3)
  
      do 10 k=1,3
        aux_p(1) = 1 - csi_p(k) - eta_p(k)
        aux_p(2) = csi_p(k)
        aux_p(3) = eta_p(k)
        aux_f(1) = 1 - csi_f(k) - eta_f(k)
        aux_f(2) = csi_f(k)
        aux_f(3) = eta_f(k)
        do 11 i=1,3
          do 12 j=1,3
            val(i,j) = val(i,j) + aux_f(i)*aux_p(j)
 12       continue
 11     continue
 10   continue

      coef=1 !(velsq(1)*velsq(1))
      do 13 i=1,3
        do 14 j=1,3
          val(i,j) = val(i,j) * normal * coef *area/3.d0
          CALL MatSetValue(KR,col(j),row(i),-val(i,j),ADD_VALUES,IERR)
 14     continue
 13   continue
      do 15 i=1,3
        do 16 j=1,3
!          val(i,j) = val(i,j) * densf(1)  
          CALL MatSetValue(M ,row(i),col(j),+val(i,j),ADD_VALUES,IERR)
 16     continue
 15   continue

      do 23 i=1,3
        do 24 j=1,3
 24     continue
 23   continue


      return
      end
