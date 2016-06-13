      subroutine ensflpl_presion2(KR,M,row,col,nu,csi_p,eta_p,
     &           csi_f,eta_f,area,dirp,thin)

      implicit double precision (a-h,o-z)
      common/coeffl/densf,velsq,ndsd,isd
      dimension isd(30),densf(30),velsq(30)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"

      double precision nu(*),normal,csi_p(3),eta_p(3),csi_f(3),eta_f(3),
     &                 aux_p(3),aux_f(3),thin
      integer dirp,ndsdf

      PetscInt    col(3),row(3)
      PetscScalar val(3,3)
      Mat         KR,M

      do 1 i=1,3
        val(1,i)=0.d0
 1    continue

      normal = 1
      if(dirp.eq.1) normal=nu(1)
      if(dirp.eq.2) normal=nu(2)
      if(dirp.eq.3) normal=nu(3)
  
      aux_p(1) = 1 - csi_p(1) - eta_p(1)
      aux_p(2) = csi_p(1)
      aux_p(3) = eta_p(1)
      do 11 i=1,3
        if(aux_p(i) .gt. 0.5)then
          val(1,i) = 1
        else 
          val(1,i) = 0   
        endif
 11   continue

      coef=1
      do 13 i=1,3
        val(1,i) = val(1,i) * normal
        CALL MatSetValue(M ,row(1),col(i),+val(1,i),ADD_VALUES,IERR)
        CALL MatSetValue(KR,col(i),row(1),-val(1,i),ADD_VALUES,IERR)
 13   continue
      do 15 i=1,3
        do 16 j=1,3
 16     continue
 15   continue

      do 23 i=1,3
        do 24 j=1,3
 24     continue
 23   continue
      do 25 i=1,3
        do 26 j=1,3
 26     continue
 25   continue


      return
      end
