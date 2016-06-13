      subroutine ensflso_presion(KR,M,row,col,nu,csi_s,eta_s,chi_s,
     &           csi_f,eta_f,area)

      implicit double precision (a-h,o-z)
      common/coeffl/densf,velsq,ndsd,isd
      dimension isd(30),densf(30),velsq(30)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"

      double precision nu(*),csi_s(3),eta_s(3),chi_s(3),
     &                 csi_f(3),eta_f(3),aux_s(4),aux_f(3)
      integer ndsdf

      PetscInt    col(4,3),row(3)
      PetscScalar val(3,4,3)
      Mat         KR,M

      do 1 i=1,3
        do 2 j=1,4
          do 3 l=1,3
            val(i,j,l)=0.d0
 3        continue
 2      continue
 1    continue
  
      do 10 k=1,3
        aux_s(1) = 1 - csi_s(k) - eta_s(k) - chi_s(k)
        aux_s(2) = csi_s(k)
        aux_s(3) = eta_s(k)
        aux_s(4) = chi_s(k)
        aux_f(1) = 1 - csi_f(k) - eta_f(k)
        aux_f(2) = csi_f(k)
        aux_f(3) = eta_f(k)
        do 11 i=1,3
          do 12 j=1,4
            do 13 l=1,3
              val(i,j,l) = val(i,j,l) + aux_f(i)*aux_s(j)*nu(l)
 13         continue
 12       continue
 11     continue
 10   continue

      coef=1 !densf(1)
      do 21 i=1,3
      do 22 j=1,4
      do 23 l=1,3
        val(i,j,l) = val(i,j,l) *area/3.d0
        CALL MatSetValue(M ,row(i),col(j,l),+val(i,j,l),ADD_VALUES,IERR)
        CALL MatSetValue(KR,col(j,l),row(i),-val(i,j,l),ADD_VALUES,IERR)
 23   continue
 22   continue
 21   continue

      return
      end
