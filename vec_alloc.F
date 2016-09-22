!     ----------------------------------------------------------------
      subroutine allocKR(dof,iat_v,jat_v,vec_iperm,r_min,r_max,
     &                   alloc_d,alloc_o)
!     ------------------------------------------------------------------
!
      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      dimension iat_v(*),jat_v(*)
      integer vec_iperm(*),dof
      PetscInt  alloc_d(1),alloc_o(1)
      PetscInt  r_min,r_max
!
      do 1000 i=1,dof
        nrow = i 
        nrow = vec_iperm(nrow) + 1
        if(nrow.gt.r_min .and. nrow.le.r_max)then
          alloc_d(nrow-r_min) = alloc_d(nrow-r_min) + 2   ! 1 + por si mismo 
          do 1100 j=iat_v(i)+1,iat_v(i+1)
            ncol = jat_v(j) + 1
            ncol = vec_iperm(ncol) + 1
            if(ncol.gt.nrow)then
              if(ncol.gt.r_min .and. ncol.le.r_max)then
                alloc_d(nrow-r_min) = alloc_d(nrow-r_min) + 1
              else
                alloc_o(nrow-r_min) = alloc_o(nrow-r_min) + 1
              endif
            endif
 1100     continue
        endif
 1000 continue
!
      return
      end

!     ------------------------------------------------------------------
      subroutine allocM(dof,iat_v,jat_v,vec_iperm,r_min,r_max,
     &                  nglp,ngls,nglf,alloc_d,alloc_o)
!     ------------------------------------------------------------------
!
      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      dimension iat_v(*),jat_v(*)
      integer vec_iperm(*),dof
      PetscInt  alloc_d(1),alloc_o(1)
      PetscInt  r_min,r_max
!
      do 1000 i=1,dof
        nrow = i 
        if(nrow.le.nglp)then
          irow=1
        elseif(nrow.le.(nglp+ngls))then
          irow=2
        elseif(nrow.le.(nglp+ngls+nglf))then
          irow=3
        else
          goto 333
        endif
        nrow = vec_iperm(nrow) + 1
        if(nrow.gt.r_min .and. nrow.le.r_max)then
          alloc_d(nrow-r_min) = alloc_d(nrow-r_min) + 1   ! 1 + por si mismo 
          do 1100 j=iat_v(i)+1,iat_v(i+1)
            ncol = jat_v(j) + 1
            if(ncol.le.nglp)then
              icol=1
            elseif(ncol.le.(nglp+ngls))then
              icol=2
            elseif(ncol.le.(nglp+ngls+nglf))then
              icol=3
            else
              goto 333
            endif
            if(irow.ne.icol) goto 333
            ncol = vec_iperm(ncol) + 1
            if(ncol.gt.nrow)then
              if(ncol.gt.r_min .and. ncol.le.r_max)then
                alloc_d(nrow-r_min) = alloc_d(nrow-r_min) + 1
              else
                alloc_o(nrow-r_min) = alloc_o(nrow-r_min) + 1
              endif
            endif
 1100     continue
        endif
 333    continue       
 1000 continue
!
      return
      end

!     ----------------------------------------------------------------
      subroutine allocKR_presion(dof,iat_v,jat_v,vec_iperm,r_min,r_max,
     &                           alloc_d,alloc_o)
!     ------------------------------------------------------------------
!
      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      dimension iat_v(*),jat_v(*)
      integer vec_iperm(*),dof
      PetscInt  alloc_d(1),alloc_o(1)
      PetscInt  r_min,r_max
!
      do 1000 i=1,dof
        nrow = i 
        nrow = vec_iperm(nrow) + 1
        if(nrow.gt.r_min .and. nrow.le.r_max)then
          alloc_d(nrow-r_min) = alloc_d(nrow-r_min) + 1   ! 1 + por si mismo 
          do 1100 j=iat_v(i)+1,iat_v(i+1)
            ncol = jat_v(j) + 1
            ncol = vec_iperm(ncol) + 1
            if(ncol.gt.r_min .and. ncol.le.r_max)then
              alloc_d(nrow-r_min) = alloc_d(nrow-r_min) + 1
            else
              alloc_o(nrow-r_min) = alloc_o(nrow-r_min) + 1
            endif
 1100     continue
        endif
 1000 continue
!
      return
      end

!     ------------------------------------------------------------------
      subroutine allocM_presion(dof,iat_v,jat_v,vec_iperm,r_min,r_max,
     &                          nglp,ngls,nglf,alloc_d,alloc_o)
!     ------------------------------------------------------------------
!
      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      dimension iat_v(*),jat_v(*)
      integer vec_iperm(*),dof
      PetscInt  alloc_d(1),alloc_o(1)
      PetscInt  r_min,r_max
!
      do 1000 i=1,dof
        nrow = i 
        if(nrow.le.(nglp+ngls+nglf))then
          irow=1
        else
          goto 333
        endif
        nrow = vec_iperm(nrow) + 1
        if(nrow.gt.r_min .and. nrow.le.r_max)then
          alloc_d(nrow-r_min) = alloc_d(nrow-r_min) + 1   ! 1 + por si mismo 
          do 1100 j=iat_v(i)+1,iat_v(i+1)
            ncol = jat_v(j) + 1
            if(ncol.le.(nglp+ngls+nglf))then
              icol=1
            else
              goto 333
            endif
            if(irow.ne.icol) goto 333
            ncol = vec_iperm(ncol) + 1
            if(ncol.gt.r_min .and. ncol.le.r_max)then
              alloc_d(nrow-r_min) = alloc_d(nrow-r_min) + 1
            else
              alloc_o(nrow-r_min) = alloc_o(nrow-r_min) + 1
            endif
 1100     continue
        endif
 333    continue       
 1000 continue
!
      return
      end

