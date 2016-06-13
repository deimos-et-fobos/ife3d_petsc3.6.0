!     ------------------------------------------------------------------
      subroutine metis_perm(iat_v,n_iat,nedge_grafo,nverp,narp,nvers,
     &            nnodf,nvercos,ncacofp,ncacofs,cnump,ia_perm,ia_iperm,
     &            jat_v,perm,iperm,vec_perm,vec_iperm)
!     ------------------------------------------------------------------
!
      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
      parameter (max_vec=1000)
      dimension iat_v(*),jat_v(*),ia_perm(*),ia_iperm(*)
      integer cnump(*),perm(*),iperm(*),vec_perm(*),vec_iperm(*)
!
!*    \param perm is an array of size nvtxs such that if A and A' a
!*           the original and permuted matrices, then A'[i] = A[perm[i]].
!*    \param iperm is an array of size nvtxs such that if A and A' are
!*           the original and permuted matrices, then A[i] = A'[iperm[i]].
!      write(*,*) '---> METIS_NodeND_F2C'
!      call METIS_NodeND_F2C(n_iat, iat_v, jat_v, perm, iperm)
      write(*,*) '---> NDMETIS_F2C started'
      call ndmetis_f2c(n_iat,nedge_grafo,iat_v,jat_v, perm, iperm)
      write(*,*) '---> NDMETIS_F2C finished'
!
      do 112 i=1,n_iat          ! start with 1 again
        perm(i)=perm(i)+1
        iperm(i)=iperm(i)+1
 112  continue      
!
!     VECTOR IA_PERM AUXILIAR PARA LA PERMUTACION 
!
      ia_perm(1)=1
!*    PLACA
      n_i=1;n_f=nverp+narp;
      do 31 i=n_i,n_f
        ia_perm(i+1)=cnump(i+1)+1
 31   continue
!*    SOLIDO
      n_i=n_f+1;n_f=n_f+nvers;
      do 32 i=n_i,n_f
        ia_perm(i+1)=ia_perm(i)+3
 32   continue
!*    FLUIDO
      n_i=n_f+1;n_f=n_f+nnodf;
      do 33 i=n_i,n_f
        ia_perm(i+1)=ia_perm(i)+1
 33   continue
!*    INT. SOLIDO-PLACA
      n_i=n_f+1;n_f=n_f+nvercos;
      do 34 i=n_i,n_f
        ia_perm(i+1)=ia_perm(i)+3
 34   continue
!*    INT. FLUIDO-PLACA
      n_i=n_f+1;n_f=n_f+ncacofp;
      do 35 i=n_i,n_f
        ia_perm(i+1)=ia_perm(i)+1
 35   continue
!*    INT. FLUIDO-SOLIDO
      n_i=n_f+1;n_f=n_f+ncacofs;
      do 36 i=n_i,n_f
        ia_perm(i+1)=ia_perm(i)+1
 36   continue
!
!     VECTOR IA_IPERM AUXILIAR PARA LA PERMUTACION 
!
      ia_iperm(1)=1
      do 40 i=1,n_iat
        ia_iperm(i+1)=ia_iperm(i)+ia_perm(perm(i)+1)-ia_perm(perm(i))
 40   continue
!
!     VEC_PERM y VEC_IPERM
!
      do 50 i=1,n_iat
        jaux=0
        do 51 j=ia_perm(i),ia_perm(i+1)-1
          vec_iperm(j)=ia_iperm(iperm(i))+jaux-1   ! start with 0
          jaux=jaux+1
 51     continue
 50   continue
      do 60 i=1,ia_perm(n_iat+1)-1
        vec_perm(vec_iperm(i)+1)= i-1     ! start with 0
 60   continue
!
      return
      end

!     ------------------------------------------------------------------
      subroutine metis_perm_presion(iat_v,n_iat,nedge_grafo,nverp,narp,
     &            nvers,nverf,nvercos,cnump,ia_perm,ia_iperm,jat_v,perm,
     &            iperm,vec_perm,vec_iperm)
!     ------------------------------------------------------------------
!
      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
      parameter (max_vec=1000)
      dimension iat_v(*),jat_v(*),ia_perm(*),ia_iperm(*)
      integer cnump(*),perm(*),iperm(*),vec_perm(*),vec_iperm(*)
!
!*    \param perm is an array of size nvtxs such that if A and A' a
!*           the original and permuted matrices, then A'[i] = A[perm[i]].
!*    \param iperm is an array of size nvtxs such that if A and A' are
!*           the original and permuted matrices, then A[i] = A'[iperm[i]].
!      write(*,*) '---> METIS_NodeND_F2C'
!      call METIS_NodeND_F2C(n_iat, iat_v, jat_v, perm, iperm)
      write(*,*) '---> NDMETIS_F2C started'
      call ndmetis_f2c(n_iat,nedge_grafo,iat_v,jat_v, perm, iperm)
      write(*,*) '---> NDMETIS_F2C finished'
!
      do 112 i=1,n_iat          ! start with 1 again
        perm(i)=perm(i)+1
        iperm(i)=iperm(i)+1
 112  continue      
!
!     VECTOR IA_PERM AUXILIAR PARA LA PERMUTACION 
!
      ia_perm(1)=1
!*    PLACA
      n_i=1;n_f=nverp+narp;
      do 31 i=n_i,n_f
        ia_perm(i+1)=cnump(i+1)+1
 31   continue
!*    SOLIDO
      n_i=n_f+1;n_f=n_f+nvers;
      do 32 i=n_i,n_f
        ia_perm(i+1)=ia_perm(i)+3
 32   continue
!*    FLUIDO
      n_i=n_f+1;n_f=n_f+nverf;
      do 33 i=n_i,n_f
        ia_perm(i+1)=ia_perm(i)+1
 33   continue
!*    INT. SOLIDO-PLACA
      n_i=n_f+1;n_f=n_f+nvercos;
      do 34 i=n_i,n_f
        ia_perm(i+1)=ia_perm(i)+3
 34   continue
!
!     VECTOR IA_IPERM AUXILIAR PARA LA PERMUTACION 
!
      ia_iperm(1)=1
      do 40 i=1,n_iat
        ia_iperm(i+1)=ia_iperm(i)+ia_perm(perm(i)+1)-ia_perm(perm(i))
 40   continue
!
!     VEC_PERM y VEC_IPERM
!
      do 50 i=1,n_iat
        jaux=0
        do 51 j=ia_perm(i),ia_perm(i+1)-1
          vec_iperm(j)=ia_iperm(iperm(i))+jaux-1   ! start with 0
          jaux=jaux+1
 51     continue
 50   continue
      do 60 i=1,ia_perm(n_iat+1)-1
        vec_perm(vec_iperm(i)+1)= i-1     ! start with 0
 60   continue
!
      return
      end

!     ------------------------------------------------------------------
      subroutine metis_perm_g(n_iat,nedge_grafo,iat_v,jat_v,
     &                        vec_perm,vec_iperm)
!     ------------------------------------------------------------------
!
      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
      dimension iat_v(*),jat_v(*)
      integer vec_perm(*),vec_iperm(*)
!
!*    \param perm is an array of size nvtxs such that if A and A' a
!*           the original and permuted matrices, then A'[i] = A[perm[i]].
!*    \param iperm is an array of size nvtxs such that if A and A' are
!*           the original and permuted matrices, then A[i] = A'[iperm[i]].
      write(*,*) '---> NDMETIS_F2C started'
      call ndmetis_f2c(n_iat,nedge_grafo,iat_v,jat_v,vec_perm,vec_iperm)
      write(*,*) '---> NDMETIS_F2C finished'
!
      return
      end
