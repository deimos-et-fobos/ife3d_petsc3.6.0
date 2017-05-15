!-----------------------------------------------------------------------
!	Function WriteRee - write restart file

!	Its function is to write the values of a sequential PETSc vector 
!	to a file.
!	Only one processor has to call it.
!	Also writes "headings and footnotes".

!	Parameters:
!	nev: eigenvalue number of the mode 
!	eigen: eigenvalue.      (Heading, info)
!	IOvec: vector to save
!	nglp: # dof in the plate
!	ngls: # dof in the solid
!	nglf: # dof in the fluid
!	ncacof: # dof in the contact surface (FLUID) (P)
!	nvercos: # dof in the contact surface (SOLID) (P)
!-----------------------------------------------------------------------

      subroutine getsol(IOvecScan,i_x,up,us,uf,pres,pressp,
     &                  nglp,ngls,nglf,ncacof,nvercos,vec_perm)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"

      double precision up(*),us(*),uf(*),pres(*),pressp(*)
      integer nglp,ngls,nglf,ncacof,nvercos
      integer ini(5),fin(5),vec_perm(*)

      PetscScalar IOvecScan(*)
      PetscOffset i_x
      
      ini(1) = 1
      ini(2) = ini(1) + nglp
      ini(3) = ini(2) + ngls
      ini(4) = ini(3) + nglf
      ini(5) = ini(4) + 3*nvercos  
      fin(1) = nglp
      fin(2) = fin(1) + ngls
      fin(3) = fin(2) + nglf
      fin(4) = fin(3) + 3*nvercos
      fin(5) = fin(4) + ncacof
      
      do 11 i=ini(1),fin(1)
         up(i-ini(1)+1) = IOvecScan(i_x + vec_perm(i) +1)
 11   continue
      do 12 i=ini(2),fin(2)
         us(i-ini(2)+1) = IOvecScan(i_x + vec_perm(i) +1)
 12   continue
      do 13 i=ini(3),fin(3)
         uf(i-ini(3)+1) = IOvecScan(i_x + vec_perm(i) +1)
 13   continue
      do 14 i=ini(4),fin(4)
         pressp(i-ini(4)+1) = IOvecScan(i_x + vec_perm(i) +1)
 14   continue
      do 15 i=ini(5),fin(5)
         pres(i-ini(5)+1) = IOvecScan(i_x + vec_perm(i) +1)
 15   continue

      return
      end
