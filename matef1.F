      subroutine matef1(Me,auxM,p,vol,dens,area,sigm)
      
      implicit double precision(a-h,o-z)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"

      dimension area(4),p(3,4)
      integer sigm(4),fact
      
      PetscScalar    Me(4,4)
      
      do 10 i=1,4
         do 20 j=1,4
            Me(i,j)=0.d0
 20      continue
 10   continue
 
      do 40 j=1,4
         do 41 k=1,4
            do 42 l=1,4
               do 43 mm=1,4
                  if (l.eq.mm) then
                     fact=2
                  else
                     fact=1
                  endif
                  subt=0.d0
                  do 44 i=1,3
                     subt=subt+(p(i,l)-p(i,j))*(p(i,mm)-p(i,k))
 44               continue
                  Me(j,k)=Me(j,k)+fact*subt                     
 43            continue                  
 42         continue
            Me(j,k)=Me(j,k)*sigm(j)*sigm(k)*area(j)*area(k)
     &                                    *dens*6/(9*vol*120.d0)
 41      continue
 40   continue
 
      return
      end
