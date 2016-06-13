      subroutine solp_noconf(nel,ngl,mm,nn,ng,z,vol,dens,velsq,
     &                       eigen,iflag,componentes,p_noconf,z_noconf)
     
      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"

      PetscScalar    eigen
      
      dimension mm(4,*),nn(4,*),ng(4,*),z(3,*),vol(*),dens(*),velsq(*),
     &          iflag(*),componentes(4,*),p_noconf(*),
     &          xg(4),yg(4),zg(4),z_noconf(3,*)

      integer cn(4,4)

      cn(1,1)=2; cn(1,2)=3; cn(1,3)=4; cn(1,4)=1
      cn(2,1)=3; cn(2,2)=4; cn(2,3)=1; cn(2,4)=2
      cn(3,1)=4; cn(3,2)=1; cn(3,3)=2; cn(3,4)=3
      cn(4,1)=1; cn(4,2)=2; cn(4,3)=3; cn(4,4)=4

      pa = 0.5854105
      pb = 0.1381965

      do 10 i=1,ngl
        iflag(i) = 0
 10   continue

      do 100 k=1,nel
        
        a=componentes(1,k)
        b=componentes(2,k)
        c=componentes(3,k)
        d=componentes(4,k)
        
        do 110 i=1,4
          xg(i) = pa * z(1,mm(i,k)) + pb * ( z(1,mm(1+mod(i,4),k)) +
     &            z(1,mm(1+mod(i+1,4),k)) + z(1,mm(1+mod(i+2,4),k)) )
          yg(i) = pa * z(2,mm(i,k)) + pb * ( z(2,mm(1+mod(i,4),k)) +
     &            z(2,mm(1+mod(i+1,4),k)) + z(2,mm(1+mod(i+2,4),k)) )
          zg(i) = pa * z(3,mm(i,k)) + pb * ( z(3,mm(1+mod(i,4),k)) +
     &            z(3,mm(1+mod(i+1,4),k)) + z(3,mm(1+mod(i+2,4),k)) )
 110    continue
        
        do 120 j=1,4
          if(iflag(nn(j,k))==0) then
            
            iflag(nn(j,k))=1
            
            uh_fi=0.0
            do 130 i=1,4
              uh_fi = uh_fi + (a+d*xg(i))*(xg(i)-z(1,mm(cn(j,4),k))) +
     &                        (b+d*yg(i))*(yg(i)-z(2,mm(cn(j,4),k))) +
     &                        (c+d*zg(i))*(zg(i)-z(3,mm(cn(j,4),k)))
 130        continue
            
            p_noconf(nn(j,k)) = - dens(1)*velsq(1)**2*3*d
     &                  + dens(1)*eigen*ng(j,k)*uh_fi/(4*3)

            do 140 i=1,3
              z_noconf(i,nn(j,k)) = ( z(i,mm(cn(j,1),k)) + 
     &                    z(i,mm(cn(j,2),k)) + z(i,mm(cn(j,3),k)) ) /3
 140        continue

          endif
 120    continue
 100  continue
      
      return
      end
