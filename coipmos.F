      subroutine coipmos(nvercos,ivercos,ielps,indlp,zs,mmp,nnp,zp,cnum,
     &                   dirp,smp,DOF,KR,ncacof,vec_perm)

      implicit double precision (a-h,o-z) 
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"
      common/p_fluido/varf

      dimension   mmp(3,*),nnp(6,*),zp(2,*),zs(3,*),smp(*),
     &            ivercos(*),ielps(*),indlp(*),xvp(2),yvp(2),zvp(2)
      integer cnum(*),dirp(*),vec_perm(*)
      character*20 varf
      
      PetscInt    COL,DOF,row_x(2,2)
      PetscInt    row_y(2,2),row_z(2,2)
      Mat         KR

      tol=0.000001

      do 100 j=1,nvercos            ! loop sobre los vertices de contacto

        if(varf(1:7).eq.'presion')then
          COL = vec_perm(DOF - 3*nvercos + 3*(j-1) + 1)  !(LOS INDICES EN PETSC VAN DESDE 0)
        else
          COL = vec_perm(DOF - ncacof - 3*nvercos + 3*(j-1) + 1)  !(LOS INDICES EN PETSC VAN DESDE 0)
        endif        

        ns=ivercos(j)               ! nodo del solido 
        k=ielps(j)                  ! elemento del placa
        lp=indlp(j)                 ! lado de la placa
      
        ! coordenada del vertice del solido
        xvs=zs(1,ns)
        yvs=zs(2,ns)
        zvs=zs(3,ns)

        ! coordenadas de los vertices del placa
        if(dirp(k).eq.1)then
          xvp(1)=smp(k)
          xvp(2)=smp(k)
          yvp(1)=zp(1,mmp(lp,k))
          yvp(2)=zp(1,mmp(1+mod(lp,3),k))
          zvp(1)=zp(2,mmp(lp,k))
          zvp(2)=zp(2,mmp(1+mod(lp,3),k))
          ! desp x !(LOS INDICES EN PETSC VAN DESDE 0) 
          row_x(1,1)=vec_perm(cnum(nnp(lp,k))+1) 
          row_x(2,1)=vec_perm(cnum(nnp(1+mod(lp,3),k))+1) 
          row_x(1,2)=0
          row_x(2,2)=0
          ! desp y
          row_y(1,1)=row_x(1,1)+1
          row_y(2,1)=row_x(2,1)+1
          row_y(1,2)=row_x(1,1)+3
          row_y(2,2)=row_x(2,1)+3
          ! desp z
          row_z(1,1)=row_x(1,1)+2
          row_z(2,1)=row_x(2,1)+2
          row_z(1,2)=row_x(1,1)+4
          row_z(2,2)=row_x(2,1)+4
          ! valor de las funciones base en el vertice del sólido
          hz=xvs-smp(k)
          if(dabs(yvp(1)-yvp(2)).le.tol)then
            psi=(zvs-zvp(1))/(zvp(2)-zvp(1))
          else
            psi=(yvs-yvp(1))/(yvp(2)-yvp(1))
          endif
        endif      


        if(dirp(k).eq.2)then
          xvp(1)=zp(2,mmp(lp,k))
          xvp(2)=zp(2,mmp(1+mod(lp,3),k))
          yvp(1)=smp(k)
          yvp(2)=smp(k)
          zvp(1)=zp(1,mmp(lp,k))
          zvp(2)=zp(1,mmp(1+mod(lp,3),k))
          ! desp y !(LOS INDICES EN PETSC VAN DESDE 0)   
          row_y(1,1)=vec_perm(cnum(nnp(lp,k))+1) 
          row_y(2,1)=vec_perm(cnum(nnp(1+mod(lp,3),k))+1) 
          row_y(1,2)=0
          row_y(2,2)=0
          ! desp z
          row_z(1,1)=row_y(1,1)+1
          row_z(2,1)=row_y(2,1)+1
          row_z(1,2)=row_y(1,1)+3
          row_z(2,2)=row_y(2,1)+3
          ! desp x
          row_x(1,1)=row_y(1,1)+2
          row_x(2,1)=row_y(2,1)+2
          row_x(1,2)=row_y(1,1)+4
          row_x(2,2)=row_y(2,1)+4
          ! valor de las funciones base en el vertice del sólido
          hz=yvs-smp(k)
          if(dabs(xvp(1)-xvp(2)).le.tol)then
            psi=(zvs-zvp(1))/(zvp(2)-zvp(1))
          else
            psi=(xvs-xvp(1))/(xvp(2)-xvp(1))
          endif
        endif      


        if(dirp(k).eq.3)then
          xvp(1)=zp(1,mmp(lp,k))
          xvp(2)=zp(1,mmp(1+mod(lp,3),k))
          yvp(1)=zp(2,mmp(lp,k))
          yvp(2)=zp(2,mmp(1+mod(lp,3),k))
          zvp(1)=smp(k)
          zvp(2)=smp(k)
          ! desp z !(LOS INDICES EN PETSC VAN DESDE 0)   
          row_z(1,1)=vec_perm(cnum(nnp(lp,k))+1) 
          row_z(2,1)=vec_perm(cnum(nnp(1+mod(lp,3),k))+1) 
          row_z(1,2)=0
          row_z(2,2)=0
          ! desp x
          row_x(1,1)=row_z(1,1)+1
          row_x(2,1)=row_z(2,1)+1
          row_x(1,2)=row_z(1,1)+3
          row_x(2,2)=row_z(2,1)+3
          ! desp y
          row_y(1,1)=row_z(1,1)+2
          row_y(2,1)=row_z(2,1)+2
          row_y(1,2)=row_z(1,1)+4
          row_y(2,2)=row_z(2,1)+4
          ! valor de las funciones base en el vertice del sólido
          hz=zvs-smp(k)
          if(dabs(yvp(1)-yvp(2)).le.tol)then
            psi=(xvs-xvp(1))/(xvp(2)-xvp(1))
          else
            psi=(yvs-yvp(1))/(yvp(2)-yvp(1))
          endif
        endif      

        call ccoipmos(KR,COL,row_x,row_y,row_z,psi,hz,dirp(k))

 100  continue

      return      
      end
