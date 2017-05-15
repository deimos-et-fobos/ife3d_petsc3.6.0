      subroutine matrflpl_presion(numitap,ilcif,iverco,inodf,indcp,ielp,
     &           nelp,nuf,mmp,nnp,zp,cnum,zf,densf,x1,y1,x2,y2,
     &           x3,y3,dirp,smp,nglp,thin,KR,M,vec_perm,
     &           nvercof,ivercof,ielpf,indlpf,ngls)

      implicit double precision (a-h,o-z) 
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"

      dimension   mmp(3,*),nnp(6,*),zp(2,*),zf(3,*),
     &            ielp(*),indcp(*),ilcif(*),iverco(3,*),inodf(*),
     &            x1(*),y1(*),x2(*),y2(*),x3(*),y3(*),
     &            xp(3),yp(3),xf(3),yf(3),eta_p(3),csi_p(3),
     &            eta_f(3),csi_f(3),psi(3),xmc(3),ymc(3),zmc(3),
     &            ivercof(*),ielpf(*),indlpf(*)
      double precision nuf(3,*),densf(*),thin(*)
      integer cnum(*),dirp(*),dirn,smp(*),vec_perm(*)
      
      PetscInt    col(3),row(3)
      Mat         KR,M

      tol=0.000001
      do 100 j=1,numitap            ! loop sobre las caritas de contacto

         jk = ilcif(j)
         do 101 i=1,3
           row(i) = vec_perm (nglp+ngls+iverco(i,jk)) !(LOS INDICES EN PETSC VAN DESDE 0)
 101     continue

         if (indcp(j).eq.0) then   
   
            k=ielp(j)      ! elemento de la placa
            nf=inodf(j)    ! nodo del fluido

            ! coordenadas de los vertices de la placa
            do 12 i=1,3
               xp(i)=zp(1,nnp(i,k))
               yp(i)=zp(2,nnp(i,k))
 12         continue
            ! coordenadas de los vertices del fluido
            do 13 i=1,3
              if(dirp(k).eq.1)then
                xf(i)=zf(2,iverco(i,jk))
                yf(i)=zf(3,iverco(i,jk))
              endif
              if(dirp(k).eq.2)then
                xf(i)=zf(3,iverco(i,jk))
                yf(i)=zf(1,iverco(i,jk))
              endif
              if(dirp(k).eq.3)then
                xf(i)=zf(1,iverco(i,jk))
                yf(i)=zf(2,iverco(i,jk))
              endif
 13         continue

            ! coordenadas del centro de los lados de la carita j de la placa 
            xmc(1)=(x1(j)+x2(j))/2
            xmc(2)=(x3(j)+x2(j))/2
            xmc(3)=(x1(j)+x3(j))/2
            ymc(1)=(y1(j)+y2(j))/2
            ymc(2)=(y3(j)+y2(j))/2
            ymc(3)=(y1(j)+y3(j))/2

            ! altura de las funciones base de la placa en el medio de los lados de la carita
            ap=xp(2)-xp(1)
            bp=xp(3)-xp(1)
            cp=yp(2)-yp(1)
            dp=yp(3)-yp(1)
            do 20 i=1,3
              ep=xmc(i)-xp(1)
              fp=ymc(i)-yp(1)
              csi_p(i)=(dp*ep-bp*fp)/(ap*dp-bp*cp)
              eta_p(i)=(ap*fp-cp*ep)/(ap*dp-bp*cp)
 20         continue

            ! altura de las funciones base del fluido en el medio de los lados de la carita
            ap=xf(2)-xf(1)
            bp=xf(3)-xf(1)
            cp=yf(2)-yf(1)
            dp=yf(3)-yf(1)
            do 21 i=1,3
              ep=xmc(i)-xf(1)
              fp=ymc(i)-yf(1)
              csi_f(i)=(dp*ep-bp*fp)/(ap*dp-bp*cp)
              eta_f(i)=(ap*fp-cp*ep)/(ap*dp-bp*cp)
 21         continue

            ! area de la carita j
            area=dabs((x2(j)-x1(j))*(y3(j)-y1(j))-
     &                   (x3(j)-x1(j))*(y2(j)-y1(j)))/2.d0

            ! columna de la matriz de la placa donde ensamblar
            do 22 i=1,3
               col(i)=vec_perm(cnum(nnp(i,k))+1) !(LOS INDICES EN PETSC VAN DESDE 0)
 22         continue
            
            call ensflpl_presion(KR,M,row,col,nuf(1,nf),csi_p,eta_p,
     &           csi_f,eta_f,area,dirp(k),thin(k))
            
         endif
 100  continue



      do 200 j=1,nvercof           

        nf=ivercof(j)    ! nodo del fluido
        row(1) = vec_perm (nglp+ngls+nf) !(LOS INDICES EN PETSC VAN DESDE 0)

        if (indlpf(j).eq.0) then   
   
          k=ielpf(j)      ! elemento de la placa

          ! coordenadas de los vertices de la placa
          do 212 i=1,3
            xp(i)=zp(1,nnp(i,k))
            yp(i)=zp(2,nnp(i,k))
 212      continue
          ! coordenadas de los vertices del fluido
          if(dirp(k).eq.1)then
            xf(1)=zf(2,nf)
            yf(1)=zf(3,nf)
          endif
          if(dirp(k).eq.2)then
            xf(1)=zf(3,nf)
            yf(1)=zf(1,nf)
          endif
          if(dirp(k).eq.3)then
            xf(1)=zf(1,nf)
            yf(1)=zf(2,nf)
          endif

          ! altura de las funciones base de la placa en el nodo del fluido
          ap=xp(2)-xp(1)
          bp=xp(3)-xp(1)
          cp=yp(2)-yp(1)
          dp=yp(3)-yp(1)
          ep=xf(1)-xp(1)
          fp=yf(1)-yp(1)
          csi_p(1)=(dp*ep-bp*fp)/(ap*dp-bp*cp)
          eta_p(1)=(ap*fp-cp*ep)/(ap*dp-bp*cp)

          ! area de la carita j
          area=dabs((x2(j)-x1(j))*(y3(j)-y1(j))-
     &              (x3(j)-x1(j))*(y2(j)-y1(j)))/2.d0

          ! columna de la matriz de la placa donde ensamblar
          do 222 i=1,3
            col(i)=vec_perm(cnum(nnp(i,k))+1) !(LOS INDICES EN PETSC VAN DESDE 0)
 222      continue
            
          call ensflpl_presion2(KR,M,row,col,nuf(1,nf),csi_p,eta_p,
     &           csi_f,eta_f,area,dirp(k),thin(k))
            
         endif
 200  continue

      return      
      end
