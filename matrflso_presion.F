      subroutine matrflso_presion(numitas,ilcif,iverco,inodf,iels,
     &                nels,nuf,mms,zs,zf,densf,x1,y1,z1,
     &                x2,y2,z2,x3,y3,z3,nglp,ngls,KR,M,vec_perm)

      implicit double precision (a-h,o-z) 
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"

      dimension mms(4,*),zs(3,*),zf(3,*),
     &          iels(*),ilcif(*),iverco(3,*),inodf(*),
     &          x1(*),y1(*),z1(*),x2(*),y2(*),z2(*),x3(*),y3(*),z3(*),
     &          xs(4),ys(4),sz(4),xf(4),yf(4),fz(4),
     &          eta_f(3),csi_f(3),chi_f(3),eta_s(3),csi_s(3),chi_s(3),
     &          v1(3),v2(3),v3(3),xmc(3),ymc(3),zmc(3)
      double precision nuf(3,*),nus(3),densf(*),aux_norm
      integer vec_perm(*)
      
      PetscInt    col(4,3),row(3)
      Mat         KR,M

      tol=0.000001
      do 100 j=1,numitas            ! loop sobre las caritas de contacto

        jk = ilcif(j)
        do 101 i=1,3
          row(i) = vec_perm (nglp+ngls+iverco(i,jk)) !(LOS INDICES EN PETSC VAN DESDE 0)
 101    continue

        k=iels(j)                  ! elemento del solido
        nf=inodf(j)    ! nodo del fluido

        ! coordenadas de los vertices del solido
        do 12 i=1,4
          xs(i)=zs(1,mms(i,k))
          ys(i)=zs(2,mms(i,k))
          sz(i)=zs(3,mms(i,k))
 12     continue
        ! coordenadas de los vertices del fluido
        do 13 i=1,3
          xf(i)=zf(1,iverco(i,jk))
          yf(i)=zf(2,iverco(i,jk))
          fz(i)=zf(3,iverco(i,jk))
 13     continue
         
        ! coordenadas del centro de los lados de la carita j de la placa 
        xmc(1)=(x1(j)+x2(j))/2
        xmc(2)=(x3(j)+x2(j))/2
        xmc(3)=(x1(j)+x3(j))/2
        ymc(1)=(y1(j)+y2(j))/2
        ymc(2)=(y3(j)+y2(j))/2
        ymc(3)=(y1(j)+y3(j))/2
        zmc(1)=(z1(j)+z2(j))/2
        zmc(2)=(z3(j)+z2(j))/2
        zmc(3)=(z1(j)+z3(j))/2

        ! area de la carita j
        nus(1)=(y2(j)-y1(j))*(z3(j)-z1(j))-(z2(j)-z1(j))*(y3(j)-y1(j))
        nus(2)=(z2(j)-z1(j))*(x3(j)-x1(j))-(x2(j)-x1(j))*(z3(j)-z1(j))
        nus(3)=(x2(j)-x1(j))*(y3(j)-y1(j))-(y2(j)-y1(j))*(x3(j)-x1(j))
        area = norm2(nus)/2.d0
         
        ! altura de las funciones base del solido en el medio de los lados de la carita
        v1(1)=xs(2)-xs(1);v1(2)=ys(2)-ys(1);v1(3)=sz(2)-sz(1)
        v2(1)=xs(3)-xs(1);v2(2)=ys(3)-ys(1);v2(3)=sz(3)-sz(1)
        v3(1)=xs(4)-xs(1);v3(2)=ys(4)-ys(1);v3(3)=sz(4)-sz(1)
         
        det = 0
        det = det + v1(1)*(v2(2)*v3(3)-v2(3)*v3(2))
        det = det + v1(2)*(v2(3)*v3(1)-v2(1)*v3(3))
        det = det + v1(3)*(v2(1)*v3(2)-v2(2)*v3(1))

        do 20 i=1,3
          aj=xmc(i)-xs(1)
          ak=ymc(i)-ys(1)
          al=zmc(i)-sz(1)
          csi_s(i) = ( aj*(v2(2)*v3(3)-v2(3)*v3(2)) + ak*(v2(3)*v3(1)-
     &                 v3(3)*v2(1)) + al*(v2(1)*v3(2)-v3(1)*v2(2)))/det
          eta_s(i) = ( aj*(v3(2)*v1(3)-v1(2)*v3(3)) + ak*(v3(3)*v1(1)-
     &                 v3(1)*v1(3)) + al*(v3(1)*v1(2)-v1(1)*v3(2)))/det
          chi_s(i) = ( aj*(v1(2)*v2(3)-v2(2)*v1(3)) + ak*(v1(3)*v2(1)-
     &                 v2(3)*v1(1)) + al*(v1(1)*v2(2)-v1(2)*v2(1)))/det
 20     continue         

        ! altura de las funciones base del fluido en el medio de los lados de la carita
        v1(1)=xf(2)-xf(1);v1(2)=yf(2)-yf(1);v1(3)=fz(2)-fz(1)
        v2(1)=xf(3)-xf(1);v2(2)=yf(3)-yf(1);v2(3)=fz(3)-fz(1)
        do 30 i=1,3
          v3(i)=(v1(i)+v2(i))/2.d0
 30     continue
        aux_norm=norm2(v3)
        xf(4)=nuf(1,nf)*aux_norm+(xf(1)+xf(2)+xf(3))/3
        yf(4)=nuf(2,nf)*aux_norm+(yf(1)+yf(2)+yf(3))/3
        fz(4)=nuf(3,nf)*aux_norm+(fz(1)+fz(2)+fz(3))/3
        v3(1)=xf(4)-xf(1);v3(2)=yf(4)-yf(1);v3(3)=fz(4)-fz(1)
         
        det = 0
        det = det + v1(1)*(v2(2)*v3(3)-v2(3)*v3(2))
        det = det + v1(2)*(v2(3)*v3(1)-v2(1)*v3(3))
        det = det + v1(3)*(v2(1)*v3(2)-v2(2)*v3(1))
        
        do 21 i=1,3
          aj=xmc(i)-xf(1)
          ak=ymc(i)-yf(1)
          al=zmc(i)-fz(1)
          csi_f(i) = ( aj*(v2(2)*v3(3)-v2(3)*v3(2)) + ak*(v2(3)*v3(1)-
     &                 v3(3)*v2(1)) + al*(v2(1)*v3(2)-v3(1)*v2(2)))/det
          eta_f(i) = ( aj*(v3(2)*v1(3)-v1(2)*v3(3)) + ak*(v3(3)*v1(1)-
     &                 v3(1)*v1(3)) + al*(v3(1)*v1(2)-v1(1)*v3(2)))/det
          chi_f(i) = ( aj*(v1(2)*v2(3)-v2(2)*v1(3)) + ak*(v1(3)*v2(1)-
     &                 v2(3)*v1(1)) + al*(v1(1)*v2(2)-v1(2)*v2(1)))/det
 21     continue         


         ! columnas de la matriz del solido donde ensamblar
         do 24 i=1,4
           do 25 l=1,3
             col(i,l) = vec_perm(3*(mms(i,k)-1) + l + NGLP) !(LOS INDICES EN PETSC VAN DESDE 0)
 25        continue
 24      continue

         call ensflso_presion(KR,M,row,col,nuf(1,nf),csi_s,eta_s,chi_s,
     &                        csi_f,eta_f,area)
            
 100  continue

      return      
      end


      

            
         

