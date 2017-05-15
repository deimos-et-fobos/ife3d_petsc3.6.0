      subroutine coismo(ncaco,numitas,inodf,ilcif,iels,
     &                  nels,nuf,mms,z,x1,y1,z1,x2,y2,z2,
     &                  x3,y3,z3,NGLP,DOF,KR,vec_perm)
c
c	By Rodolfo Rodriguez y Ronia
c	8/7/98
c
c	OBJETIVO
c	
c	Arma la matriz cois (interaccion placa-Interface) con 
c	non matching grids
c
c	INPUT
c
c	ncaco	            --> 	# de caras de contacto del fluido
c	numitas	      --> 	# de caritas de contacto fluido-placa
c	inodf(numitas)     --> 	nodo del fluido de la carita de contacto
c	ilcif(numitas)	-->   n de cara de contacto del fluido de la carita de contacto
c	iels(numitas)	-->   n de elemento de la placa de la carita de contacto
c	nels		-->	# de elementos en el solido
c	nrcs		-->	referencia de los elementos del solido
c	icref		-->	referencias de las caras de contacto del fluido
c	mms		-->	vertices del solido
c	nnp		-->	nodos del solido
c	iverco      -->	vertices de las caras del fluido en 
c				las caras de contacto
c	z		-->	coordenadas de los vertices del solido	
c	zf		-->	coordenadas de los vertices del fluido
c	mua1c       --> 	puntero a filas
c	mua2c		-->	puntero a columnas
c	cois		-->	valores de la matriz
c	ndcacos     -->	memoria reservada para la matriz cois
c	
c
      implicit double precision (a-h,o-z) 
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"

      dimension mms(4,*),z(3,*),nels(*),inodf(*),iels(*),
     &          ilcif(*),x1(*),y1(*),z1(*),x2(*),y2(*),z2(*),
     &          x3(*),y3(*),z3(*),xc(3),yc(3),zc(3),xs(4),ys(4),zs(4),
     &          v1(3),v2(3),v3(3)
      double precision nuf(3,*),nus(3)
      integer vec_perm(*)

      PetscInt    col(12),ROW,DOF
      Mat         KR
      
      do 100 j=1,numitas            ! loop sobre las caritas de contacto
      
         jk = ilcif(j)
         ROW = vec_perm(DOF - NCACO + jk) !(LOS INDICES EN PETSC VAN DESDE 0)
         k=iels(j)                  ! elemento del solido
         nf=inodf(j)                ! nodo del fluido

         ! coordenadas de los vertices del solido
         do 12 i=1,4
            xs(i)=z(1,mms(i,k))
            ys(i)=z(2,mms(i,k))
            zs(i)=z(3,mms(i,k))
 12      continue
         
         ! coordenadas de los vertices de la carita j del solido 
         xc(1)=x1(j);xc(2)=x2(j);xc(3)=x3(j)
         yc(1)=y1(j);yc(2)=y2(j);yc(3)=y3(j)
         zc(1)=z1(j);zc(2)=z2(j);zc(3)=z3(j)
      
         xcb=(xc(1)+xc(2)+xc(3))/3
         ycb=(yc(1)+yc(2)+yc(3))/3
         zcb=(zc(1)+zc(2)+zc(3))/3

            
         
         ! altura de las funciones base en el baricentro de la carita
         v1(1)=xs(2)-xs(1);v1(2)=ys(2)-ys(1);v1(3)=zs(2)-zs(1)
         v2(1)=xs(3)-xs(1);v2(2)=ys(3)-ys(1);v2(3)=zs(3)-zs(1)
         v3(1)=xs(4)-xs(1);v3(2)=ys(4)-ys(1);v3(3)=zs(4)-zs(1)
         
         det = 0
         det = det + v1(1)*(v2(2)*v3(3)-v2(3)*v3(2))
         det = det + v1(2)*(v2(3)*v3(1)-v2(1)*v3(3))
         det = det + v1(3)*(v2(1)*v3(2)-v2(2)*v3(1))

         aj=xcb-xs(1)
         ak=ycb-ys(1)
         al=zcb-zs(1)
         csibar = ( aj*(v2(2)*v3(3)-v2(3)*v3(2)) + ak*(v2(3)*v3(1)-
     &              v3(3)*v2(1)) + al*(v2(1)*v3(2)-v3(1)*v2(2))) / det  
         etabar = ( aj*(v3(2)*v1(3)-v1(2)*v3(3)) + ak*(v3(3)*v1(1)-
     &              v3(1)*v1(3)) + al*(v3(1)*v1(2)-v1(1)*v3(2))) / det  
         chibar = ( aj*(v1(2)*v2(3)-v2(2)*v1(3)) + ak*(v1(3)*v2(1)-
     &              v2(3)*v1(1)) + al*(v1(1)*v2(2)-v1(2)*v2(1))) / det  
         
         ! area de la carita j
         nus(1)=(yc(2)-yc(1))*(zc(3)-zc(1))-(zc(2)-zc(1))*(yc(3)-yc(1))
         nus(2)=(zc(2)-zc(1))*(xc(3)-xc(1))-(xc(2)-xc(1))*(zc(3)-zc(1))
         nus(3)=(xc(2)-xc(1))*(yc(3)-yc(1))-(yc(2)-yc(1))*(xc(3)-xc(1))
         area =dsqrt(nus(1)**2+nus(2)**2+nus(3)**2)/2.d0
         
         ! columnas de la matriz del solido donde ensamblar
         do 24 i=1,4
           do 25 l=1,3
             col((i-1)*3+l) = vec_perm(3*(mms(i,k)-1) + l + NGLP) !(LOS INDICES EN PETSC VAN DESDE 0)
 25        continue
 24      continue
         
         call ccoismo(KR,col,ROW,nuf(1,nf),csibar,etabar,chibar,area)
 100  continue

      return      
      end
