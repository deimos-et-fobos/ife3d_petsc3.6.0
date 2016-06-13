      subroutine coipmo(ncaco,numitap,inodf,ilcif,indcp,ielp,
     &                  nelp,nuf,mmp,nnp,zp,cnum,
     &                  x1,y1,x2,y2,x3,y3,dirp,smp,DOF,KR,vec_perm)
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
c	numitap	      --> 	# de caritas de contacto fluido-placa
c	inodf(numitap)     --> 	nodo del fluido de la carita de contacto
c	ilcif(numitap)	-->   n de cara de contacto del fluido de la carita de contacto
c	ielp(numitap)	-->   n de elemento de la placa de la carita de contacto
c	ncaco		--> 	# de caras de contacto del fluido
c	nelp		-->	# de elementos en la placa
c	icref		-->	referencias de las caras de contacto del fluido
c	mmp		-->	vertices de la placa
c	nnp		-->	nodos de la placa
c	iverco      -->	vertices de las caras del fluido en 
c				las caras de contacto
c	zp		-->	coordenadas de los vertices de la placa	
c	zf		-->	coordenadas de los vertices del fluido
c	
c
      implicit double precision (a-h,o-z) 
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"

      dimension   mmp(3,*),nnp(6,*),zp(2,*),
     &            inodf(*),ielp(*),ilcif(*),
     &            x1(*),y1(*),x2(*),y2(*),x3(*),y3(*),xc(3),yc(3),
     &            xp(3),yp(3),psi(3),indcp(*),xmc(3),ymc(3),zmc(3)
      double precision nuf(3,*)
      integer cnum(*),dirp(*),dirn,smp(*),vec_perm(*)
      
      PetscInt    COL(3,2),ROW,DOF
      Mat         KR

      tol=0.000001

      do 100 j=1,numitap            ! loop sobre las caritas de contacto

         jk = ilcif(j)
         ROW = vec_perm (DOF - NCACO + jk ) !(LOS INDICES EN PETSC VAN DESDE 0)

         if (indcp(j).eq.0) then   
   
            k=ielp(j)      ! elemento de la placa
            nf=inodf(j)    ! nodo del fluido

            ! coordenadas de los vertices de la placa
            do 12 i=1,3
               xp(i)=zp(1,mmp(i,k))
               yp(i)=zp(2,mmp(i,k))
 12         continue

            ! coordenadas de los vertices de la carita j de la placa
            xc(1)=x1(j)
            xc(2)=x2(j)
            xc(3)=x3(j)
            yc(1)=y1(j)
            yc(2)=y2(j)
            yc(3)=y3(j)

            xcb=(xc(1)+xc(2)+xc(3))/3
            ycb=(yc(1)+yc(2)+yc(3))/3
            
            ! altura de las funciones base en el baricentro de la carita
            ap=xp(2)-xp(1)
            bp=xp(3)-xp(1)
            cp=yp(2)-yp(1)
            dp=yp(3)-yp(1)
            ep=xcb-xp(1)
            fp=ycb-yp(1)

            csibar=(dp*ep-bp*fp)/(ap*dp-bp*cp)
            etabar=(ap*fp-cp*ep)/(ap*dp-bp*cp)
               
            ! area de la carita j
            area=dabs((xc(2)-xc(1))*(yc(3)-yc(1))-
     &                   (xc(3)-xc(1))*(yc(2)-yc(1)))/2.d0
            
            ! columna de la matriz de la placa donde ensamblar
            do 24 i=1,3
               col(i,1)=vec_perm(cnum(nnp(i,k))+1) !(LOS INDICES EN PETSC VAN DESDE 0)
 24         continue
            
           call ccoipmo(KR,col,ROW,nuf(1,nf),csibar,etabar,area,dirp(k))
            
         endif
         
         if (indcp(j).ge.1 .and. indcp(j).le.3) then   
            k=ielp(j)                  ! elemento de la placa
            nf=inodf(j)                ! nodo del fluido
            lp=indcp(j)
            ! coordenadas de los vertices de la placa
            xp(1)=zp(1,mmp(lp,k))
            xp(2)=zp(1,mmp(1+mod(lp,3),k))
            yp(1)=zp(2,mmp(lp,k))
            yp(2)=zp(2,mmp(1+mod(lp,3),k))
            ! columna de la matriz de la placa donde ensamblar !(LOS INDICES EN PETSC VAN DESDE 0)
            col(1,1)=vec_perm(cnum(nnp(lp,k))+3)            !rotaciones
            col(2,1)=vec_perm(cnum(nnp(1+mod(lp,3),k))+3)
            col(1,2)=vec_perm(cnum(nnp(lp,k))+5)            !desp in-plane
            col(2,2)=vec_perm(cnum(nnp(1+mod(lp,3),k))+5)
            dirn=1+mod(dirp(k)+1,3)
            
            if(dabs(xp(1)-xp(2)).le.tol) then
              xp(1)=yp(1)
              xp(2)=yp(2)
              yp(1)=zp(1,mmp(lp,k))
              yp(2)=zp(1,mmp(1+mod(lp,3),k))
              ! columna de la matriz de la placa donde ensamblar !(LOS INDICES EN PETSC VAN DESDE 0) 
              col(1,1)=vec_perm(cnum(nnp(lp,k))+2)          !rotaciones
              col(2,1)=vec_perm(cnum(nnp(1+mod(lp,3),k))+2)
              col(1,2)=vec_perm(cnum(nnp(lp,k))+4)          !desp in-plane
              col(2,2)=vec_perm(cnum(nnp(1+mod(lp,3),k))+4)
              dirn=1+mod(dirp(k),3)
            endif
            
            ! coordenadas del centro de los lados de la carita j de la placa 
            xmc(1)=(x1(j)+x2(j))/2
            xmc(2)=(x3(j)+x2(j))/2
            xmc(3)=(x1(j)+x3(j))/2
            ymc(1)=(y1(j)+y2(j))/2
            ymc(2)=(y3(j)+y2(j))/2
            ymc(3)=(y1(j)+y3(j))/2
            
            ! area de la carita j
            area=dabs((x2(j)-x1(j))*(y3(j)-y1(j))-
     &                   (x3(j)-x1(j))*(y2(j)-y1(j)))/2.d0
     
            ! altura de las funciones base en el medio de los lados de las caritas
            do 25 i=1,3
               zmc(i)=ymc(i)-smp(k)
               psi(i)=(xmc(i)-xp(1))/(xp(2)-xp(1))
 25         continue
            
            call ccoipmo2(KR,col,ROW,nuf(1,nf),psi,zmc,area,dirn)
         endif
 100  continue

      return      
      end
