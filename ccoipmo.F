      subroutine ccoipmo(KR,col,ROW,nu,csibar,etabar,area,dirp)

c
c	By Rodolfo Rodriguez y Ronia
c	8/7/98
c
c	OBJETIVO
c
c	Esta rutina se encarga de armar el vector de punteros a filas
c	de la matriz de interaccion solido-interface.
c	Esta funcion es llamada desde coipmo.
c
c	INPUT
c
c	col		-->	nodos (columnas) del solido en donde ensamblar
c	jk		-->	numero de la cara de contacto del liquido
c	nf		-->	nodo de contacto del liquido
c     nu		-->	normal entrante al solido 
c	eta,csi     -->	coordenadas de referencia en los vertices de la carita 
c	area		--> 	area de la carita de interseccion solido-liquido
c	dirp		-->	dirección de la normal de la placa
c
      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"

      double precision nu(*),normal
      integer dirp

      PetscInt    COL(3,2),ROW
      PetscScalar VAL(3)
      Mat         KR

      val(1) = 1 - csibar - etabar
      val(2) = csibar
      val(3) = etabar
      
      normal = 1
      if(dirp.eq.1) normal=nu(1)
      if(dirp.eq.2) normal=nu(2)
      if(dirp.eq.3) normal=nu(3)
  
      do 12 i=1,3
         val(i) = val(i) * area * normal
 12   continue

      do 13 j=1,3
         if(ROW.ge.COL(j,1))then
           CALL MatSetValue(KR,COL(j,1),ROW,VAL(j),ADD_VALUES,IERR)
         else
           CALL MatSetValue(KR,ROW,COL(j,1),VAL(j),ADD_VALUES,IERR)
         endif      
 13   continue

      return
      end
