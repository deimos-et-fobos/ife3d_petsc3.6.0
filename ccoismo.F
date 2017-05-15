      subroutine ccoismo(KR,col,ROW,nu,csibar,etabar,chibar,area)
c
c	By Rodolfo Rodriguez y Ronia
c	8/7/98
c
c	OBJETIVO
c
c	Esta rutina se encarga de armar el vector de punteros a filas
c	de la matriz de interaccion solido-interface.
c	Esta funcion es llamada desde coismo.
c
c	INPUT
c
c	mua1c       --> 	puntero a filas
c	mua2c		-->	puntero a columnas
c	cois		-->	valores de la matriz
c	ndcacop     -->	memoria reservada para la matriz cois
c	col		-->	nodos (columnas) del solido en donde ensamblar
c	jk		-->	numero de la cara de contacto del liquido
c	nf		-->	nodo de contacto del liquido
c     nu		-->	normal entrante al solido 
c	eta,csi     -->	coordenadas de referencia en los vertices de la carita 
c	area		--> 	area de la carita de interseccion solido-liquido
c
      implicit double precision (a-h,o-z)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"
      
      double precision nu(*)

      PetscInt    col(*),ROW
      PetscScalar VAL(12)
      Mat         KR

      
      do 10 i=1,3
        val(i) = 1 - csibar - etabar - chibar
        val(i+3) = csibar
        val(i+6) = etabar
        val(i+9) = chibar
 10   continue
     
      do 12 i=0,3
        val(1+i*3) = val(1+i*3) * area * nu(1) 
        val(2+i*3) = val(2+i*3) * area * nu(2) 
        val(3+i*3) = val(3+i*3) * area * nu(3) 
 12   continue
      
      do 13 j=1,12
         if(ROW.ge.COL(j))then
           CALL MatSetValue(KR,COL(j),ROW,VAL(j),ADD_VALUES,IERR)
         else
           CALL MatSetValue(KR,ROW,COL(j),VAL(j),ADD_VALUES,IERR)
         endif   
 13   continue

      return
      end
