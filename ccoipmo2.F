      subroutine ccoipmo2(KR,col,ROW,nu,psi,zmc,area,dirn)

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
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"

      dimension zmc(3),psi(3) 
      double precision nu(*),normal
      integer dirn

      PetscInt    COL(3,2),ROW
      PetscScalar VAL(3)
      Mat        KR

      normal = 1
      if(dirn.eq.1) normal=nu(1)
      if(dirn.eq.2) normal=nu(2)
      if(dirn.eq.3) normal=nu(3)
   
************************************************************************
*     Rotaciones
************************************************************************
      val(1)=0.0
      val(2)=0.0
      do 10 i=1,3
         val(1)= val(1) + zmc(i)*(1 - psi(i))
         val(2)= val(2) + zmc(i)*psi(i)
 10   continue

      do 12 i=1,2
        val(i) = val(i) * area * normal /3
 12   continue

      do 13 j=1,2
        if(ROW.ge.COL(j,1))then
          CALL MatSetValue(KR,COL(j,1),ROW,VAL(j),ADD_VALUES,IERR)
        else
          CALL MatSetValue(KR,ROW,COL(j,1),VAL(j),ADD_VALUES,IERR)
        endif
 13   continue

************************************************************************
*     Desplazamiento in-plane
************************************************************************
      val(1)=0.0
      val(2)=0.0
      do 20 i=1,3
         val(1)= val(1) + (1 - psi(i))
         val(2)= val(2) + psi(i)
 20   continue

      do 22 i=1,2
        val(i) = val(i) * area * normal /3
 22   continue

      do 23 j=1,2
        if(ROW.ge.COL(j,2))then
          CALL MatSetValue(KR,COL(j,2),ROW,VAL(j),ADD_VALUES,IERR)
        else
          CALL MatSetValue(KR,ROW,COL(j,2),VAL(j),ADD_VALUES,IERR)
        endif
 23   continue

      return     
      end
