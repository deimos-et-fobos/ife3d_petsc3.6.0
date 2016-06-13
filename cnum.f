      subroutine cnum(nel,nver,nar,nn,num)

**************************************************************************
*                                                                        *
*      Calculo del puntero para almacenamiento tipo perfil de la matriz  *
*    del sistema                                                         *
*                                                                        *
*      VARIABLES:                                                        *
*                                                                        *
*        ENTRADA:                                                        *
*          nel --------> Numero de elementos de la triangulacion         *
*          ngl --------> Numero de grados de libertad                    *
*          mm ---------> Numeros de los vertices de la triangulacion     *
*                                                                        *
*        ENTRADA/SALIDA:                                                 *
*          mu ---------> Puntero                                         *
*                                                                        *
*      AUTORA: Maria Victoria Otero Pineiro                              *
*                                                                        *
*      FECHA: Junio, 1994                                                *
*                                                                        *
**************************************************************************

      implicit double precision(a-h,o-z)
      dimension nn(6,*),ind(nver+nar),num(nver+nar+1)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Diferenciamos los nodos que corresponden a un vertice o una arista  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   

      do 1 k=1,nel
         do 2 j=1,3
            ind(nn(j,k))=1         ! Vertice
            ind(nn(j+3,k))=0       ! Arista      
  2      continue
  1   continue    
 
      num(1)=0
      do 4 i=1,nver+nar 
         if (ind(i).eq.1) then
            num(i+1)=num(i)+5       !2 desp en el plano, 2 rot, 1 desp transversal
         else
            num(i+1)=num(i)+1
         endif
  4   continue

      return
      end
