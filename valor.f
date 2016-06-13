****************************************************************
*                subprograma valor (llamado en carco)
* FIN : valor del flujo en un punto
*       de coordenadas baricentricas dadas
****************************************************************
      subroutine valor(vol,area,j,z1,z2,z3,z4,ngk,uf1,uf2,uf3,uf4,q)
****************************************************************
*   PARAMETROS :
*     entrada
*         vol           : volumen del elemento k
*         area(i)       : area de la cara i (opuesta al vertice i)
*         j             : numero de vertice del elemento 
*         z1,z2,z3,z4   : coordenadas de los vertices del elemento
*                         que contiene al punto                    
*         ngk           : contiene los signos asociados a los nodos
*                         del elemento que contiene al punto
*         uf1,uf2,uf3   : flujo normal en nodos de tal elemento   
*     salida  
*         q             : valor del flujo en el punto             
****************************************************************
      implicit double precision (a-h,o-z)
      dimension z1(*),z2(*),z3(*),z4(*),ngk(*),q(*),area(*),
     &            ab(3),ac(3),ad(3),bc(3),bd(3),cd(3) 
      
      do 10 i=1,3   
         ab(i)=z2(i)-z1(i)
         ac(i)=z3(i)-z1(i)
         ad(i)=z4(i)-z1(i)
         bc(i)=z3(i)-z2(i)
         bd(i)=z4(i)-z2(i)
         cd(i)=z4(i)-z3(i)
 10   continue

      r1=uf1*ngk(1)*area(1)/(3*vol)
      r2=uf2*ngk(2)*area(2)/(3*vol)
      r3=uf3*ngk(3)*area(3)/(3*vol)
      r4=uf4*ngk(4)*area(4)/(3*vol)
      
      if (j.eq.1) then
         do 11 k=1,3
            q(k)= - r2*ab(k) - r3*ac(k) - r4*ad(k)
 11      continue
      endif
      
      if (j.eq.2) then
         do 12 k=1,3
            q(k)=   r1*ab(k) - r3*bc(k) - r4*bd(k)
 12      continue
      endif
      
      if (j.eq.3) then
         do 13 k=1,3
            q(k)=   r1*ac(k) + r2*bc(k) - r4*cd(k)
 13      continue
      endif
      
      if (j.eq.4) then
         do 14 k=1,3
            q(k)=   r1*ad(k) + r2*bd(k) + r3*cd(k)
 14      continue
      endif             

      return
      end
