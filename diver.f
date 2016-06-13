c*********************************************************************
c                 subprograma diver (doble precision)
c                          -----------------
c fin : calculo de la divergencia
c*********************************************************************
      subroutine diver(vol,area,ngk,uf1,uf2,uf3,uf4,div)
c*********************************************************************
c parametros :
c  vol            : volumen del elemento k.
c  area(i)        : area de la cara i (opuesta al vertice i)
c  z1,z2,z3,z4    : coordenadas de los vertices del elemento k.
c  ngk            : signos de los nodos del elemento k.
c  uf1,uf2,uf3,uf4: flujo normal en nodos del elemento k.
c  div            : divergencia del elemento k
c*********************************************************************
      implicit double precision(a-h,o-z)
      dimension ngk(*),area(*)

      r1=uf1*ngk(1)*area(1)/(3*vol)
      r2=uf2*ngk(2)*area(2)/(3*vol)
      r3=uf3*ngk(3)*area(3)/(3*vol)
      r4=uf4*ngk(4)*area(4)/(3*vol)

      div=(r1+r2+r3+r4)*3.d0

      return
      end
