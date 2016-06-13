cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                      c
      subroutine cramer3x3(a,b,x)
c                                                                      c
c     OBJECT:     resuelve un sistema de 3x3 no homogeneo              c
c                 por el metodo de cramer vs cramer.                   c
c                                                                      c
c     coded by Ronia                                                   c
c                                                                      c
c     INPUT:                                                           c
c           a:    matriz de 3x3                                        c
c           b:    termino independiente                                c
c                                                                      c
c     OUTPUT:                                                          c
c           x:    vector solucion                                      c
c                                                                      c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit none

      double precision det,x1,x2,x3,a(3,3),x(3),b(3)

      det=a(1,1)*a(2,2)*a(3,3)+a(1,2)*a(2,3)*a(3,1)+
     &      a(1,3)*a(2,1)*a(3,2) -a(3,1)*a(2,2)*a(1,3)-
     &      a(2,1)*a(1,2)*a(3,3)-a(1,1)*a(3,2)*a(2,3)

      x1=b(1)*a(2,2)*a(3,3)+a(1,2)*a(2,3)*b(3)+
     &      a(1,3)*b(2)*a(3,2)-b(3)*a(2,2)*a(1,3)-
     &      b(2)*a(1,2)*a(3,3)-b(1)*a(3,2)*a(2,3)

      x2=a(1,1)*b(2)*a(3,3)+b(1)*a(2,3)*a(3,1)+
     &      a(1,3)*a(2,1)*b(3)-a(3,1)*b(2)*a(1,3)-
     &      a(2,1)*b(1)*a(3,3)-a(1,1)*b(3)*a(2,3)

      x3=a(1,1)*a(2,2)*b(3)+a(1,2)*b(2)*a(3,1)+
     &      b(1)*a(2,1)*a(3,2)-a(3,1)*a(2,2)*b(1)-
     &      a(2,1)*a(1,2)*b(3)-a(1,1)*a(3,2)*b(2)

      x(1)=x1/det
      x(2)=x2/det
      x(3)=x3/det

      return
      end
