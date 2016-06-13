cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                      c
      subroutine solver4x4(AA,bb,x)
c                                                                      c
c     OBJECT:     resuelve un sistema de 3x3 no homogeneo              c
c                 por el metodo de cramer vs cramer.                   c
c                                                                      c
c     coded by Ronia                                                   c
c                                                                      c
c     INPUT:                                                           c
c           A:    matriz de 4x4                                        c
c           b:    termino independiente                                c
c                                                                      c
c     OUTPUT:                                                          c
c           x:    vector solucion                                      c
c                                                                      c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit double precision (a-h,o-z)

      dimension A(4,4),x(4),b(4),AA(4,4),bb(4)
      integer pivot
      
      do 1 i=1,4
         do 2 j=1,4
            A(i,j)= AA(i,j)
 2       continue
         b(i)= bb(i)
 1    continue
 
      tol = 1d-9
      do 10 i=1,3
         pivot = i
         do 11 j=i+1,4
            if (dabs(A(j,i)).gt.dabs(A(pivot,i))) pivot = j
 11      continue
         if (pivot.ne.i) then
            do 12 k=i,4
               aux    = A(i,k)
               A(i,k) = A(pivot,k)
               A(pivot,k) = aux
 12         continue
            aux      = b(i)
            b(i)     = b(pivot)
            b(pivot) = aux
         endif
         if (dabs(A(i,i)).lt.tol) stop 'Pivot = 0'

         do 20 j=i+1,4
            A(j,i)=A(j,i)/A(i,i)
            do 21 k=i+1,4
               A(j,k)=A(j,k)-A(j,i)*A(i,k)
 21         continue
            b(j)=b(j)-A(j,i)*b(i)
 20      continue
 10   continue
      
      x(4)=b(4)/A(4,4)
      do 30 i=3,1,-1 
         x(i)=b(i)
         do 31 j=i+1,4
            x(i)=x(i)-A(i,j)*x(j)
 31      continue
         x(i)=x(i)/A(i,i)
 30   continue
 
      return
      end
