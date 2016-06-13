c     Ordena los nodos de la superposición del sólido con el fluido en sentido antihorario    
      subroutine ordnod(xint,yint,inter)
                 
      implicit double precision (a-h,o-z)
      integer inter,i,j
      dimension xint(6),yint(6)

      do 100 i=2,inter-1
         do 200 j=i+1,inter
            det = (xint(i)-xint(1))*(yint(j)-yint(1))-
     &            (xint(j)-xint(1))*(yint(i)-yint(1)) 
            if (det.lt.0) then
                aux=xint(j)
                xint(j)=xint(i)
                xint(i)=aux
                aux=yint(j)
                yint(j)=yint(i)
                yint(i)=aux
            end if
  200    continue
  100 continue
      
      return
      end
