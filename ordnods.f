c     Ordena los nodos de la superposición del sólido con el fluido en sentido antihorario    
      subroutine ordnods(xint,yint,zint,inter)
                 
      implicit double precision (a-h,o-z)
      integer inter,i,j,dir_min
      dimension xint(6),yint(6),zint(6),delta(3)
      double precision max(3),min(3)
      
      tol=1.d-6
      det=1
      
      max(1)=xint(1);min(1)=xint(1)
      max(2)=yint(1);min(2)=yint(1)
      max(3)=zint(1);min(3)=zint(1)

      do 10 i=2,inter
        if(xint(i).gt.max(1)) max(1)=xint(i)
        if(yint(i).gt.max(2)) max(2)=yint(i)
        if(zint(i).gt.max(3)) max(3)=zint(i)
        if(xint(i).lt.min(1)) min(1)=xint(i)
        if(yint(i).lt.min(2)) min(2)=yint(i)
        if(zint(i).lt.min(3)) min(3)=zint(i)
 10   continue

      delta_min=10.d10
      dir_min=1
      do 20 i=1,3
        delta(i)=dabs(max(i)-min(i))
        if(delta(i).lt.delta_min)then
          delta_min=delta(i)
          dir_min=i
        endif
 20   continue

      do 100 i=2,inter-1
        do 200 j=i+1,inter
          if(dir_min.eq.1)then
            det = (yint(i)-yint(1))*(zint(j)-zint(1))-
     &            (yint(j)-yint(1))*(zint(i)-zint(1))
          endif
          if(dir_min.eq.2)then
            det = (zint(i)-zint(1))*(xint(j)-xint(1))-
     &            (zint(j)-zint(1))*(xint(i)-xint(1))
          endif
          if(dir_min.eq.3)then
            det = (xint(i)-xint(1))*(yint(j)-yint(1))-
     &            (xint(j)-xint(1))*(yint(i)-yint(1))
          endif
          if(det.lt.0)then
            aux=xint(j)
            xint(j)=xint(i)
            xint(i)=aux
            aux=yint(j)
            yint(j)=yint(i)
            yint(i)=aux
            aux=zint(j)
            zint(j)=zint(i)
            zint(i)=aux
          end if
  200   continue
  100 continue

      return
      end
