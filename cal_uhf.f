ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      function cal_uhf(mmf,zf,componentes,vol)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      implicit double precision (a-h,o-z)

      dimension mmf(4),zf(3,*),componentes(4),xg(4),yg(4),zg(4)
      double precision lmax,aux,l(6)

      a=componentes(1)
      b=componentes(2)
      c=componentes(3)
      d=componentes(4)

      pa = 0.5854105
      pb = 0.1381965
      aux = 0.0

      do 10 i=1,4
         xg(i) = pa * zf(1,mmf(i)) + pb * ( zf(1,mmf(1+mod(i,4))) + 
     &           zf(1,mmf(1+mod(i+1,4))) + zf(1,mmf(1+mod(i+2,4))) )
         yg(i) = pa * zf(2,mmf(i)) + pb * ( zf(2,mmf(1+mod(i,4))) + 
     &           zf(2,mmf(1+mod(i+1,4))) + zf(2,mmf(1+mod(i+2,4))) )
         zg(i) = pa * zf(3,mmf(i)) + pb * ( zf(3,mmf(1+mod(i,4))) + 
     &           zf(3,mmf(1+mod(i+1,4))) + zf(3,mmf(1+mod(i+2,4))) )
         
         aux = aux + (a+d*xg(i))**2 + (b+d*yg(i))**2 + (c+d*zg(i))**2
 10   continue
      
      do 20 i=1,6
         l(i) = 0
 20   continue
      do 21 i=1,3
         l(1) = l(1) + ( zf(i,mmf(1))-zf(i,mmf(2)) )**2
         l(2) = l(2) + ( zf(i,mmf(1))-zf(i,mmf(3)) )**2
         l(3) = l(3) + ( zf(i,mmf(1))-zf(i,mmf(4)) )**2
         l(4) = l(4) + ( zf(i,mmf(2))-zf(i,mmf(3)) )**2
         l(5) = l(5) + ( zf(i,mmf(2))-zf(i,mmf(4)) )**2
         l(6) = l(6) + ( zf(i,mmf(3))-zf(i,mmf(4)) )**2
 21   continue
      do 22 i=1,6
         l(i) = dsqrt(l(i))
 22   continue
 
      lmax = l(1)
      do 30 i=2,6
         if(l(i).gt.lmax) lmax = l(i)
 30   continue

      cal_uhf = aux * vol * lmax**2 / 4.0

      return
      end


