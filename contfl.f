ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      function contfl(x1,y1,x2,y2,x3,y3,zi,indcp,clf,w,rot,zmp)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit double precision (a-h,o-z)

      dimension w(3),rot(3),wg(6),csi(6),eta(6),xg(6),zg(6)
      integer elemento

      area=dabs((x2-x1)*(y3-y1)-(x3-x1)*(y2-y1))/2.d0
      
      ! Integral de Gauss para funciones de orden 4 con 6 puntos
      wg(1) = 2*0.054975871827671861
      wg(2) = 2*0.054975871827671861
      wg(3) = 2*0.054975871827671861
      wg(4) = 2*0.1116907948390055
      wg(5) = 2*0.1116907948390055
      wg(6) = 2*0.1116907948390055
      csi(1) = 0.091576213509771
      csi(2) = 0.091576213509771
      csi(3) = 0.816847572980459
      csi(4) = 0.108103018168070
      csi(5) = 0.445948490915965
      csi(6) = 0.445948490915965
      eta(1) = 0.091576213509771
      eta(2) = 0.816847572980459
      eta(3) = 0.091576213509771
      eta(4) = 0.445948490915965
      eta(5) = 0.108103018168070
      eta(6) = 0.445948490915965

      if (indcp.eq.0) then
         x12=(x1+x2)/2
         x23=(x3+x2)/2
         x31=(x1+x3)/2
         y12=(y1+y2)/2
         y23=(y3+y2)/2
         y31=(y1+y3)/2

         w12=w(1)+w(2)*x12+w(3)*y12
         w23=w(1)+w(2)*x23+w(3)*y23
         w31=w(1)+w(2)*x31+w(3)*y31

         contfl = ((clf-w12)**2+(clf-w23)**2+(clf-w31)**2) * area / 3
         
!         write(79,*) clf
      endif

      if (indcp.ne.0) then

         do 10 i=1,6
            xg(i)=x1*(1-csi(i)-eta(i)) + x2*csi(i) + x3*eta(i)
            zg(i)=y1*(1-csi(i)-eta(i)) + y2*csi(i) + y3*eta(i) - zmp
 10      continue

         contfl = 0
         do 11 i=1,6
            contfl = contfl + wg(i) * ( clf - ( zg(i) *
     &                  (rot(1) + rot(2)*xg(i) + rot(3)*zi) ) )**2
 11      continue

      endif

      return
      end
