      subroutine intersec2(x1s,x2s,x1f,y1f,x2f,y2f,
     &                     x3f,y3f,xint,yint,smp,
     &                     thin,inter)

      implicit double precision (a-h,o-z)
      integer inter,i,j,k
      dimension xs(4),ys(4),xf(3),yf(3),xint(6),yint(6)
      double precision L1,L2,tol
            
      tol=0.000000000001
      do 200 i=1,6
         xint(i)=999
         yint(i)=999
  200 continue
     
      if(x1s.gt.x2s) then
         aux=x1s
         x1s=x2s
         x2s=aux
      endif
   
      xs(1)=x1s; xs(2)=x2s; xs(3)=x2s; xs(4)=x1s  
      ys(1)=smp-thin/2; ys(2)=smp-thin/2; 
      ys(3)=smp+thin/2; ys(4)=smp+thin/2;
      xf(1)=x1f; xf(2)=x2f; xf(3)=x3f  
      yf(1)=y1f; yf(2)=y2f; yf(3)=y3f 
      
      do 1000 i=1,4
         do 2000 j=1,3
            vxs=xs(1+mod(i,4))-xs(i)
            vys=ys(1+mod(i,4))-ys(i)
            vxf=xf(1+mod(j,3))-xf(j)
            vyf=yf(1+mod(j,3))-yf(j)
            vxs_u=vxs/dsqrt(vxs**2+vys**2)
            vys_u=vys/dsqrt(vxs**2+vys**2)
            vxf_u=vxf/dsqrt(vxf**2+vyf**2)
            vyf_u=vyf/dsqrt(vxf**2+vyf**2)
         
         ! Veo si son paralelas
            if (dabs(vxs_u-vxf_u).le.tol) then
               if (dabs(vys_u-vyf_u).le.tol) goto 3000
            endif
            if (dabs(vxs_u+vxf_u).le.tol) then
               if (dabs(vys_u+vyf_u).le.tol) goto 3000
            endif                
            L1=-(xs(i)*vyf-xf(j)*vyf-vxf*ys(i)+vxf*yf(j))/
     &          (vxs*vyf-vys*vxf)
            L2=-(-vxs*ys(i)+yf(j)*vxs+vys*xs(i)-vys*xf(j))/
     &          (vxs*vyf-vys*vxf)
            xsol=xs(i)+L1*vxs
            ysol=ys(i)+L1*vys
            if (L1.ge.(-tol) .and. L1.le.(1+tol)) then
               if (L2.ge.(-tol) .and. L2.le.(1+tol)) then
                  if (inter.eq.0) then
                     inter=inter+1
                     xint(inter)=xsol      
                     yint(inter)=ysol
                  else
                     do 2200 k=1,inter 
                        if (dabs(xint(k)-xsol).le.tol) then
                           if (dabs(yint(k)-ysol).le.tol) goto 3000
                        endif 
 2200                continue  
                        inter=inter+1
                     xint(inter)=xsol      
                     yint(inter)=ysol
                  endif
               end if
            end if
 3000       continue
 2000    continue
             
         det=determinante2(xf(1),yf(1),xf(2),yf(2),xf(3),yf(3))
         aux=determinante2(xf(1),yf(1),xf(2),yf(2),xs(i),ys(i))
         if ( (aux*det) .gt. tol) then
            aux=determinante2(xf(2),yf(2),xf(3),yf(3),xs(i),ys(i))
            if ( (aux*det) .gt. tol) then
               aux=determinante2(xf(3),yf(3),xf(1),yf(1),xs(i),ys(i))
               if ( (aux*det) .gt. tol) then
                  if (inter.eq.0) then
                     inter=inter+1
                     xint(inter)=xs(i)    
                     yint(inter)=ys(i)
                  else
                     do 2300 k=1,inter 
                        if (dabs(xint(k)-xs(i)).le.tol) then
                           if (dabs(yint(k)-ys(i)).le.tol) goto 3100
                        endif 
 2300                continue  
                     inter=inter+1
                     xint(inter)=xs(i)    
                     yint(inter)=ys(i)
                  endif
               end if
            end if
         end if
 3100    continue         
         
 1000 continue

      do 4000 i=1,3
         if(xf(i).gt.(xs(1)-tol) .and. xf(i).lt.(xs(3)+tol)) then
            if(xf(i).gt.(xs(1)-tol) .and. xf(i).lt.(xs(3)+tol)) then
               if (inter.eq.0) then
                  inter=inter+1
                  xint(inter)=xf(i)      
                  yint(inter)=yf(i)
               else
                  do 4100 k=1,inter 
                     if (dabs(xint(k)-xf(i)).le.tol) then
                        if (dabs(yint(k)-yf(i)).le.tol) goto 5000
                     endif 
 4100             continue  
                  inter=inter+1
                  xint(inter)=xf(i)    
                  yint(inter)=yf(i)
               endif
            endif
         endif
 5000    continue    
 4000 continue
 
      do 6000 i=1,inter
         yint(i)=yint(i)-smp
 6000 continue

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                        FUNCION DETERMINANTE	
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      double precision function determinante2(x1,y1,x2,y2,x3,y3)
      
      implicit double precision (a-h,o-z) 
      determinante2=(x2-x1)*(y3-y1)-(x3-x1)*(y2-y1)

      end
