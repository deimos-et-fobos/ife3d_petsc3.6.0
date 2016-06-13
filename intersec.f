      subroutine intersec(x1s,y1s,x2s,y2s,x3s,y3s,x1f,y1f,
     &                    x2f,y2f,x3f,y3f,xint,yint,inter)

      implicit double precision (a-h,o-z)
      integer inter,i,j,k
      dimension xs(3),ys(3),xf(3),yf(3),zf(3),xint(6),yint(6),
     &          csis(3),etas(3),csif(3),etaf(3),csiint(6),etaint(6),
     &     csis2(3),etas2(3),csif2(3),etaf2(3),csiint2(6),etaint2(6)
      double precision L1,L2,tol
            
      tol=0.0001
      tol2=1.d-6
      do 100 i=1,6
         xint(i)=999
         yint(i)=999
         csiint(i)=999
         etaint(i)=999
         csiint2(i)=999
         etaint2(i)=999
  100 continue   
  
      if (determinante(x1s,y1s,x2s,y2s,x3s,y3s).lt.0) then
        aux=x2s; x2s=x3s; x3s=aux
        aux=y2s; y2s=y3s; y3s=aux
      endif
      
      if (determinante(x1f,y1f,x2f,y2f,x3f,y3f).lt.0) then
        aux=x2f; x2f=x3f; x3f=aux
        aux=y2f; y2f=y3f; y3f=aux
      endif

      xs(1)=x1s; xs(2)=x2s; xs(3)=x3s  
      ys(1)=y1s; ys(2)=y2s; ys(3)=y3s
      xf(1)=x1f; xf(2)=x2f; xf(3)=x3f  
      yf(1)=y1f; yf(2)=y2f; yf(3)=y3f 
      csis(1)=0; csis(2)=1; csis(3)=0      
      etas(1)=0; etas(2)=0; etas(3)=1      

      div = (ys(3)-ys(1))*(xs(2)-xs(1))-(xs(3)-xs(1))*(ys(2)-ys(1))
      do 200 i=1,3
        csif(i)=(ys(3)-ys(1))*(xf(i)-xs(1))
        csif(i)=csif(i)-(xs(3)-xs(1))*(yf(i)-ys(1))
        csif(i)=csif(i)/div
        if (dabs(ys(3)-ys(1)).gt.tol2) then
          etaf(i)=(yf(i)-ys(1))/(ys(3)-ys(1))
          etaf(i)=etaf(i)-(ys(2)-ys(1))/(ys(3)-ys(1))*csif(i)
        else
          etaf(i)=(xf(i)-xs(1))/(xs(3)-xs(1))
          etaf(i)=etaf(i)-(xs(2)-xs(1))/(xs(3)-xs(1))*csif(i)
        endif
 200  continue
        
      do 1000 i=1,3
         do 2000 j=1,3
            vcsis=csis(1+mod(i,3))-csis(i)
            vetas=etas(1+mod(i,3))-etas(i)
            vcsif=csif(1+mod(j,3))-csif(j)
            vetaf=etaf(1+mod(j,3))-etaf(j)
            vcsis_u=vcsis/dsqrt(vcsis**2+vetas**2)
            vetas_u=vetas/dsqrt(vcsis**2+vetas**2)
            vcsif_u=vcsif/dsqrt(vcsif**2+vetaf**2)
            vetaf_u=vetaf/dsqrt(vcsif**2+vetaf**2)
         
            ! Veo si son paralelas
            if (dabs(vcsis_u-vcsif_u).le.tol) then
               if (dabs(vetas_u-vetaf_u).le.tol) goto 2666
            endif  
            if (dabs(vcsis_u+vcsif_u).le.tol) then
               if (dabs(vetas_u+vetaf_u).le.tol) goto 2666
            endif             
            L1=-(csis(i)*vetaf-csif(j)*vetaf-vcsif*etas(i)+
     &           vcsif*etaf(j))/(vcsis*vetaf-vetas*vcsif)
            L2=-(-vcsis*etas(i)+etaf(j)*vcsis+vetas*csis(i)-
     &           vetas*csif(j))/(vcsis*vetaf-vetas*vcsif)
            csisol=csis(i)+L1*vcsis
            etasol=etas(i)+L1*vetas
            if (L1.ge.(-tol) .and. L1.le.((1+tol))) then
               if (L2.ge.(-tol) .and. L2.le.((1+tol))) then
                  if(L1.le.tol) then
                     csisol=csis(i)
                     etasol=etas(i)
                  endif
                  if(L1.ge.(1-tol)) then
                     csisol=csis(1+mod(i,3))
                     etasol=etas(1+mod(i,3))
                  endif
                  if(L2.le.tol) then
                     csisol=csif(j)
                     etasol=etaf(j)
                  endif
                  if(L2.ge.(1-tol)) then
                     csisol=csif(1+mod(j,3))
                     etasol=etaf(1+mod(j,3))
                  endif
                  if (inter.eq.0) then
                     inter=inter+1
                     csiint(inter)=csisol      
                     etaint(inter)=etasol
                  else
                     do 2200 k=1,inter 
                       if(dabs(csiint(k)-csisol).lt.tol)then
                         if(dabs(etaint(k)-etasol).lt.tol) goto 2666
                       endif                   
 2200                continue
                     inter=inter+1
                     csiint(inter)=csisol      
                     etaint(inter)=etasol
                  endif
               endif
            endif
 2666       continue 
 2000    continue
 1000 continue
     
      do 3000 i=1,3
        if(csif(i).ge.(-tol).and.csif(i).le.(1+tol))then
          if(etaf(i).ge.(-tol).and.etaf(i).le.(1-csif(i)+tol))then
            if (inter.eq.0) then
              inter=inter+1
              csiint(inter)=csif(i)      
              etaint(inter)=etaf(i)
            else
              do 3100 k=1,inter
                if(dabs(csiint(k)-csif(i)).lt.tol)then
                  if(dabs(etaint(k)-etaf(i)).lt.tol) goto 3166
                endif
 3100         continue
              inter=inter+1
              csiint(inter)=csif(i)
              etaint(inter)=etaf(i)
            endif
          endif
        endif
 3166   continue
 3000 continue  

      do 3200 j=1,inter
        xint(j)=xs(1)+(xs(2)-xs(1))*csiint(j)+(xs(3)-xs(1))*etaint(j)
        yint(j)=ys(1)+(ys(2)-ys(1))*csiint(j)+(ys(3)-ys(1))*etaint(j)
 3200 continue

      csif2(1)=0; csif2(2)=1; csif2(3)=0      
      etaf2(1)=0; etaf2(2)=0; etaf2(3)=1      
      div2 = (yf(3)-yf(1))*(xf(2)-xf(1))-(xf(3)-xf(1))*(yf(2)-yf(1))
      do 300 i=1,3
        csis2(i)=(yf(3)-yf(1))*(xs(i)-xf(1))
        csis2(i)=csis2(i)-(xf(3)-xf(1))*(ys(i)-yf(1))
        csis2(i)=csis2(i)/div2
        if (dabs(yf(3)-yf(1)).gt.tol2) then
          etas2(i)=(ys(i)-yf(1))/(yf(3)-yf(1))
          etas2(i)=etas2(i)-(yf(2)-yf(1))/(yf(3)-yf(1))*csis2(i)
        else
          etas2(i)=(xs(i)-xf(1))/(xf(3)-xf(1))
          etas2(i)=etas2(i)-(xf(2)-xf(1))/(xf(3)-xf(1))*csis2(i)
        endif
 300  continue
      
      inter2=0
      do 4000 i=1,3
        if(csis2(i).ge.(-tol).and.csis2(i).le.(1+tol))then
          if(etas2(i).ge.(-tol).and.etas2(i).le.(1-csis2(i)+tol))then
            if(inter2.eq.0) then
              inter2=inter2+1
              csiint2(inter2)=csis2(i)
              etaint2(inter2)=etas2(i)
            else
              do 4100 k=1,inter2
                if(dabs(csiint2(k)-csis2(i)).lt.tol)then
                  if(dabs(etaint2(k)-etas2(i)).lt.tol) goto 4166
                endif
 4100         continue
              inter2=inter2+1
              csiint2(inter2)=csis2(i)
              etaint2(inter2)=etas2(i)
            endif
          endif
        endif
 4166   continue
 4000 continue

      do 4200 j=1,inter2
        xsol=xf(1)+(xf(2)-xf(1))*csiint2(j)+(xf(3)-xf(1))*etaint2(j)
        ysol=yf(1)+(yf(2)-yf(1))*csiint2(j)+(yf(3)-yf(1))*etaint2(j) 
        csisol=(ys(3)-ys(1))*(xsol-xs(1))
        csisol=csisol-(xs(3)-xs(1))*(ysol-ys(1))
        csisol=csisol/div
        if (dabs(ys(3)-ys(1)).gt.tol2) then
          etasol=(ysol-ys(1))/(ys(3)-ys(1))
          etasol=etasol-(ys(2)-ys(1))/(ys(3)-ys(1))*csisol
        else
          etasol=(xsol-xs(1))/(xs(3)-xs(1))
          etasol=etasol-(xs(2)-xs(1))/(xs(3)-xs(1))*csisol
        endif
        if (inter.eq.0) then
          inter=inter+1
          xint(inter)=xsol    
          yint(inter)=ysol
        else
          do 4300 k=1,inter
            if(dabs(csiint(k)-csisol).lt.tol)then
              if(dabs(etaint(k)-etasol).lt.tol)goto 4366
            endif
 4300     continue
          inter=inter+1
          xint(inter)=xsol
          yint(inter)=ysol  
        endif
 4366   continue
 4200 continue
 
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                        FUNCION DETERMINANTE	
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      double precision function determinante(x1,y1,x2,y2,x3,y3)
   
      implicit double precision (a-h,o-z) 

      tol=0.0001
      a=(x2-x1)
      b=(y2-y1)
      c=(x3-x1)
      d=(y3-y1)

      if(c.eq.0) then
         px = a
      else
         px = a/c
      endif 
      if(d.eq.0) then
         py = b
      else
         py = b/d
      endif
      
      if(dabs(px-py).le.tol) then
         determinante = 0
      else 
         determinante=a*d-b*c
      endif

      end
