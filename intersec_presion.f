      subroutine intersec_presion(x1s,y1s,x2s,y2s,x3s,y3s,x1f,y1f,inter)

      implicit double precision (a-h,o-z)
      integer inter,i,j,k
      dimension xs(3),ys(3)
      double precision tol
            
      tol=0.001
  
      if (determinante(x1s,y1s,x2s,y2s,x3s,y3s).lt.0) then
        aux=x2s; x2s=x3s; x3s=aux
        aux=y2s; y2s=y3s; y3s=aux
      endif
      
      xs(1)=x1s; xs(2)=x2s; xs(3)=x3s  
      ys(1)=y1s; ys(2)=y2s; ys(3)=y3s

      ap=xs(2)-xs(1)
      bp=xs(3)-xs(1)
      cp=ys(2)-ys(1)
      dp=ys(3)-ys(1)
      ep=x1f-xs(1)
      fp=y1f-ys(1)
      csif=(dp*ep-bp*fp)/(ap*dp-bp*cp)
      etaf=(ap*fp-cp*ep)/(ap*dp-bp*cp)

      if(csif.ge.(-tol).and.csif.le.(1+tol))then
        if(etaf.ge.(-tol).and.etaf.le.(1-csif+tol))then
          inter=1
        endif
      endif

      return
      end


