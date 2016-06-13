      function coefs(i,x,y,z,nsd)
      implicit double precision (a-h,o-z)
      COMMON/coelass/youngs,poiss,denss,ndsds,isds
      dimension isds(30),youngs(30),poiss(30),denss(30)

      coefs=1
      do 1 j=1,ndsds
         if(isds(j).eq.nsd) then
            if(i.eq.1) then 
               coefs=youngs(j)*poiss(j)/((1+poiss(j))*(1-2*poiss(j)))
            endif
            if(i.eq.2) then
               coefs=youngs(j)/(2*(1+poiss(j)))
            endif
         endif
 1    continue

      return
      end
