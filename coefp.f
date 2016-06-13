      function coefp(i,x,y,nsd)
      implicit double precision (a-h,o-z)
      common/coelasp/youngp,poisp,corrkp,densp,ndsdp,isdp
      dimension isdp(30),youngp(30),poisp(30),corrkp(30),densp(30)

      coefp=1
      do 1 j=1,ndsdp
         if(isdp(j).eq.nsd) then
            if(i.eq.1) coefp=youngp(j)
            if(i.eq.2) coefp=poisp(j)
            if(i.eq.3) coefp=corrkp(j)
         endif
 1    continue
      
      return
      end
