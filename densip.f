      function densip(x,y,nsd)
      implicit double precision (a-h,o-z)
      common/coelasp/youngp,poisp,corrkp,densp,ndsdp,isdp
      dimension isdp(30),youngp(30),poisp(30),corrkp(30),densp(30) 

      densip=densp(1)

      if (ndsdp.eq.1) densip=densp(1)

      if (ndsdp.eq.2) then
        if(  (x.ge. 0.303)  .and. (x.le. 0.918) .and. 
     &      (y.ge. -0.0325) .and. (y.le. 0.0325) )then
          densip=densp(2)
        else
          densip=densp(1)
        endif
      endif

      return
      end
