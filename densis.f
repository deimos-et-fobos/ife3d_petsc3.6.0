      function densis(x,y,z,nsd)
      implicit double precision (a-h,o-z)
      COMMON/coelass/youngs,poiss,denss,ndsds,isds
      dimension isds(30),youngs(30),poiss(30),denss(30)

      densis=denss(nsd)

      return
      end
