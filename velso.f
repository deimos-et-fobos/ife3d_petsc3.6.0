      function velso(x,y,z,nsd)
      implicit double precision (a-h,o-z)
      common/coeffl/densf,velsq,ndsd,isd
      dimension isd(30),densf(30),velsq(30)

      velso=velsq(1)
      do 1 j=1,ndsd
         if(isd(j).eq.nsd) then
            velso=velsq(j)
         end if
    1 continue

      return
      end
