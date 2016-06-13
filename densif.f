      function densif(x,y,z,nsd)
      implicit double precision (a-h,o-z)
      common/coeffl/densf,velsq,ndsd,isd
      dimension isd(30),densf(30),velsq(30)

      densif=densf(1)
      do 1 j=1,ndsd
         if(isd(j).eq.nsd) then
            densif=densf(j)
         end if
 1    continue

      return
      end
