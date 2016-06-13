      subroutine signof(nn,nel,nnod,iflag,ng)
      
      dimension nn(4,*),ng(4,*),iflag(*)
 
      do 1 i=1,nnod
         iflag(i)=0
 1    continue
      
      do 2 k=1,nel
         do 3 j=1,4
            if(iflag(nn(j,k)).eq.0)then
               ng(j,k)=1
               iflag(nn(j,k))=1
            else
               ng(j,k)=-1
            end if
 3       continue
!         write(99,*) (nn(j,k),j=1,4),(ng(i,k),i=1,4)
 2    continue 
           
      return
      end
