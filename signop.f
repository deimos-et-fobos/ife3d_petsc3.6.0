      subroutine signop(nn,nel,nnod,iflag,ng)
      dimension nn(6,*),ng(3,*),iflag(*)
      
      do 1 i=1,nnod
         iflag(i)=0
 1    continue
      
      do 2 k=1,nel
         do 3 j=1,3
            if(iflag(nn(j+3,k)).eq.0)then
               ng(j,k)=1
               iflag(nn(j+3,k))=1
            else
               ng(j,k)=-1
            end if
 3       continue
!         write(59,*) (nn(i+3,k),i=1,3),(ng(i,k),i=1,3)
 2    continue 
 
      return
      end
