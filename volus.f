      subroutine volus(z,mm,nel,nver,vol)
      
      implicit double precision (a-h,o-z)
      dimension mm(4,*),vec(3,3),z(3,*),vol(*),aux(3)
      
      do 100 i=1,nel
         do 300 k=1,3
            do 310 l=2,4
               vec(k,l-1)=z(k,mm(l,i))-z(k,mm(1,i))
 310        continue
 300     continue 
         vol(i)=0.d0
         do 320 k=1,3
            aux(k)=vec(1+mod(k,3),1)*vec(1+mod(k+1,3),2)-
     &               vec(1+mod(k+1,3),1)*vec(1+mod(k,3),2)
            vol(i)=vol(i)+aux(k)*vec(k,3)
 320     continue
         vol(i)=dabs(vol(i))/6.d0
 100  continue
 
!     do 500 i=1,nel
!        write(1001,*) vol(i)
! 500 continue

      return
      end
