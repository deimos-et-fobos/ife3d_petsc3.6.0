      subroutine face2elem(z,mm,nn,face2el,ng,nel,nnod,nu,vol)
      
      implicit double precision (a-h,o-z)
      dimension mm(4,*),nn(4,*),ng(4,*),vec(3,3),z(3,*),vol(*),aux(3)
      integer cn(4,4),face2el(5,*)
      double precision nu(3,*)
      
      cn(1,1)=2; cn(1,2)=3; cn(1,3)=4; cn(1,4)=1
      cn(2,1)=1; cn(2,2)=4; cn(2,3)=3; cn(2,4)=2
      cn(3,1)=1; cn(3,2)=4; cn(3,3)=2; cn(3,4)=3
      cn(4,1)=1; cn(4,2)=2; cn(4,3)=3; cn(4,4)=4

      do 10 i=1,nnod
         face2el(nnod,5)=0      
 10   continue
 
      do 100 i=1,nel
         do 200 j=1,4
            do 300 k=1,3
               do 310 l=2,4
                  vec(k,l-1)=z(k,mm(cn(j,l),i))-z(k,mm(cn(j,1),i))
 310           continue
 300        continue 
            vol(i)=0.d0
            do 320 k=1,3
               aux(k)=vec(1+mod(k,3),1)*vec(1+mod(k+1,3),2)-
     &                  vec(1+mod(k+1,3),1)*vec(1+mod(k,3),2)
               area=dsqrt(aux(1)**2+aux(2)**2+aux(3)**2)
               vol(i)=vol(i)+aux(k)*vec(k,3)
 320        continue
            if (ng(j,i).eq.1) then
               do 400 k=1,3
                  face2el(k,nn(j,i))=mm(cn(j,k),i)
 400           continue
               face2el(4,nn(j,i))=i
               if (vol(i).gt.0) then
                  do 410 k=1,3
                     nu(k,nn(j,i))=-aux(k)/area
 410              continue
               else
                  do 420 k=1,3
                     nu(k,nn(j,i))=aux(k)/area
 420              continue
               endif    
            else
               face2el(5,nn(j,i))=i
            endif
 200     continue
         vol(i)=dabs(vol(i))/6.d0
 100  continue
 
!      do 500 i=1,nel
!         write(101,*) vol(i)
! 500  continue
!      do 600 i=1,nnod
!         write(102,*) (face2el(j,i),j=1,5)
!         if(face2el(5,i).eq.0) then 
!            write(103,*) (nu(j,i),j=1,3)
!         else
!            write(103,*) 0,0,0
!         endif
! 600  continue

      return
      end
