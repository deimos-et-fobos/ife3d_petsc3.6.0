      subroutine grad_presion(p,InvDk,grad)

      implicit double precision(a-h,o-z)

      double precision p(4),InvDk(3,3),aux(4,3),grad(3)

      do 1 i=1,3
        grad(i)=0.d0
        aux(1,i)= -(InvDk(i,1)+InvDk(i,2)+InvDk(i,3))
        aux(2,i)=   InvDk(i,1)
        aux(3,i)=   InvDk(i,2)
        aux(4,i)=   InvDk(i,3)
 1    continue

      do 2 i=1,3
        do 3 j=1,4
          grad(i)=grad(i)+aux(j,i)*p(j)     
 3      continue            
 2    continue            

      return
      end
