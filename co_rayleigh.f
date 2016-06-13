      subroutine co_rayleigh(nel,mm,z,p_conf,vol,velsq,rayleigh)

      implicit double precision (a-h,o-z)

      dimension mm(4,*),z(3,*),vol(*),p_conf(*),velsq(*),vec(4),sol(4)
      double precision matriz(4,4)

      pa = 0.5854105
      pb = 0.1381965
      
      rayleigh = 0.0
      ph = 0.0
      grad_p = 0.d0
      
      do 10 k=1,nel
        
        do 110 i=1,4
          matriz(i,1) = 1
          matriz(i,2) = z(1,mm(i,k))
          matriz(i,3) = z(2,mm(i,k))
          matriz(i,4) = z(3,mm(i,k))
          vec(i) = p_conf(mm(i,k))
 110    continue
        call solver4x4(matriz,vec,sol)
        
        grad_p = grad_p + (sol(2)**2 + sol(3)**2 + sol(4)**2) * vol(k)

        aux = 0.d0
        do 120 i=1,4
          aux1 = pa * p_conf(mm(i,k)) + pb* ( p_conf(mm(1+mod(i,4),k)) +
     &         p_conf(mm(1+mod(i+1,4),k)) + p_conf(mm(1+mod(i+2,4),k)))
          aux = aux + aux1**2
 120    continue
 
        ph = ph + aux * vol(k) / 4.0

 10   continue
      
      rayleigh = velsq(1)**2 * grad_p / ph
      return
      end
