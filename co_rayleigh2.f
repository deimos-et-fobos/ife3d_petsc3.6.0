      subroutine co_rayleigh2(nel,mm,z,u,vol,velsq,rayleigh)

      implicit double precision (a-h,o-z)

      dimension mm(4,*),z(3,*),vol(*),u(3,*),velsq(*),solx(4),soly(4),
     &            solz(4),vecx(4),vecy(4),vecz(4)
      double precision matriz(4,4)

      pa = 0.5854105
      pb = 0.1381965
      
      rayleigh = 0.0
      uh = 0.0
      div_u = 0.d0
      
      do 10 k=1,nel
        
        do 110 i=1,4
          matriz(i,1) = 1
          matriz(i,2) = z(1,mm(i,k))
          matriz(i,3) = z(2,mm(i,k))
          matriz(i,4) = z(3,mm(i,k))
          vecx(i) = u(1,mm(i,k))
          vecy(i) = u(2,mm(i,k))
          vecz(i) = u(3,mm(i,k))
 110    continue
        call solver4x4(matriz,vecx,solx)
        call solver4x4(matriz,vecy,soly)
        call solver4x4(matriz,vecz,solz)
        
        div_u = div_u + ( solx(2) + soly(3) + solz(4) )**2 * vol(k)
        
        aux = 0.d0
        do 120 i=1,4
          ux = pa * u(1,mm(i,k)) + pb* ( u(1,mm(1+mod(i,4),k)) +
     &         u(1,mm(1+mod(i+1,4),k)) + u(1,mm(1+mod(i+2,4),k)))
          uy = pa * u(2,mm(i,k)) + pb* ( u(2,mm(1+mod(i,4),k)) +
     &         u(2,mm(1+mod(i+1,4),k)) + u(2,mm(1+mod(i+2,4),k)))
          uz = pa * u(3,mm(i,k)) + pb* ( u(3,mm(1+mod(i,4),k)) +
     &         u(3,mm(1+mod(i+1,4),k)) + u(3,mm(1+mod(i+2,4),k)))
          aux = aux + ux**2 + uy**2 + uz**2
 120    continue
 
        uh = uh + aux * vol(k) / 4.0

 10   continue
 
      rayleigh = velsq(1)**2 * div_u / uh
      return
      end
