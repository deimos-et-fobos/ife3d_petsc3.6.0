      subroutine solp_conf(nel,nver,mm,nn,z,p_noconf,z_noconf,
     &                                          vol,vsum,iver,p_conf)

      implicit double precision (a-h,o-z)

      dimension mm(4,*),nn(4,*),z(3,*),p_noconf(*),z_noconf(3,*),
     &          p_conf(*),vec(4),sol(4),iver(*),vol(*),bar(3),vsum(*)
      double precision matriz(4,4),int_pnc
      
      open(24,file='pnoconf.sol00')
      write(24,'(a)')  '*NOD'        
      write(24,*) nel*4
      write(24,'(a)')  '*SCALAR_FIELD' 
      write(24,'(a)')  ' Continuation parameter: 0' 
      write(24,'(a)') '<NONE>'
      
      do 10 i=1,nver
         iver(i)=0
         p_conf(i)=0.d0
         vsum(i)=0.d0
 10   continue
 
      do 100 k=1,nel
        
        do 110 i=1,4
          matriz(i,1) = 1
          matriz(i,2) = z_noconf(1,nn(i,k))
          matriz(i,3) = z_noconf(2,nn(i,k))
          matriz(i,4) = z_noconf(3,nn(i,k))
          vec(i) = p_noconf(nn(i,k))
 110    continue
        call solver4x4(matriz,vec,sol)

!        do 120 i=1,3
!          bar(i) = ( z(i,mm(1,k)) + z(i,mm(2,k)) 
!     &             + z(i,mm(3,k)) + z(i,mm(4,k)) )/4
! 120    continue
! 
!        int_pnc = (sol(1) + sol(2)*bar(1) + sol(3)*bar(2)
!     &                                    + sol(4)*bar(3) ) * vol(k)
!        do 130 j=1,4
!          vsum(mm(j,k)) = vsum(mm(j,k)) + vol(k)
!          p_conf(mm(j,k)) = p_conf(mm(j,k)) + int_pnc 
!          write(24,*) sol(1) + sol(2)*z(1,mm(j,k)) + sol(3)*
!     &                              z(2,mm(j,k)) + sol(4)*z(3,mm(j,k))
! 130    continue

        do 120 j=1,4
          iver(mm(j,k))=iver(mm(j,k))+1
          p_conf(mm(j,k)) = p_conf(mm(j,k)) + sol(1) + sol(2)*
     &        z(1,mm(j,k)) + sol(3)*z(2,mm(j,k)) + sol(4)*z(3,mm(j,k))
          write(24,*) sol(1) + sol(2)*z(1,mm(j,k)) + sol(3)*
     &                              z(2,mm(j,k)) + sol(4)*z(3,mm(j,k))
 120    continue
 
 100  continue
      
      do 200 i=1,nver
         p_conf(i)=p_conf(i)/iver(i)
!         p_conf(i)=p_conf(i)/vsum(i)
 200  continue
      
      close(24)
      
      return
      end
