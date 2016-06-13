cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                      c
      subroutine cal_componentesf(nelf,mmf,zf,nnf,uf,ngf,nuf,
     &            componentes,vol)
c                                                                      c
c     Objetivo: calcula las componentes de las u                       c
c                                                                      c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit double precision (a-h,o-z)

      dimension solucion(4),zf(3,*),uf(*),uv(3),vv(3),
     &            componentes(4,*),mmf(4,*),nnf(4,*),ngf(4,*),vol(*)
      integer nelf,cn(4,4)

      cn(1,1)=2; cn(1,2)=3; cn(1,3)=4; cn(1,4)=1
      cn(2,1)=3; cn(2,2)=4; cn(2,3)=1; cn(2,4)=2
      cn(3,1)=4; cn(3,2)=1; cn(3,3)=2; cn(3,4)=3
      cn(4,1)=1; cn(4,2)=2; cn(4,3)=3; cn(4,4)=4

!      OPEN(76,file='COMPONENTESF')
      do k=1,nelf                         ! loop para todos los elementos

         do 20 j=1,4
            componentes(j,k) = 0.0
 20      continue

         do 21 j=1,4
            do 22 i=1,3
               uv(i)=zf(i,mmf(cn(j,2),k))-zf(i,mmf(cn(j,1),k))
               vv(i)=zf(i,mmf(cn(j,3),k))-zf(i,mmf(cn(j,1),k))
 22         continue   
            area=dabs(dsqrt((uv(2)*vv(3)-uv(3)*vv(2))**2+(uv(3)*vv(1)-
     &             uv(1)*vv(3))**2+(uv(1)*vv(2)-uv(2)*vv(1))**2))/2.d0
            aux = uf(nnf(j,k)) * ngf(j,k) * area / (3 * vol(k))
            componentes(1,k) = componentes(1,k) - 
     &                         aux * zf(1,mmf(cn(j,4),k))
            componentes(2,k) = componentes(2,k) - 
     &                         aux * zf(2,mmf(cn(j,4),k))
            componentes(3,k) = componentes(3,k) - 
     &                         aux * zf(3,mmf(cn(j,4),k))
            componentes(4,k) = componentes(4,k) + aux
 21      continue
!         write(76,*) (componentes(j,k),j=1,4)
      enddo
!      close(76)
      return
      end
