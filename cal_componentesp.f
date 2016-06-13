cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                      c
      subroutine cal_componentesp(nelp,mmp,zp,upv,beta_l,
     &                              componentes_pz,componentes_prot)
c                                                                      c
c     Objetivo: calcula las componentes de las u                       c
c                                                                      c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit double precision (a-h,o-z)

      dimension upv(*),beta_l(2,*),componentes_pz(3,*),mmp(3,*),
     &            componentes_prot(2,3,*),zp(2,*),p(2,3),
     &            solucion(3),valor(3)
      double precision matriz(3,3)
      integer nelp
!      OPEN(77,file='COMPONENTESP')
      do 10 k=1,nelp                         ! loop para todos los elementos

         do 23 j=1,3
            matriz(j,1)= 1                            ! alfa
            matriz(j,2)= zp(1,mmp(j,k))               ! beta
            matriz(j,3)= zp(2,mmp(j,k))               ! gamma
 23      continue

         do 30 j=1,3
            solucion(j)= upv(mmp(j,k))
 30      continue
         call cramer3x3(matriz,solucion,valor)
         do 31 j=1,3
            componentes_pz(j,k)=valor(j)
 31      continue

         do 40 j=1,3
            solucion(j)= beta_l(1,mmp(j,k))
 40      continue
         call cramer3x3(matriz,solucion,valor)
         do 41 j=1,3
            componentes_prot(1,j,k)=valor(j)
 41      continue

         do 50 j=1,3
            solucion(j)= beta_l(2,mmp(j,k))
 50      continue
         call cramer3x3(matriz,solucion,valor)
         do 51 j=1,3
            componentes_prot(2,j,k)=valor(j)
 51      continue
!         write(77,*) (componentes_pz(j,k),j=1,3)
!         write(77,*) (componentes_prot(1,j,k),j=1,3)
!         write(77,*) (componentes_prot(2,j,k),j=1,3)
 10   continue
!      close(77)
      return
      end
