ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine errestf(nelf,nverf,mmf,ietf,jetf,jeef,zf,nnf,uf,
     &            ngf,nuf,componentes,zp,mmp,nrcof,irefcf,nrcf,ncacof,
     &            inodfp,numitap,x1p,y1p,x2p,y2p,x3p,y3p,zintp,indcp,
     &            ielp,dirp,smp,componentes_pz,componentes_prot,ndirf,
     &            ethau,jlf,uhf,ethaf,eigen,velsq,densf,volf,ethaf_aux,
     &            iver,ethaf_sum)
c
c     Objetivo: calcula el error  
c     ndirf --> nro de lados con cond de dir en el fluido
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"

      PetscScalar    eigen
      
      dimension mmf(4,*),ietf(*),jetf(*),jeef(*),zf(3,*),
     &            nnf(4,*),uf(*),ngf(4,*),componentes(4,*),zp(2,*),
     &            irefcf(*),nrcf(4,*),inodfp(*),x1p(*),y1p(*),x2p(*),
     &            y2p(*),x3p(*),y3p(*),zintp(*),indcp(*),ielp(*),
     &            componentes_pz(3,*),componentes_prot(2,3,*),ethau(*),
     &            uhf(*),ethaf(*),velsq(*),densf(*),smp(*),mmp(3,*)
      double precision nuf(3,*),jlf(*),jl,uh,cal_jlf,cal_uhf,
     &                  volf(*),ethaf_aux(*),jlf_max
      integer dirp(*),iver(*),el_ref(500000)
      
!*
!*****      calcular la matriz de conectividad jee (de elementos)
!*
      call caljee(4,mmf,ietf,jetf,jeef,nverf,nelf)

c
c     calcula las componente de cada triangulo (a+d x,b+d y,c+d z)
c
      call cal_componentesf(nelf,mmf,zf,nnf,uf,ngf,nuf,componentes,
     &                        volf)

      ethaf_sum = 0
      etha_max = 0
      uhf_max = 0
      jlf_max = 0
      umbral = 0.7

      do 10 i=1,nverf
         iver(i)=0
         ethaf_aux(i)=0.d0
 10   continue

      do 20 k=1,nelf
         jl=cal_jlf(k,mmf,nnf,zf,zp,mmp,componentes,jeef,nrcof,irefcf,
     &            nrcf,ncacof,inodfp,numitap,x1p,y1p,x2p,y2p,x3p,y3p,
     &            zintp,indcp,uf,ielp,dirp,smp,componentes_pz,
     &            componentes_prot,ndirf,nuf)

         uh=cal_uhf(mmf(1,k),zf,componentes(1,k),volf(k))

!         ethaf(k)=dsqrt(jl)
!         ethaf(k)=dsqrt(uh)
         uhf(k)=uh*(eigen**2)
         jlf(k)=jl*(velsq(1)**4)
!         ethaf(k) = dsqrt(jlf(k)+uhf(k))
         ethaf(k) = dsqrt(jlf(k))
!         ethaf(k) = dsqrt(uhf(k))
         
         ethaf_sum = ethaf_sum + ethaf(k)**2
         
         if (ethaf(k).gt.etha_max) etha_max = ethaf(k)
         if (jlf(k).gt.jlf_max) jlf_max = jlf(k)
         if (uhf(k).gt.uhf_max) uhf_max = uhf(k)
         
         do 21 i = 1,4
            ethaf_aux(mmf(i,k)) = ethaf_aux(mmf(i,k)) + ethaf(k)
            iver(mmf(i,k)) = iver(mmf(i,k)) + 1
 21      continue

 20   continue
      
      ethaf_sum = dsqrt(ethaf_sum)

      do 30 i=1,nverf
         ethaf_aux(i) = ethaf_aux(i) / iver(i)
 30   continue
      
      nel_ref = 0
      do 40 k=1,nelf
         if (ethaf(k).gt.etha_max*umbral) then
            nel_ref = nel_ref + 1
            el_ref(nel_ref) = k
         endif
 40   continue
      
      call elem2ref(nel_ref,el_ref)
      
!      OPEN(74,FILE='fort.74',ACCESS='APPEND')
!      write(74,*) 'jlf_max = ',jlf_max
!      write(74,*) 'uhf_max = ',uhf_max
!      CLOSE(74)
      
      open(24,file='errores.sol00')
      write(24,'(a)')  '*NOD'        
      write(24,*) nelf*4
      write(24,'(a)')  '*SCALAR_FIELD' 
      write(24,'(a)')  ' Continuation parameter: 0' 
      write(24,'(a)') '<NONE>'  
      do  i=1,nelf
         do j=1,4
            write(24,*) jlf(i)
            write(24,*) uhf(i)
            write(24,*) ethaf(i)
            write(24,*)componentes(1,i)+componentes(4,i)*zf(1,mmf(j,i))
            write(24,*)componentes(2,i)+componentes(4,i)*zf(2,mmf(j,i))
            write(24,*)componentes(3,i)+componentes(4,i)*zf(3,mmf(j,i))
         enddo
      enddo
      close(24)

      return
      end
