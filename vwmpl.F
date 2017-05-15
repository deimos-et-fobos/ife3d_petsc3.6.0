!******************************************************************
!*
!*    CREACION DE FICHEROS PARA GRAFICACION EN EL SOLIDO
!*
!******************************************************************

      subroutine vwmpl(nelp,nverp,narp,mmp,nnp,cnum,ngp,
     &               zp,up,upv,beta_l,rot,smp,thin,nev)

      implicit double precision (a-h,o-z)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"

      dimension mmp(3,*),nnp(6,*),zp(2,*),up(*),upv(*),beta_l(2,*),
     &      tau(2),coord(2),ngp(3,*),rot(2,*),ver(2,3),smp(*),thin(*)
      double precision med,thin2,smp2
      integer cnum(*),sig
      character*80 WorkDir,filename
      character*3  StNev, Str3
      
      PetscInt       nev

      thin2=0.0
      smp2=0.0
      WorkDir = 'Sol/Placa/'
      StNev = Str3(nev)

!*
!******     Escritura en fichero de las rotaciones (parte cuadratica)
!*    
!      filename = trim(WorkDir) // 'placa' // StNev // '.cuad.vwm'
!      ilen=len_trim(filename)
!      write(*,*)'Escribiendo ',filename(1:ilen)
!      open(16,file=filename)
!      write(16,'(a)')  '*DIMENSION'
!      write(16,*) 2
!      write(16,*)
      
!      write(16,'(a)')  '*COORDINATES'
!      write(16,*) nverp+narp  
      do  i=1,nverp+narp
         do 1000 k=1,nelp
            do 1100 j=1,3
               if (nnp(j,k).eq.i) then
                  upv(mmp(j,k))=up(cnum(i)+1)
                  
                  beta_l(1,mmp(j,k))=up(cnum(i)+2)
                  rot(1,i)=up(cnum(i)+2)
            
                  beta_l(2,mmp(j,k))=up(cnum(i)+3)
                  rot(2,i)=up(cnum(i)+3)
            
!                  write(16,*) i,zp(1,mmp(j,k)),zp(2,mmp(j,k))
                  goto 6666
               endif
               if (nnp(j+3,k).eq.i) then
                  ! Calculo la tangente al lado
                  do 1200 l=1,3
                     ver(1,l)=zp(1,mmp(l,k))
                     ver(2,l)=zp(2,mmp(l,k))
 1200             continue
                  tau(1)=ver(1,1+mod(j,3))-ver(1,j)
                  tau(2)=ver(2,1+mod(j,3))-ver(2,j)
                  med=dsqrt(tau(1)**2+tau(2)**2)
                  tau(1)=tau(1)
                  tau(2)=tau(2)

                  if (ngp(j,k).eq.1) then
                     sig= 1
                  else
                     sig=-1
                  endif

                  rot(1,i)=up(cnum(i)+1)*tau(1)*sig+
     &                           (up(cnum(nnp(1+mod(j,3),k))+2)+
     &                              up(cnum(nnp(j,k))+2))/2.d0
                  rot(2,i)=up(cnum(i)+1)*tau(2)*sig+
     &                           (up(cnum(nnp(1+mod(j,3),k))+3)+
     &                              up(cnum(nnp(j,k))+3))/2.d0

                  coord(1)=(ver(1,1+mod(j,3))+ver(1,j))/2.d0
                  coord(2)=(ver(2,1+mod(j,3))+ver(2,j))/2.d0
                  
!                  write(16,*) i,coord(1),coord(2)
                  goto 6666
               endif
 1100       continue
 1000    continue
 6666    continue
      enddo
      
!      write(16,*)
!      write(16,'(a)')  '*ELEMENT_GROUPS'
!      write(16,*) 1
!      write(16,*) 1,nelp,' Tri6'
!      write(16,*)

!      write(16,'(a)')  '*INCIDENCE'
!      write(16,'(a)') '<NONE>'  
!      do i=1,nelp
!         write(16,*) (nnp(j,i),j=1,6)
!      enddo
!      write(16,*)

!      write(16,'(a)')  '*VECTOR_FIELDS'
!      write(16,'(a)')  ' 1 1'
!      write(16,'(a)')  '<TEST>'
!      do i=1,nverp+narp
!         write(16,*) i,rot(1,i),rot(2,i)
!      enddo
!      write(16,*)
   
!      write(16,'(a)')  '*END'
!      close(16)

!***********************************************************************
      fact=1.d0
!***********************************************************************

!*
!******     Escritura en fichero de los desplazamientos
!*
!      filename = trim(WorkDir) // 'placa' // StNev // '.desp.vwm'
!      ilen=len_trim(filename)
!      write(*,*)'Escribiendo ',filename(1:ilen)
!      open(14,file=filename)
!      write(14,'(a)')  '*DIMENSION'
!      write(14,*) 2
!      write(14,*)
      
!      write(14,'(a)')  '*COORDINATES'
!      write(14,*) nverp  
!      do i=1,nverp
!         write(14,*) i,zp(1,i),zp(2,i)
!      enddo
!      write(14,*)

!      write(14,'(a)')  '*ELEMENT_GROUPS'
!      write(14,*) 1
!      write(14,*) 1,nelp,' Tri3'
!      write(14,*)
      
!      write(14,'(a)')  '*INCIDENCE'
!      write(14,'(a)') '<NONE>'  
!      do i=1,nelp
!         write(14,*) mmp(1,i),mmp(2,i),mmp(3,i)
!      enddo
!      write(14,*)

!      write(14,'(a)')  '*SCALAR_FIELDS'
!      write(14,'(a)')  ' 1'
!      write(14,'(a)')  '<TEST>'
!      do i=1,nverp
!         write(14,*) i,upv(i)
!      enddo
!      write(14,*)

!      write(14,'(a)')  '*END'
!      close(14)
 
!*
!******     Escritura en fichero de las rotaciones (parte lineal)
!*    
!      filename = trim(WorkDir) // 'placa' // StNev // '.rot.vwm'
!      ilen=len_trim(filename)
!      write(*,*)'Escribiendo ',filename(1:ilen)
!      open(15,file=filename)
!      write(15,'(a)')  '*DIMENSION'
!      write(15,*) 2
!      write(15,*)
   
!      write(15,'(a)')  '*COORDINATES'
!         write(15,*) nverp  
!      do i=1,nverp
!         write(15,*) i,zp(1,i),zp(2,i)
!      enddo
!      write(15,*)

!      write(15,'(a)')  '*ELEMENT_GROUPS'
!      write(15,*) 1
!      write(15,*) 1,nelp,' Tri3'
!      write(15,*)

!      write(15,'(a)')  '*INCIDENCE'
!      write(15,'(a)') '<NONE>'  
!      do i=1,nelp
!         write(15,*) mmp(1,i),mmp(2,i),mmp(3,i)
!      enddo
!      write(15,*)

!      write(15,'(a)')  '*VECTOR_FIELDS'
!      write(15,'(a)')  ' 1 1'
!      write(15,'(a)')  '<TEST>'
!      do i=1,nverp
!         write(15,*) i,beta_l(1,i),beta_l(2,i)
!      enddo
!      write(15,*)

!      write(15,'(a)')  '*END'
!      close(15)

!*
!******     Escritura de la malla VWM de la placa en 3D
!*   
      if(nev.eq.0) then
      
      filename = trim(WorkDir) // 'placa3D.vwm'
      ilen=len_trim(filename)
      write(*,*)'Escribiendo ',filename(1:ilen)
      open(17,file=filename)
      write(17,'(a)')  '*COORDINATES'
      write(17,*) 2*nverp  
      do 401 i=1,nverp
         do 402 j=1,nelp
            do 403 k=1,3
               if(i.eq.mmp(k,j))then
                  smp2=smp(j)
                  thin2=thin(j)
                  goto 641
               endif
 403        continue
 402     continue
 641     continue
         write(17,*) i,zp(1,i),zp(2,i),smp2-thin2/2
 401  continue
      do 404 i=1,nverp
         do 405 j=1,nelp
            do 406 k=1,3
               if(i.eq.mmp(k,j))then
                  smp2=smp(j)
                  thin2=thin(j)
                  goto 642
               endif
 406        continue
 405     continue
 642     continue
         write(17,*) i+nverp,zp(1,i),zp(2,i),smp2+thin2/2
 404  continue
      write(17,*)
   
      write(17,'(a)')  '*ELEMENT_GROUPS'
      write(17,*) 1
      write(17,*) 1,3*nelp,' Tetra4'
      write(17,*)
   
      write(17,'(a)')  '*INCIDENCE'
      write(17,'(a)') '<NONE>'  
      do i=1,nelp
         write(17,*) mmp(1,i),mmp(2,i),mmp(3,i),mmp(1,i)+nverp
         write(17,*) mmp(1,i)+nverp,mmp(2,i),mmp(3,i)+nverp,
     &               mmp(2,i)+nverp
         write(17,*) mmp(1,i)+nverp,mmp(3,i)+nverp,mmp(2,i),mmp(3,i)
      enddo
      write(17,*)
      
      write(17,'(a)')  '*END'
      close(17)

      endif

!*
!******     Escritura en fichero formato vtk
!*
      filename = trim(WorkDir) // 'placa' // StNev // '.sol000'
      ilen=len_trim(filename)
      write(*,*)'Escribiendo ',filename(1:ilen)
      open(24,file=filename)
      write(24,'(a)')  '*NOD'        
      write(24,*) 2*nverp
      write(24,'(a)')  '*SCALAR_FIELD' 
      write(24,*)  'Continuation parameter: ', nev
      write(24,'(a)') '<NONE>'  
      do 501 i=1,nverp
         do 502 j=1,nelp
            do 503 k=1,3
               if(i.eq.mmp(k,j))then
                  smp2=smp(j)
                  thin2=thin(j)
                  goto 651
               endif
 503        continue
 502     continue
 651     continue
         write(24,*) -thin2/2*beta_l(1,i)
         write(24,*) -thin2/2*beta_l(2,i)
         write(24,*) upv(i)
 501  continue
      do 504 i=1,nverp
         do 505 j=1,nelp
            do 506 k=1,3
               if(i.eq.mmp(k,j))then
                  smp2=smp(j)
                  thin2=thin(j)
                  goto 652
               endif
 506        continue
 505     continue
 652     continue
         write(24,*) thin2/2*beta_l(1,i)
         write(24,*) thin2/2*beta_l(2,i)
         write(24,*) upv(i)
 504  continue
      close(24)

      if(nev.eq.0) then
        filename = trim(WorkDir) // 'gppospl.cfg'
        open(27,file=filename)
        write(27,'(a)')  ' 1'        
        write(27,'(a)')  ' 3 Desplazamiento'
        close(27)
      endif

!*
!******     Escritura de resultados crudos
!*
!      filename = trim(WorkDir) // 'placa.raw' // StNev
!      ilen=len_trim(filename)
!      write(*,*)'Escribiendo ',filename(1:ilen)
!      open(28,file=filename)
!      do 511 i=1,3*nverp+narp
!         write(28,*) up(i)
! 511  continue
!      close(28)

      return
      end
   
      FUNCTION Str3(n)
      CHARACTER*3 Str3, A
         WRITE (A, 91) n
         Str3 = A
 91      FORMAT (I3.3)
      RETURN
      END
