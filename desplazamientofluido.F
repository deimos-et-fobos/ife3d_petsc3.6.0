      subroutine desplazamientofluido(nelf,mmf,zf,nnf,uf,ngf,nuf,
     &            componentes,volf,div,nev)


      implicit double precision (a-h,o-z)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"
      
      dimension mmf(4,*),zf(3,*),nnf(4,*),uf(*),ngf(4,*),
     &          componentes(4,*),div(*)
      double precision nuf(3,*),volf(*)
      character*80 WorkDir,filename
      character*3  StNev, Str3
      
      PetscInt       nev

c     calcula las componente de cada triangulo (a+d x,b+d y,c+d z)
      call cal_componentesf(nelf,mmf,zf,nnf,uf,ngf,nuf,componentes,
     &                        volf)

      WorkDir = 'Sol/Fluido/'
      StNev = Str3(nev)
      filename = trim(WorkDir) // 'desp' // StNev // '.sol000'
      ilen=len_trim(filename)
      write(*,*)'Escribiendo ',filename(1:ilen)
      open(24,file=filename)
      write(24,'(a)')  '*NOD'        
      write(24,*) nelf*4
      write(24,'(a)')  '*SCALAR_FIELD' 
      write(24,*)  ' Continuation parameter: ', nev 
      write(24,'(a)') '<NONE>'  
      do  i=1,nelf
         do j=1,4
            write(24,*)componentes(1,i)+componentes(4,i)*zf(1,mmf(j,i))
            write(24,*)componentes(2,i)+componentes(4,i)*zf(2,mmf(j,i))
!            write(24,*) 0
!            write(24,*) 0
            write(24,*)componentes(3,i)+componentes(4,i)*zf(3,mmf(j,i))
            write(24,*)div(i)
         enddo
      enddo
      close(24)

      return
      end
