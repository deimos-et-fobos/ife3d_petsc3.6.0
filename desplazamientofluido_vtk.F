      subroutine desplazamientofluido_vtk(nelf,mmf,zf,nnf,uf,ngf,nuf,
     &            componentes,volf,div,nev)


      implicit double precision (a-h,o-z)
#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      
      dimension mmf(4,*),zf(3,*),nnf(4,*),uf(*),ngf(4,*),
     &          componentes(4,*),div(*)
      double precision nuf(3,*),volf(*)
      character*80 WorkDir,filename,Header
      character*4  StNev, Str4
      
      PetscInt       nev

c     calcula las componente de cada triangulo (a+d x,b+d y,c+d z)
      call cal_componentesf(nelf,mmf,zf,nnf,uf,ngf,nuf,componentes,
     &                        volf)

      WorkDir = 'Sol/Fluido/'
      StNev = Str4(nev)

      filename=trim(WorkDir)//'desplazamiento_fluido-'//StNev//'.vec'
      write(*,*)'Writing: ',trim(filename)
      open(24,file=filename,form='UNFORMATTED',access='DIRECT',recl=4)
      Header='Desplazamiento'
      do 10 j=1,20   !Header   
        write(24,rec=j) Header(j*4-3 : j*4)
 10   continue
      j=20
      do 11 i=1,nelf
        do 12 k=1,4
          write(24,rec=j+1) real(componentes(1,i) + 
     &                           componentes(4,i)*zf(1,mmf(k,i)))
          write(24,rec=j+2) real(componentes(2,i) + 
     &                           componentes(4,i)*zf(2,mmf(k,i)))
          write(24,rec=j+3) real(componentes(3,i) + 
     &                           componentes(4,i)*zf(3,mmf(k,i)))
          j=j+3
 12     continue
 11   continue
      close(24)

      filename = trim(WorkDir) // 'divergencia-' // StNev // '.scl'
      write(*,*)'Writing: ',trim(filename)
      open(24,file=filename,form='UNFORMATTED',access='DIRECT',recl=4)
      Header='Divergencia'
      do 20 j=1,20   !Header   
        write(24,rec=j) Header(j*4-3 : j*4)
 20   continue
      j=20
      do 21 i=1,nelf
        do 22 k=1,4
          write(24,rec=j+1) real(div(i))
          j=j+1
 22     continue
 21   continue
      close(24)

      if(nev.eq.0)then
        filename = trim(WorkDir) // 'gpposerr.cfg'
        open(27,file=filename)
        write(27,'(a)')  ' 2'
        write(27,'(a)')  ' 3 Desplazamiento'
        write(27,'(a)')  ' 1 Divergencia'
        close(27)
      endif

      return
      end
