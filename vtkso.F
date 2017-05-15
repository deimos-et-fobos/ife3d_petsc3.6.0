!******************************************************************
!*
!*    CREACION DE FICHEROS PARA GRAFICACION EN EL SOLIDO
!*
!******************************************************************
      subroutine vtkso(nvers,us,usv,nev)

      implicit double precision (a-h,o-z)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"

      dimension us(*),usv(3,*)
      character*80 WorkDir,filename,Header
      character*4  StNev,Str4

      PetscInt       nev
   
      WorkDir = 'Sol/Solido/'
      StNev = Str4(nev)
   
      do 1 i=1,nvers
         usv(1,i)=us(3*(i-1)+1)
         usv(2,i)=us(3*(i-1)+2)
         usv(3,i)=us(3*(i-1)+3)
 1    continue   


!*
!******     Escritura en fichero de los desplazamientos del sólido
!*
      filename = trim(WorkDir) // 'desplazamiento-' // StNev // '.vec'
      write(*,*)'Writing: ',trim(filename)
      open(24,file=filename,form='UNFORMATTED',access='DIRECT',recl=4)

      Header='Desplazamiento'
      do 10 j=1,20   !Header   
        write(24,rec=j) header(j*4-3 : j*4)
 10   continue
      j=20
      do 11 i=1,nvers
        write(24,rec=j+1) real(usv(1,i))
        write(24,rec=j+2) real(usv(2,i))
        write(24,rec=j+3) real(usv(3,i))
        j=j+3
 11   continue
      close(24)

      if(nev.eq.0)then
        filename = trim(WorkDir) // 'par_gppos.cfg'
        open(27,file=filename)
        write(27,'(a)')  ' 1'        
        write(27,'(a)')  ' 3 Desplazamiento' 
        close(27)
      endif

      return
      end
