!******************************************************************
!*
!*    CREACION DE FICHEROS PARA GRAFICACION EN EL SOLIDO
!*
!******************************************************************
      subroutine vwmso(nvers,us,usv,nev)

      implicit double precision (a-h,o-z)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"

      dimension us(*),usv(3,*)
      character*80 WorkDir,filename
      character*3  StNev, Str3

      PetscInt       nev
   
      WorkDir = 'Sol/Solido/'
      StNev = Str3(nev)
   
      do 11 i=1,nvers
         usv(1,i)=us(3*(i-1)+1)
         usv(2,i)=us(3*(i-1)+2)
         usv(3,i)=us(3*(i-1)+3)
 11   continue   


!*
!******     Escritura en fichero de los desplazamientos del sólido
!*
      filename = trim(WorkDir) // 'solido' // StNev // '.sol000'
      ilen=len_trim(filename)
      write(*,*)'Escribiendo ',filename(1:ilen)
      open(24,file=filename)
      write(24,'(a)')  '*NOD'        
      write(24,*) nvers
      write(24,'(a)')  '*SCALAR_FIELD' 
      write(24,*)  'Continuation parameter: ', nev 
      write(24,'(a)') '<NONE>'  
      do  i=1,nvers
         write(24,*) usv(1,i)
         write(24,*) usv(2,i)
!         write(24,*) 0
!         write(24,*) 0
         write(24,*) usv(3,i)
      enddo
      close(24)
 
      filename = trim(WorkDir) // 'gpposso.cfg'
      open(27,file=filename)
      write(27,'(a)')  ' 1'        
      write(27,'(a)')  ' 3 Desplazamiento' 
      close(27)
 
      return
      end
