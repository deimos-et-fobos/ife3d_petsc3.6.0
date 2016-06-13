!******************************************************************
!*											*
!*	CREACION DE FICHEROS PARA GRAFICACION EN EL FLUIDO		*
!*											*
!******************************************************************

      subroutine vwmfl(nverf,nglf,uf,ufv,p,p_conf,eta,nev)

      implicit double precision (a-h,o-z)

#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscmat.h"
      dimension uf(*),ufv(3,*),p(*),eta(*),p_conf(*)
      character*80 WorkDir,filename
      character*3  StNev, Str3
      
      PetscInt       nev

      WorkDir = 'Sol/Fluido/'
      StNev = Str3(nev)
      filename = trim(WorkDir) // 'fluido' // StNev // '.sol000'
      ilen=len_trim(filename)
      write(*,*)'Escribiendo ',filename(1:ilen)
      
!*	
!******	Escritura en fichero de los desplazamientos en el solido.
!*
      open(24,file=filename)
      write(24,'(a)')  '*NOD'        
      write(24,*) nverf
      write(24,'(a)')  '*SCALAR_FIELD' 
      write(24,*)  'Continuation parameter: ', nev
      write(24,'(a)') '<NONE>'  
      do  i=1,nverf
         write(24,*) eta(i)
         write(24,*) p(i)
         write(24,*) p_conf(i)
         write(24,*) ufv(1,i)
         write(24,*) ufv(2,i)
!         write(24,*) 0
!         write(24,*) 0
         write(24,*) ufv(3,i)
      enddo
      close(24)

      filename = trim(WorkDir) // 'gpposfl.cfg'
      open(27,file=filename)
      write(27,'(a)')  ' 4'        
      write(27,'(a)')  ' 1 Error'
      write(27,'(a)')  ' 1 Presion' 
      write(27,'(a)')  ' 1 Pconf' 
      write(27,'(a)')  ' 3 Desplazamiento' 
      close(27)
      
!*
!******     Escritura de resultados crudos
!*
!      filename = trim(WorkDir) // 'fluido.raw' // StNev
!      ilen=len_trim(filename)
!      write(*,*)'Escribiendo ',filename(1:ilen)
!      open(28,file=filename)
!      do 511 i=1,nglf
!         write(28,*) uf(i)
! 511  continue
!      close(28)
      
      return
      end
