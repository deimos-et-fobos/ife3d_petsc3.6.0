!******************************************************************
!*											*
!*	CREACION DE FICHEROS PARA GRAFICACION EN EL FLUIDO		*
!*											*
!******************************************************************

      subroutine vtkfl(nverf,uf,ufv,p,p_conf,eta,nev)

      implicit double precision (a-h,o-z)

#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"
      dimension uf(*),ufv(3,*),p(*),eta(*),p_conf(*)
      character*80 WorkDir,filename,Header
      character*4  StNev, Str4
      
      PetscInt       nev

      WorkDir = 'Sol/Fluido/'
      StNev = Str4(nev)
      
!*	
!******	Escritura en fichero de los desplazamientos en el solido.
!*
      filename = trim(WorkDir) // 'error-' // StNev // '.scl'
      write(*,*)'Writing: ',trim(filename)
      open(24,file=filename,form='UNFORMATTED',access='DIRECT',recl=4)
      Header='Error'
      do 10 j=1,20   !Header   
        write(24,rec=j) header(j*4-3 : j*4)
 10   continue
      j=20
      do 11 i=1,nverf
        write(24,rec=j+1) real(eta(i))
        j=j+1
 11   continue
      close(24)

      filename = trim(WorkDir) // 'presion-' // StNev // '.scl'
      write(*,*)'Writing: ',trim(filename)
      open(24,file=filename,form='UNFORMATTED',access='DIRECT',recl=4)
      Header='Presion'
      do 20 j=1,20   !Header   
        write(24,rec=j) header(j*4-3 : j*4)
 20   continue
      j=20
      do 21 i=1,nverf
        write(24,rec=j+1) real(p(i))
        j=j+1
 21   continue
      close(24)

      filename = trim(WorkDir) // 'pconf-' // StNev // '.scl'
      write(*,*)'Writing: ',trim(filename)
      open(24,file=filename,form='UNFORMATTED',access='DIRECT',recl=4)
      Header='Pconf'
      do 30 j=1,20   !Header   
        write(24,rec=j) header(j*4-3 : j*4)
 30   continue
      j=20
      do 31 i=1,nverf
        write(24,rec=j+1) real(p_conf(i))
        j=j+1
 31   continue
      close(24)

      filename = trim(WorkDir) // 'desplazamiento-' // StNev // '.vec'
      write(*,*)'Writing: ',trim(filename)
      open(24,file=filename,form='UNFORMATTED',access='DIRECT',recl=4)
      Header='Desplazamiento'
      do 40 j=1,20   !Header   
        write(24,rec=j) header(j*4-3 : j*4)
 40   continue
      j=20
      do 41 i=1,nverf
        write(24,rec=j+1) real(ufv(1,i))
        write(24,rec=j+2) real(ufv(2,i))
        write(24,rec=j+3) real(ufv(3,i))
        j=j+3
 41   continue
      close(24)
      
      return
      end
