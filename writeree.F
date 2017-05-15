!-----------------------------------------------------------------------
!	Function WriteRee - write restart file

!	Its function is to write the values of a sequential PETSc vector 
!	to a file.
!	Only one processor has to call it.
!	Also writes "headings and footnotes".

!	Parameters:
!	nev: eigenvalue number of the mode 
!	eigen: eigenvalue.      (Heading, info)
!	IOvec: vector to save
!	nglp: # dof in the plate
!	ngls: # dof in the solid
!	nglf: # dof in the fluid
!	ncaco: # dof in the contact surface (P)
!	pslc: number that indicates if there is plate, solid, fluid 
!		or/and contact in the problem
!-----------------------------------------------------------------------

      subroutine writeree(nev,eigen,IOvec,IOvecScan,
     &                        nglp,ngls,nglf,ncaco,pslc)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"

      double precision eigen
      character*3  StNev, Str3
      character*80 filename,WorkDir
      integer dof,nev,ierr,length,nglp,ngls,nglf,ncaco,ilen,pslc
      integer ini(4),fin(4)

      Vec            IOvec
      PetscScalar    IOvecScan(*)
      PetscInt       nev,length

      VecGetSize(IOvec, length, ierr)
      VecGetArray(IOvec, IOvecScan, ierr)
   
      ini(1) = 0
      ini(2) = ini(1) + nglp
      ini(3) = ini(2) + ngls
      ini(4) = ini(3) + nglf
      fin(1) = mod(pslc,1000)*nglp
      fin(2) = fin(1) + mod(pslc,100)*ngls
      fin(3) = fin(2) + mod(pslc,10)*nglf
      fin(4) = fin(3) + mod(pslc,1)*ncaco

      if (fin(1).gt.ini(1)) then
         WorkDir = 'Sol/Placa'
         StNev = Str3(i - 1)
         filename = trim(WorkDir) // 'placa.sol' // StNev // '.dat'
         ilen=len_trim(filename)
!     write(*,*)'Escribiendo ',filename(1:ilen)
         open(1,file=filename,status='unknown',err=666)
         write(1,'(a)') '*NOD'
         write(1,*) nglp
         write(1,'(a)') '*SCALAR_FIELD'
         write(1,90) 'Continuation parameter: ', eigen
         write(1,'(a)') '<NONE>'
         do 11 i=ini(1),fin(1)
            write(1,*) IOvecScan(i)
 11      continue
         close(1)
      endif
      if (fin(2).gt.ini(2)) then
         WorkDir = 'Sol/Solido'
         StNev = Str3(i - 1)
         filename = trim(WorkDir) // 'solido.sol' // StNev // '.dat'
         ilen=len_trim(filename)
!     write(*,*)'Escribiendo ',filename(1:ilen)
         open(1,file=filename,status='unknown',err=666)
         write(1,'(a)') '*NOD'
         write(1,*) nglp
         write(1,'(a)') '*SCALAR_FIELD'
         write(1,90) 'Continuation parameter: ', eigen
         write(1,'(a)') '<NONE>'
         do 12 i=ini(2),fin(2)
            write(1,*) IOvecScan(i)
 12      continue
         close(1)
      endif
      if (fin(3).gt.ini(3)) then
         WorkDir = 'Sol/Fluido'
         StNev = Str3(i - 1)
         filename = trim(WorkDir) // 'fluido.sol' // StNev // '.dat'
         ilen=len_trim(filename)
!     write(*,*)'Escribiendo ',filename(1:ilen)
         open(1,file=filename,status='unknown',err=666)
         write(1,'(a)') '*NOD'
         write(1,*) nglp
         write(1,'(a)') '*SCALAR_FIELD'
         write(1,90) 'Continuation parameter: ', eigen
         write(1,'(a)') '<NONE>'
         do 13 i=ini(3),fin(3)
            write(1,*) IOvecScan(i)
 13      continue
         close(1)
      endif
      if (fin(4).gt.ini(4)) then
         WorkDir = 'Sol/Placa'
         StNev = Str3(i - 1)
         filename = trim(WorkDir) // 'interfase.sol' // StNev // '.dat'
         ilen=len_trim(filename)
!      write(*,*)'Escribiendo ',filename(1:ilen)
         open(1,file=filename,status='unknown',err=666)
         write(1,'(a)') '*NOD'
         write(1,*) nglp
         write(1,'(a)') '*SCALAR_FIELD'
         write(1,90) 'Continuation parameter: ', eigen
         write(1,'(a)') '<NONE>'
         do 14 i=ini(4),fin(4)
            write(1,*) IOvecScan(i)
 14      continue
         close(1)
      endif

      VecRestoreArray(IOvec, IOvecScan, ierr)

      return

 90   format(a25,f16.8)
 666  write (6,*) "Error al abrir el archivo de preprocesador"
      stop

      end
   
      FUNCTION Str3(n)
      CHARACTER*3 Str3, A
         WRITE (A, 91) n
         Str3 = A
 91      FORMAT (I3.3)
      RETURN
      END
