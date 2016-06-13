
      subroutine move_fluido(mm,mmsur,z,dx,dy,dz)

      IMPLICIT double precision (a-h,o-z)
      parameter (maxgroups=1000)
      
      CHARACTER*80 FILEVWM,FILESUR,VWMNEW,SURNEW
      dimension mm(4,*),mmsur(3,*),z(3,*)
      integer el_gr(maxgroups),sur_gr(maxgroups),el_ngrs,sur_ngrs

!     abre archivo VWM
      WRITE (6,*) "Archivo vwm: "
      READ  (5,*) FILEVWM
      OPEN  (11,FILE=FILEVWM,STATUS='OLD',ERR=10)

!     abre archivo SUR
      WRITE (6,*) "Archivo sur: "
      READ  (5,*) FILESUR
      open  (12,FILE=FILESUR,STATUS='OLD',ERR=10)

      call searstr(11,'COORDINATES')
      read(11,*) nver
      do 100 i=1,nver
        read(11,*) inutil,z(1,i),z(2,i),z(3,i)
 100  continue

      call searstr(11,'ELEMENT_GROUPS')
      read(11,*) el_ngrs
      nel=0
      do 200 i=1,el_ngrs
        read(11,*) inutil, el_gr(i)
        nel=nel+el_gr(i)
 200  continue

      call searstr(11,'INCIDENCE')
      read(11,*)
      do 300 i=1,nel
        read(11,*) mm(1,i),mm(2,i),mm(3,i),mm(4,i)
 300  continue

      call searstr(12,'ELEMENT_GROUPS')
      read(12,*)sur_ngrs
      nelsur=0
      do 400 i=1,sur_ngrs
        read(12,*) inutil, sur_gr(i)
        nelsur=nelsur+sur_gr(i)
 400  continue  

      call searstr(12,'INCIDENCE')
      read(12,*)
      do 500 i=1,nelsur
        read(12,*) mmsur(1,i),mmsur(2,i),mmsur(3,i)
 500  continue

!     Escritura de archivos nuevos
      VWMNEW = trim(FILEVWM) // '.new'
      SURNEW = trim(FILESUR) // '.new'
      open(14,file=VWMNEW)
      open(15,file=SURNEW)
      
      write(14,'(a)') '*COORDINATES'
      write(15,'(a)') '*COORDINATES'
      write(14,*) nver
      write(15,*) nver
      do 1000 i=1,nver
        write(14,*) i,z(1,i)+dx,z(2,i)+dy,z(3,i)+dz
        write(15,*) i,z(1,i)+dx,z(2,i)+dy,z(3,i)+dz
 1000 continue

      write(14,'(a)') '*ELEMENT_GROUPS'
      write(14,*) el_ngrs
      do 2000 i=1,el_ngrs
        write(14,123) i,el_gr(i),'Tetra4'
 2000 continue
 123  format(1x,i3,1x,i9,1x,a6)

      write(14,'(a)') '*INCIDENCES' 
      write(14,'(a)') '<NONE>'
      do 3000 i=1,nel
        write(14,*) mm(1,i),mm(2,i),mm(3,i),mm(4,i)
 3000 continue

      write(15,'(a)') '*ELEMENT_GROUPS'
      write(15,*) sur_ngrs
      do 4000 i=1,sur_ngrs
        write(15,123) i,sur_gr(i),'Tri3  '
 4000 continue

      write(15,'(a)') '*INCIDENCES' 
      write(15,'(a)') '<NONE>'
      do 5000 i=1,nelsur
        write(15,*) mmsur(1,i),mmsur(2,i),mmsur(3,i)
 5000 continue

      write(14,'(a)') '*END'
      write(15,'(a)') '*END'

      close(11)
      close(12)
      close(14)
      close(15)

      return

 10   write (6,*) "Error al abrir el archivo de preprocesador"
      STOP
      end







