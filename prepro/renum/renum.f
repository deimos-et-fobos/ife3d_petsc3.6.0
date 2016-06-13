CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Preprocesamiento de mallas. Condensa los element_groups a 1
C           para utilizar en el enredo y renumerar los nodos
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      program renum
      
      IMPLICIT double precision (A-H,O-Z)
      integer nodmax
      parameter (nodmax=100000000)
      CHARACTER*60 MALLA, NEW, REC, tipo
      dimension z(3),mm(4),NSD(nodmax)
      integer coord,vertices, trash, el_ngr, aux
c
c	abre archivo VWM
c
      READ  (5,*) MALLA
      OPEN  (1,FILE=MALLA,STATUS='OLD',ERR=10)

c
c	abre archivo placa3.vwm o fluidorenum.vwm
c


      if (MALLA(1:9).eq.'placa.vwm') then
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Lee el archivo VWM y guarda en liquido4.vwm
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         OPEN  (2,FILE='placa3.vwm',STATUS='UNKNOWN',ERR=10)
         call searstr(1,'COORDINATES')
         read(1,*) nver
         write(2,'(A)') '*COORDINATES'
         write(2,*) nver
         do 6000 i=1,nver
            read(1,*) aux,(z(j),j=1,2)
            write(2,*) aux,(z(j),j=1,2)
 6000    continue
      
         call searstr(1,'ELEMENT_GROUPS')
         read(1,*) el_ngr
         nel=0
         do 6100 i=1,el_ngr
            read(1,*) trash, aux, REC
            nel=nel+aux
 6100    continue
 
         write(2,'(A)') '*ELEMENT_GROUPS'
         write(2,'(A)') ' 1'
         write(2,'(A,I7,1x,A6)') ' 1', nel, ' Tri3'

         call searstr(1,'INCIDENCE')
         read(1,*)
         write(2,'(A)') '*INCIDENCE'
         write(2,'(A)') '<NONE>'
         do 6200 i=1,nel
            read(1,*) (mm(j),j=1,3)
            write(2,*) (mm(j),j=1,3)
 6200    continue
 
         write(2,'(A)') '*END' ! Final

         close(1)
         close(2)         
      endif
      
      
      if (MALLA(1:15).eq.'fluidorenum.vwm') then
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C                 Modifica el archivo fluido.rt
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c         OPEN  (1,FILE='fluidorenum.vwm',STATUS='UNKNOWN',ERR=10)
         OPEN  (2,FILE='fluido.rt',STATUS='UNKNOWN',ERR=10)
         OPEN  (3,FILE='fluidonew.rt',STATUS='UNKNOWN',ERR=10)   
         
         call searstr(2,'COORDINATES')
         read(2,*) nver
         write(3,'(A)') '*COORDINATES'
         write(3,*) nver
         do 5000 i=1,nver
            read(2,*) aux,(z(j),j=1,3)
            write(3,*) aux,(z(j),j=1,3)
 5000    continue
         
         call searstr(2,'ELEMENT_GROUPS')
         read(2,*) el_ngr
         write(3,'(A)') '*ELEMENT_GROUPS'
         write(3,*) el_ngr
         nel=0
         do 5100 i=1,el_ngr
            read(2,*) trash, aux, REC
            write(3,*) i, aux, 'Tetra4'
            nel=nel+aux
 5100    continue
 
         call searstr(2,'INCIDENCE')
         read(2,*)
         write(3,'(A)') '*INCIDENCE'
         write(3,'(A)') '<NONE>'
         do 5200 i=1,nel
            read(2,*) (mm(j),j=1,4)
            write(3,*) (mm(j),j=1,4)
 5200    continue
 
         call searstr(2,'NFACES')
         read(2,*) aux
         write(3,'(A)') '*NFACES'
         write(3,*) aux

         call searstr(2,'FACE_INCIDENCE')
         write(3,'(A)') '*FACE_INCIDENCE'
         call searstr(1,'INCIDENCE')
         read(1,*)
         do 5300 i=1,nel
            read(2,*) 
            read(1,*) (mm(j),j=1,4)
            write(3,*) (mm(j),j=1,4)
 5300    continue
 
         call searstr(2,'NRC')
         write(3,'(A)') '*NRC'
         do 5400 i=1,nel
            read(2,*) (mm(j),j=1,4)
            write(3,*) (mm(j),j=1,4)
 5400    continue
         
         call searstr(2,'NSD')
         write(3,'(A)') '*NSD'
         read(2,*) (NSD(j),j=1,nel)
         write(3,*) (NSD(j),j=1,nel)
         
         write(3,'(A)') '*END'
  
         close(1)
         close(2)
         close(3)
      endif

      
	if (MALLA(1:10).eq.'solido.vwm') then
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Lee el archivo VWM y guarda en solido4.vwm
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         OPEN  (2,FILE='solido4.vwm',STATUS='UNKNOWN',ERR=10)
         call searstr(1,'COORDINATES')
         read(1,*) nver
         write(2,'(A)') '*COORDINATES'
         write(2,*) nver
         do 4000 i=1,nver
            read(1,*) aux,(z(j),j=1,3)
            write(2,*) aux,(z(j),j=1,3)
 4000    continue
      
         call searstr(1,'ELEMENT_GROUPS')
         read(1,*) el_ngr
         nel=0
         do 4100 i=1,el_ngr
            read(1,*) trash, aux, REC
            nel=nel+aux
 4100    continue
 
         write(2,'(A)') '*ELEMENT_GROUPS'
         write(2,'(A)') ' 1'
         write(2,'(A,1x,I10,1x,A7)') ' 1', nel, ' Tetra4'

         call searstr(1,'INCIDENCE')
         read(1,*)
         write(2,'(A)') '*INCIDENCE'
         write(2,'(A)') '<NONE>'
         do 4200 i=1,nel
            read(1,*) (mm(j),j=1,4)
            write(2,*) (mm(j),j=1,4)
 4200    continue
 
         write(2,'(A)') '*END' ! Final

         close(1)
         close(2)         
      endif



      return

 10   write (6,*) "Error al abrir el archivo de preprocesador"
      STOP
 200  write(*,*) "Error al leer los limites de las referencias"
      STOP
      end

