C
      program rt2dprepro
C
C     Condiciones de contorno
C
C     preprocesador
C
C
C     By Ronia-11/3/97
C
      IMPLICIT double precision (A-H,O-Z)
      integer elmax
      parameter (maxgroups=1000)
      parameter (nodmax=50000000)
      parameter (elmax=50000000)
      
      dimension mm(5,elmax),mmsur(4,elmax),zs(3,nodmax)
      CHARACTER*80 filename,fileout,FILESMESH
      integer el_gr(2,maxgroups),sur_gr(2,maxgroups),el_ngrs,sur_ngrs

      WRITE (6,*) "Archivo malla salome (.mesh): "
      READ  (5,*) FILESMESH
      filename = trim(FILESMESH)
      OPEN  (1,FILE=filename,STATUS='OLD',ERR=10)

      WRITE (6,*) "Archivo de salida: "
      READ  (5,*) fileout
      filename = trim(fileout) // '.vwm'
      OPEN  (2,FILE=filename,STATUS='unknown',ERR=10)
      filename = trim(fileout) // '.sur'
      OPEN  (3,FILE=filename,STATUS='unknown',ERR=10)
            
      call searstr_smesh(1,'Vertices')
      read(1,*) nver
      do 6010 i=1,nver
        read(1,*) zs(1,i),zs(2,i),zs(3,i),aux
 6010 continue

      call searstr_smesh(1,'Triangles')
      read(1,*) nel_sur
      do 6020 i=1,nel_sur
        read(1,*) mmsur(1,i),mmsur(2,i),mmsur(3,i),mmsur(4,i)
 6020 continue

      call searstr_smesh(1,'Tetrahedra')
      read(1,*) nel
      do 6030 i=1,nel
        read(1,*) mm(1,i),mm(2,i),mm(3,i),mm(4,i),mm(5,i)
 6030 continue

      el_ngrs=1
      el_gr(1,1)=1     
      el_gr(2,1)=mm(5,1)     
      sur_ngrs=1
      sur_gr(1,1)=1    
      sur_gr(2,1)=mmsur(4,1)
      do 3000 i=2,maxgroups
        do 3010 j=1,2
          el_gr(j,i)=0     
          sur_gr(j,i)=0     
 3010   continue
 3000 continue

      last=1
      do 3100 i=2,nel
        if(mm(5,i).eq.el_gr(2,last))then
          el_gr(1,last)=el_gr(1,last)+1
        else
          do 3110 j=1,el_ngrs
            if(mm(5,i).eq.el_gr(2,j))then
              last=j
              el_gr(1,last)=el_gr(1,last)+1         
              goto 3166
            endif
 3110     continue
          el_ngrs=el_ngrs+1
          last=el_ngrs
          el_gr(1,last)=1 
          el_gr(2,last)=mm(5,i)
        endif
 3166   continue
 3100 continue

      last=1
      do 3200 i=2,nel_sur
        if(mmsur(4,i).eq.sur_gr(2,last))then
          sur_gr(1,last)=sur_gr(1,last)+1
        else
          do 3210 j=1,sur_ngrs
            if(mmsur(4,i).eq.sur_gr(2,j))then
              last=j
              sur_gr(1,last)=sur_gr(1,last)+1         
              goto 3266
            endif
 3210     continue
          sur_ngrs=sur_ngrs+1
          last=sur_ngrs
          sur_gr(1,last)=1 
          sur_gr(2,last)=mmsur(4,i)
        endif
 3266   continue
 3200 continue

      write(2,'(a)') '*COORDINATES' 
      write(3,'(a)') '*COORDINATES' 
      write(2,*) nver
      write(3,*) nver
      do 4000 i=1,nver
         write(2,*) i,zs(1,i),zs(2,i),zs(3,i)
         write(3,*) i,zs(1,i),zs(2,i),zs(3,i)
 4000 continue

      write(2,*) ' '
      write(2,'(a)') '*ELEMENT_GROUPS'
      write(3,*) ' '
      write(3,'(a)') '*ELEMENT_GROUPS'
      write(2,*) el_ngrs
      do 4010 i=1,el_ngrs
        write(2,*) i,el_gr(1,i),' Tetra4'
 4010 continue
      write(3,*) sur_ngrs
      do 4020 i=1,sur_ngrs
        write(3,*) i,sur_gr(1,i),' Tri3'
 4020 continue
      write(2,*) ' '
      write(2,'(a)') '*INCIDENCE' 
      write(3,*) ' '
      write(3,'(a)') '*INCIDENCE' 
      write(2,'(a)') '<NONE>'
      write(3,'(a)') '<NONE>'
      do 4030 i=1,nel
        write(2,*) mm(1,i),mm(2,i),mm(3,i),mm(4,i)
 4030 continue
      do 4040 i=1,nel_sur
        write(3,*) mmsur(1,i),mmsur(2,i),mmsur(3,i)
 4040 continue

      write(2,'(a)') '*END' 
      write(3,'(a)') '*END' 
      close(2)
      close(3)

      return

 10   write (6,*) "Error al abrir el archivo '", trim(filename),"'"
      STOP
      end
