      subroutine  read_interfaz_fs(numitas,inodfs,ilcifs,
     &                 iels,indcs,x1s,x2s,x3s,y1s,y2s,y3s,z1s,z2s,z3s) 

      implicit double precision (a-h,o-z)

      dimension inodfs(*),ilcifs(*),iels(*),indcs(*),x1s(*),x2s(*),
     &          x3s(*),y1s(*),y2s(*),y3s(*),z1s(*),z2s(*),z3s(*) 
      
      character*80 interfaz_fs

      interfaz_fs = 'interface_fs.dat' 

      open(10,file=interfaz_fs,err=666)
      read(10,*,err=6666,end=6666) numitas
      read(10,*,err=6666,end=6666) (inodfs(i),i=1,numitas)
      read(10,*,err=6666,end=6666) (ilcifs(i),i=1,numitas)
      read(10,*,err=6666,end=6666) (iels(i),i=1,numitas)
      read(10,*,err=6666,end=6666) (indcs(i),i=1,numitas)
      read(10,*,err=6666,end=6666) (x1s(i),i=1,numitas)
      read(10,*,err=6666,end=6666) (x2s(i),i=1,numitas)
      read(10,*,err=6666,end=6666) (x3s(i),i=1,numitas)
      read(10,*,err=6666,end=6666) (y1s(i),i=1,numitas)
      read(10,*,err=6666,end=6666) (y2s(i),i=1,numitas)
      read(10,*,err=6666,end=6666) (y3s(i),i=1,numitas)
      read(10,*,err=6666,end=6666) (z1s(i),i=1,numitas)
      read(10,*,err=6666,end=6666) (z2s(i),i=1,numitas)
      read(10,*,err=6666,end=6666) (z3s(i),i=1,numitas)
      close(10)

      return

 666  write(6,*) 'Error al abrir ', interfaz_fs
      write(6,*) 'en read_interfaz_fs'
      stop

 6666 write(6,*) 'Error al leer ', interfaz_fs
      write(6,*) 'en read_interfaz_fs'
      stop

      end
