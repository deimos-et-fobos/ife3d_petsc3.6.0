      subroutine  write_interfaz_fs(numitas,inodfs,ilcifs,
     &                 iels,indcs,x1s,x2s,x3s,y1s,y2s,y3s,z1s,z2s,z3s) 

      implicit double precision (a-h,o-z)

      dimension inodfs(*),ilcifs(*),iels(*),indcs(*),x1s(*),x2s(*),
     &          x3s(*),y1s(*),y2s(*),y3s(*),z1s(*),z2s(*),z3s(*) 
      
      character*80 interfaz_fs

      interfaz_fs = 'interface_fs.dat'      

      open(10,file=interfaz_fs,err=666)
      write(10,*) numitas
      write(10,*) (inodfs(i),i=1,numitas)
      write(10,*) (ilcifs(i),i=1,numitas)
      write(10,*) (iels(i),i=1,numitas)
      write(10,*) (indcs(i),i=1,numitas)
      write(10,*) (x1s(i),i=1,numitas)
      write(10,*) (x2s(i),i=1,numitas)
      write(10,*) (x3s(i),i=1,numitas)
      write(10,*) (y1s(i),i=1,numitas)
      write(10,*) (y2s(i),i=1,numitas)
      write(10,*) (y3s(i),i=1,numitas)
      write(10,*) (z1s(i),i=1,numitas)
      write(10,*) (z2s(i),i=1,numitas)
      write(10,*) (z3s(i),i=1,numitas)
      close(10)

      return

 666  write(6,*) 'Error al abrir ', interfaz_fs
      write(6,*) 'en write_interfaz_fs'
      stop

      end
