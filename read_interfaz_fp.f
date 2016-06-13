      subroutine  read_interfaz_fp(numitap,inodfp,ilcifp,
     &                 ielp,indcp,x1p,x2p,x3p,y1p,y2p,y3p,zintp) 

      implicit double precision (a-h,o-z)

      dimension inodfp(*),ilcifp(*),ielp(*),indcp(*),x1p(*),x2p(*),
     &          x3p(*),y1p(*),y2p(*),y3p(*),zintp(*)
      
      character*80 interfaz_fp

      interfaz_fp = 'interface_fp.dat' 

      open(10,file=interfaz_fp,err=666)
      read(10,*,err=6666,end=6666) numitap
      read(10,*,err=6666,end=6666) (inodfp(i),i=1,numitap)
      read(10,*,err=6666,end=6666) (ilcifp(i),i=1,numitap)
      read(10,*,err=6666,end=6666) (ielp(i),i=1,numitap)
      read(10,*,err=6666,end=6666) (indcp(i),i=1,numitap)
      read(10,*,err=6666,end=6666) (x1p(i),i=1,numitap)
      read(10,*,err=6666,end=6666) (x2p(i),i=1,numitap)
      read(10,*,err=6666,end=6666) (x3p(i),i=1,numitap)
      read(10,*,err=6666,end=6666) (y1p(i),i=1,numitap)
      read(10,*,err=6666,end=6666) (y2p(i),i=1,numitap)
      read(10,*,err=6666,end=6666) (y3p(i),i=1,numitap)
      read(10,*,err=6666,end=6666) (zintp(i),i=1,numitap)
      close(10)

      return

 666  write(6,*) 'Error al abrir ', interfaz_fp
      write(6,*) 'en read_interfaz_fp'
      stop

 6666 write(6,*) 'Error al leer ', interfaz_fp
      write(6,*) 'en read_interfaz_fp'
      stop

      end
