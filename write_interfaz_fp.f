      subroutine  write_interfaz_fp(numitap,inodfp,ilcifp,
     &                 ielp,indcp,x1p,x2p,x3p,y1p,y2p,y3p,zintp) 

      implicit double precision (a-h,o-z)

      dimension inodfp(*),ilcifp(*),ielp(*),indcp(*),x1p(*),x2p(*),
     &          x3p(*),y1p(*),y2p(*),y3p(*),zintp(*) 
      
      character*80 interfaz_fp

      interfaz_fp = 'interface_fp.dat'      

      open(10,file=interfaz_fp,err=666)
      write(10,*) numitap
      write(10,*) (inodfp(i),i=1,numitap)
      write(10,*) (ilcifp(i),i=1,numitap)
      write(10,*) (ielp(i),i=1,numitap)
      write(10,*) (indcp(i),i=1,numitap)
      write(10,*) (x1p(i),i=1,numitap)
      write(10,*) (x2p(i),i=1,numitap)
      write(10,*) (x3p(i),i=1,numitap)
      write(10,*) (y1p(i),i=1,numitap)
      write(10,*) (y2p(i),i=1,numitap)
      write(10,*) (y3p(i),i=1,numitap)
      write(10,*) (zintp(i),i=1,numitap)
      close(10)

      return

 666  write(6,*) 'Error al abrir ', interfaz_fp
      write(6,*) 'en write_interfaz_fp'
      stop

      end
