      subroutine  read_interfaz_sp(nvercos,ivercos,ielps,indlp)

      implicit double precision (a-h,o-z)

      dimension ivercos(*),ielps(*),indlp(*)
      
      character*80 interfaz_sp

      interfaz_sp = 'interface_sp.dat' 

      open(10,file=interfaz_sp,err=666)
      read(10,*,err=6666,end=6666) nvercos
      read(10,*,err=6666,end=6666) (ivercos(i),i=1,nvercos)
      read(10,*,err=6666,end=6666) (ielps(i),i=1,nvercos)
      read(10,*,err=6666,end=6666) (indlp(i),i=1,nvercos)
      close(10)

      return

 666  write(6,*) 'Error al abrir ', interfaz_sp
      write(6,*) 'en read_interfaz_sp'
      stop

 6666 write(6,*) 'Error al leer ', interfaz_sp
      write(6,*) 'en read_interfaz_sp'
      stop

      end
