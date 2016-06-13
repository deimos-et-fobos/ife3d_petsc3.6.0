      subroutine  write_interfaz_sp(nvercos,ivercos,ielps,indlp)

      implicit double precision (a-h,o-z)

      dimension ivercos(*),ielps(*),indlp(*)
      
      character*80 interfaz_sp

      interfaz_sp = 'interface_sp.dat'      

      open(10,file=interfaz_sp,err=666)
      write(10,*) nvercos
      write(10,*) (ivercos(i),i=1,nvercos)
      write(10,*) (ielps(i),i=1,nvercos)
      write(10,*) (indlp(i),i=1,nvercos)
      close(10)

      return

 666  write(6,*) 'Error al abrir ', interfaz_sp
      write(6,*) 'en write_interfaz_sp'
      stop

      end
