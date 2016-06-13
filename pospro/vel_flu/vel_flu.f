************************************************************************
*     Campo de velocidades del fluido
*     
*     By Richi-07/09/15
************************************************************************
      program vel_flu

      IMPLICIT double precision (A-H,O-Z)
      
      parameter (nodmax=50000000)
      parameter (maxpasos=100)
      parameter (maxnev=100)
      parameter (pi=3.141592654)

      real sol(nodmax),time(2*maxpasos+1),treal(2*maxpasos+1),
     &     frec(maxnev)
      integer lfile,pasos,i,l,ndatos,nev,ev(maxnev)
      CHARACTER*80 filesol,filemov,filegeo,filecase,header,
     &             file_raiz,file_ext
      character(4) StTime,StEv,Str4

*
******      Lectura de los parámetros de movimiento
*
      write(6,'(a)') 'Cantidad de modos: '
      read(5,*) nev
      if(nev.gt.maxnev) goto 611
      write(6,'(a)') 'Modos: '
      read(5,*) (ev(i),i=1,nev)
      write(6,'(a)') 'Autofrecuencias (rad/s): '
      read(5,*) (frec(i),i=1,nev)
      write(6,'(a)') 'Periodo durante la visualizacion (seg): '
      read(5,*) Preal
      write(6,'(a)') 'Numero de periodos a graficar: '
      read(5,*) nper
      write(6,'(a)') 'Cuadros por periodo: '
      read(5,*) pasos
      if(pasos*nper.gt.maxpasos) goto 612
 55   write(6,'(a)') 'Archivo de solución (.scl ó .vec): '
      read(5,*) filesol
      iraiz=index(filesol,'****')
      iscl=index(filesol,'.scl')
      ivec=index(filesol,'.vec')
      if(iraiz.eq.0) goto 613
      if(iscl.eq.0) then
        if(ivec.eq.0) goto 614
      endif
 56   write(6,'(a)') 'Archivo de geometría (.geo): '
      read(5,*) filegeo
      igeo=index(filegeo,'.geo')
      if(igeo.eq.0) goto 615
      
      lfile=len_trim(filesol)
      file_raiz=filesol(1:iraiz-1)
      file_ext=filesol(iraiz+4:lfile)

*
******      Loop en NEV
*
      do 100 j=1,nev

        periodo=1/frec(j)
        do 1 i=1,nper*pasos+1
          time(i)=real(periodo/pasos*(i-1))
          treal(i)=real(Preal/pasos*(i-1))
 1      continue

*
******      Abre el archivo de solución y geometría:
*
        StEv=Str4(ev(j))
        filesol=trim(file_raiz)//StEv//trim(file_ext)
        OPEN(1,FILE=filesol,STATUS='OLD',form='UNFORMATTED',
     &       access='DIRECT',recl=4,ERR=661)
      
*
******      Lee el header y la solución
*
        do 10 l=1,20 !Header
          read(1,rec=l) header(l*4-3:l*4)
 10     continue
        l=1
 11     continue    !Datos
        read(1,rec=l+20,err=61) sol(l)
        l=l+1 
        goto 11
 61     continue
        close(1)
        ndatos=l-1
        if(ndatos.gt.nodmax) goto 616

*
******      Escritura de las soluciones en movimiento
*
        do 20 i=1,nper*pasos+1
          StTime = Str4(i-1)
          filemov='vel_flu'//StEv//'_'//StTime//trim(file_ext)
          ilen=len_trim(filemov)
          write(*,'(a)')'Escribiendo '//trim(filemov)
          OPEN(2,FILE=filemov,STATUS='UNKNOWN',form='UNFORMATTED',
     &         access='DIRECT',recl=4,ERR=662)
          do 21 l=1,20 !Header
            write(2,rec=l) header(l*4-3:l*4)
 21       continue
          l=20      
          do 22 l=1,ndatos      !Datos
            write(2,rec=l+20) real(frec(j)*sol(l)*dcos(frec(j)*
     &                        time(i)*2*pi))
 22       continue 
          close(2)
 20     continue

*
******      Escritura del archivo *.case
*
        filecase = 'vel_flu'//StEv//'.case'
        open(27,file=filecase)
        write(6 ,'(a)')  'Escribiendo '//trim(filecase)
        write(27,'(a)')  'FORMAT'
        write(27,'(a)')  'type:   ensight'
        write(27,'(a)')  ''
        write(27,'(a)')  'GEOMETRY'
        write(27,'(a)')  'model:     '//trim(filegeo)
        write(27,'(a)')  ''
        write(27,'(a)')  'VARIABLE'
        write(27,'(a)')  ''
        if(iscl.ne.0)then
          write(27,'(a)')  'scalar per node:     '//trim(header)//
     &    '    '//trim(file_raiz)//StEv//'_****'//trim(file_ext)
        endif
        if(ivec.ne.0)then
          write(27,'(a)')  'vector per node:     '//'Velocidad'//
     &    '    '//'vel_flu'//StEv//'_****'//trim(file_ext)
        endif
        write(27,'(a)')  ''
        write(27,'(a)')  'TIME'
        write(27,'(a)')  'time set: 1'
        write(27,'(a,1x,i3)')  'number of steps:',nper*pasos+1
        write(27,'(a)')  'filename start number:  0'
        write(27,'(a)')  'filename increment:  1'
        write(27,'(a)')  'time values:'
        do 30 i=1,nper*pasos+1
          write(27,*)  time(i)
 30     continue
        close(27)

 100  continue

      return

 611  write(6,*) 'Error ---> Demasiados modos (max = ',maxnev,')'
      stop
 612  write(6,*) 'Error ---> Demasiados pasos (max = ',maxpasos,')'
      stop
 613  write(6,*) '---> Nombre del archivo de solución '//
     &           'inválido. No contiene "****". Ej: nombre-****.vec'
      goto 55
 614  write(6,*) '---> Extensión del archivo de solución '//
     &           'inválido. La extensión debe ser ".scl" ó ".vec"'
      goto 55
 615  write(6,*) '---> Extensión del archivo de geometría '//
     &           'inválido. La extensión debe ser ".geo"'
      goto 56
 616  write(6,*) 'Error ---> Archivo solución demasiado grande. '//
     &           'ndatos(',ndatos,') > nodmax(',nodmax,')'
      stop
 661  write(6,*) 'Error al abrir el archivo ',trim(filesol)
      STOP    
 662  write(6,*) 'Error al abrir el archivo ',trim(filemov)
      STOP    
      end

      FUNCTION Str4(n)
      CHARACTER*4 Str4, A
      WRITE (A, 1112) n
      Str4 = A
 1112 FORMAT (I4.4)
      RETURN
      END
