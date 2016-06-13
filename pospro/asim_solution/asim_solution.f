C
      program asim_solution

      CHARACTER*80 FileIn, FileOut, WorkDir
      character*4  StNfile, Str4
      character*1  StNfield, Str1
      integer Nfile,Nfield,j,ivec,iscl
      REAL*4 dato

      WorkDir = ''

*
**    PV-#-####.vec & PV-#-####.scl
*
      Nfield = 0
 10   StNfield = Str1(Nfield)
      ivec=0
      iscl=0

      Nfile = 0
 11   StNfile = Str4(Nfile)
      FileIn =trim(WorkDir)//'PV-'//StNfield//'-'//StNfile//'.vec'
      FileOut=trim(WorkDir)//'PV-asim-'//StNfield//'-'//StNfile//'.vec'
      open(1,file=FileIn,access='DIRECT',recl=4,
     &     status='OLD',form='UNFORMATTED',err=1000)
      open(2,file=FileOut,form='UNFORMATTED',access='DIRECT',recl=4)
      write(*,*) 'Reading: ',trim(FileIn),
     &           ' --> Writing: ',trim(FileOut)
      do 60 j=1,20   !Header   
        read(1,rec=j,err=220) dato    
        write(2,rec=j) dato
 60   continue
      j=1         !Datos
 20   read(1,rec=20+j,err=220) dato    
      if(mod(j,3).eq.0)then 
        write(2,rec=20+j) -dato
      else
        write(2,rec=20+j) dato
      endif
      j=j+1
      goto 20
 220  continue
      close(1)
      close(2)
      Nfile=Nfile+1
      goto 11

 1000 if(Nfile.eq.0) ivec=1

      Nfile=0
 12   StNfile = Str4(Nfile)
      FileIn =trim(WorkDir)//'PV-'//StNfield//'-'//StNfile//'.scl'
      FileOut=trim(WorkDir)//'PV-asim-'//StNfield//'-'//StNfile//'.scl'
      open(1,file=FileIn,access='DIRECT',recl=4,
     &     status='OLD',form='UNFORMATTED',err=2000)
      open(2,file=FileOut,form='UNFORMATTED',access='DIRECT',recl=4)
      write(*,*) 'Reading: ',trim(FileIn),
     &           ' --> Writing: ',trim(FileOut)
      do 61 j=1,20   !Header   
        read(1,rec=j,err=230) dato    
        write(2,rec=j) dato
 61   continue
      j=1         !Datos
 62   read(1,rec=20+j,err=230) dato    
      write(2,rec=20+j) -dato
      j=j+1
      goto 62
 230  continue
      close(1)
      close(2)
      Nfile=Nfile+1
      goto 12

 2000 if(Nfile.eq.0) iscl=1

      if(ivec*iscl.eq.1) goto 210
      Nfield=Nfield+1
      goto 10

 210  continue
     
*
**    desplazamiento-####.vec
*
      Nfile = 0
 30   StNfile = Str4(Nfile)
      FileIn =trim(WorkDir)//'desplazamiento-'//StNfile//'.vec'
      FileOut=trim(WorkDir)//'desplazamiento-asim-'//StNfile//'.vec'
      open(1,file=FileIn,access='DIRECT',recl=4,
     &     status='OLD',form='UNFORMATTED',err=300)
      open(2,file=FileOut,form='UNFORMATTED',access='DIRECT',recl=4)
      write(*,*) 'Reading: ',trim(FileIn),
     &           ' --> Writing: ',trim(FileOut)
      do 31 j=1,20   !Header   
        read(1,rec=j,err=320) dato    
        write(2,rec=j) dato
 31   continue
      j=1         !Datos
 32   read(1,rec=20+j,err=320) dato    
      if(mod(j,3).eq.0)then
        write(2,rec=20+j) -dato
      else
        write(2,rec=20+j) dato
      endif
      j=j+1
      goto 32
 320  continue
      close(1)
      close(2)
      Nfile=Nfile+1
      goto 30
 300  continue

*
**    presion-####.scl
*
      Nfile = 0
 130  StNfile = Str4(Nfile)
      FileIn =trim(WorkDir)//'presion-'//StNfile//'.scl'
      FileOut=trim(WorkDir)//'presion-asim-'//StNfile//'.scl'
      open(1,file=FileIn,access='DIRECT',recl=4,
     &     status='OLD',form='UNFORMATTED',err=1300)
      open(2,file=FileOut,form='UNFORMATTED',access='DIRECT',recl=4)
      write(*,*) 'Reading: ',trim(FileIn),
     &           ' --> Writing: ',trim(FileOut)
      do 131 j=1,20   !Header   
        read(1,rec=j,err=1320) dato    
        write(2,rec=j) dato
 131  continue
      j=1         !Datos
 132  read(1,rec=20+j,err=1320) dato    
      write(2,rec=20+j) -dato
      j=j+1
      goto 132
 1320 continue
      close(1)
      close(2)
      Nfile=Nfile+1
      goto 130
 1300 continue

*
**    error-####.scl
*
      Nfile = 0
 330  StNfile = Str4(Nfile)
      FileIn =trim(WorkDir)//'error-'//StNfile//'.scl'
      FileOut=trim(WorkDir)//'error-asim-'//StNfile//'.scl'
      open(1,file=FileIn,access='DIRECT',recl=4,
     &     status='OLD',form='UNFORMATTED',err=3300)
      open(2,file=FileOut,form='UNFORMATTED',access='DIRECT',recl=4)
      write(*,*) 'Reading: ',trim(FileIn),
     &           ' --> Writing: ',trim(FileOut)
      do 331 j=1,20   !Header   
        read(1,rec=j,err=3320) dato    
        write(2,rec=j) dato
 331  continue
      j=1         !Datos
 332  read(1,rec=20+j,err=3320) dato    
      write(2,rec=20+j) -dato
      j=j+1
      goto 332
 3320 continue
      close(1)
      close(2)
      Nfile=Nfile+1
      goto 330
 3300 continue

*
**    pconf-####.scl
*
      Nfile = 0
 430  StNfile = Str4(Nfile)
      FileIn =trim(WorkDir)//'pconf-'//StNfile//'.scl'
      FileOut=trim(WorkDir)//'pconf-asim-'//StNfile//'.scl'
      open(1,file=FileIn,access='DIRECT',recl=4,
     &     status='OLD',form='UNFORMATTED',err=4300)
      open(2,file=FileOut,form='UNFORMATTED',access='DIRECT',recl=4)
      write(*,*) 'Reading: ',trim(FileIn),
     &           ' --> Writing: ',trim(FileOut)
      do 431 j=1,20   !Header   
        read(1,rec=j,err=4320) dato    
        write(2,rec=j) dato
 431  continue
      j=1         !Datos
 432  read(1,rec=20+j,err=4320) dato    
      write(2,rec=20+j) -dato
      j=j+1
      goto 432
 4320 continue
      close(1)
      close(2)
      Nfile=Nfile+1
      goto 430
 4300 continue

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      return
      end

      FUNCTION Str1(n)
      CHARACTER*1 Str1, A
         WRITE (A, 90) n
         Str1 = A
 90      FORMAT (I1.1)
      RETURN
      END

      FUNCTION Str4(n)
      CHARACTER*4 Str4, A
         WRITE (A, 91) n
         Str4 = A
 91      FORMAT (I4.4)
      RETURN
      END





