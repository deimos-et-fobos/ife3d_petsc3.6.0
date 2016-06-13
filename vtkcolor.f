!******************************************************************
!*
!*    CREACION DE FICHEROS PARA GRAFICACION EN EL SOLIDO
!*
!******************************************************************
      subroutine vtkcolor(color_p,color_s,color_f,nverp,nvers,nverf)

      implicit double precision (a-h,o-z)
      character*80 filename,Header
      integer color_p(*),color_s(*),color_f(*)
  
*     PLACA
      filename = 'Sol/Placa/color.scl'
      write(*,*)'Writing: ',trim(filename)
      open(24,file=filename,form='UNFORMATTED',access='DIRECT',recl=4)
      Header='Color'
      do 10 j=1,20   !Header   
        write(24,rec=j) header(j*4-3 : j*4)
 10   continue
      j=20
      do 11 i=1,nverp
        write(24,rec=j+1) real(color_p(i))
        j=j+1
 11   continue
      do 12 i=1,nverp
        write(24,rec=j+1) real(color_p(i))
        j=j+1
 12   continue
      close(24)

      filename = 'Sol/Placa/color.case'
      open(27,file=filename)
      write(27,'(a)')  'FORMAT'        
      write(27,'(a)')  'type:   ensight'        
      write(27,'(a)')  ''        
      write(27,'(a)')  'GEOMETRY'        
      write(27,'(a)')  'model:     placa.geo'        
      write(27,'(a)')  ''        
      write(27,'(a)')  'VARIABLE'        
      write(27,'(a)')  ''        
      write(27,'(2a)')  'scalar per node:     ',
     &                  'Color    color.scl' 
      write(27,'(a)')  ''        
      write(27,'(a)')  'TIME'        
      write(27,'(a)')  'time set: 1'        
      write(27,'(a)')  'number of steps:  1'        
      write(27,'(a)')  'filename start number:  0'        
      write(27,'(a)')  'filename increment:  1'        
      write(27,'(a)')  'time values:'        
      write(27,'(a)')  '0'        
      close(27)

*     SOLIDO 
      filename = 'Sol/Solido/color.scl'
      write(*,*)'Writing: ',trim(filename)
      open(24,file=filename,form='UNFORMATTED',access='DIRECT',recl=4)
      Header='Color'
      do 20 j=1,20   !Header   
        write(24,rec=j) header(j*4-3 : j*4)
 20   continue
      j=20
      do 21 i=1,nvers
        write(24,rec=j+1) real(color_s(i))
        j=j+1
 21   continue
      close(24)

      filename = 'Sol/Solido/color.case'
      open(27,file=filename)
      write(27,'(a)')  'FORMAT'        
      write(27,'(a)')  'type:   ensight'        
      write(27,'(a)')  ''        
      write(27,'(a)')  'GEOMETRY'        
      write(27,'(a)')  'model:     solido.geo'        
      write(27,'(a)')  ''        
      write(27,'(a)')  'VARIABLE'        
      write(27,'(a)')  ''        
      write(27,'(2a)')  'scalar per node:     ',
     &                  'Color    color.scl' 
      write(27,'(a)')  ''        
      write(27,'(a)')  'TIME'        
      write(27,'(a)')  'time set: 1'        
      write(27,'(a)')  'number of steps:  1'        
      write(27,'(a)')  'filename start number:  0'        
      write(27,'(a)')  'filename increment:  1'        
      write(27,'(a)')  'time values:'        
      write(27,'(a)')  '0'        
      close(27)

*     FLUIDO
      filename = 'Sol/Fluido/color.scl'
      write(*,*)'Writing: ',trim(filename)
      open(24,file=filename,form='UNFORMATTED',access='DIRECT',recl=4)
      Header='Color'
      do 30 j=1,20   !Header   
        write(24,rec=j) header(j*4-3 : j*4)
 30   continue
      j=20
      do 31 i=1,nverf
        write(24,rec=j+1) real(color_f(i))
        j=j+1
 31   continue
      close(24)

      filename = 'Sol/Fluido/color.case'
      open(27,file=filename)
      write(27,'(a)')  'FORMAT'        
      write(27,'(a)')  'type:   ensight'        
      write(27,'(a)')  ''        
      write(27,'(a)')  'GEOMETRY'        
      write(27,'(a)')  'model:     fluido.geo'        
      write(27,'(a)')  ''        
      write(27,'(a)')  'VARIABLE'        
      write(27,'(a)')  ''        
      write(27,'(2a)')  'scalar per node:     ',
     &                  'Color    color.scl' 
      write(27,'(a)')  ''        
      write(27,'(a)')  'TIME'        
      write(27,'(a)')  'time set: 1'        
      write(27,'(a)')  'number of steps:  1'        
      write(27,'(a)')  'filename start number:  0'        
      write(27,'(a)')  'filename increment:  1'        
      write(27,'(a)')  'time values:'        
      write(27,'(a)')  '0'        
      close(27)

      return
      end
