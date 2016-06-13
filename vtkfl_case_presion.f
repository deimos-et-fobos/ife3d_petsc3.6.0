      subroutine vtkfl_case_presion(nevs)

      implicit double precision (a-h,o-z)

      character*80 WorkDir,filename
      integer nevs
   
      WorkDir = 'Sol/Fluido/'
      filename = trim(WorkDir) // 'fluido_presion.case'
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
     &                  'Presion    presion-****.scl' 
      write(27,'(2a)')  'vector per node:     ',
     &                  'Desplazamiento    desplazamiento-****.vec' 
      write(27,'(a)')  ''        
      write(27,'(a)')  'TIME'        
      write(27,'(a)')  'time set: 1'        
      write(27,'(a,1x,i3)')  'number of steps:',nevs        
      write(27,'(a)')  'filename start number:  0'        
      write(27,'(a)')  'filename increment:  1'        
      write(27,'(a)')  'time values:'        
      do 10 i=0,nevs-1
        write(27,'(1x,i3)')  i        
 10   continue
      close(27)

      return
      end
