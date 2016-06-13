      subroutine vwmerrfl(nelf,mmf,zf)
      
      implicit double precision (a-h,o-z)
      
      dimension mmf(4,*),zf(3,*)
      
      open(25,file='errores.vwm')
      write(25,'(a)') '*COORDINATES'
      write(25,*) nelf*4
      do i=1,nelf
         do j=1,4
            write(25,*) (i-1)*4+j,(zf(k,mmf(j,i)),k=1,3)
         enddo
      enddo
      
      write(25,'(A)') '*ELEMENT_GROUPS'
      write(25,'(A)') ' 1'
      write(25,'(A,1x,I9,1x,A6)') ' 1', nelf, 'Tetra4'

      write(25,'(A)') '*INCIDENCES'
      write(25,'(A)') '<NONE>'
      do i=1,nelf
         write(25,*) ((i-1)*4+j,j=1,4)
      enddo
      
      write(25,'(A)') '*END'
      close(25)
      
      open(27,file='gpposerr.cfg')
      write(27,'(a)')  ' 2'        
!      write(27,'(a)')  ' 4'        
!      write(27,'(a)')  ' 1 Jl'
!      write(27,'(a)')  ' 1 Uh' 
!      write(27,'(a)')  ' 1 Etha' 
      write(27,'(a)')  ' 3 Desplazamiento'
      write(27,'(a)')  ' 1 Divergencia' 
      close(27)
      
      return
      end
