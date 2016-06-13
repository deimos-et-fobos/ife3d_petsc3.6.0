      subroutine elem2ref(nel,elem)

      implicit double precision (a-h,o-z)
      integer elem(*)

      open(24,file='rel3d.cfg')
      write(24,'(a11)')  '*INPUT_MESH'
      write(24,'(a10)')  'fluido.vwm'
      write(24,*)
      write(24,'(a19)')  '*ELEMENTS_TO_REFINE'
      write(24,*) nel
      write(24,*) (elem(i),i=1,nel)
      write(24,*)
      write(24,'(a12)')  '*OUTPUT_MESH'
      write(24,'(a12)')  'fluido_r.vwm'
      write(24,*)
      write(24,'(a4)')  '*END'
      close(27)

      return
      end
