ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine trasimf(nglf,nelf,nnf,ietf2)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit double precision (a-h,o-z)

      dimension nnf(4,*),ietf2(2,*)
 
!*
!*    Calcula la conectividad transpuesta del fluido como una matriz 2xNGLF
!*
      do 3000 i=1,nglf
        ietf2(1,i)=0
        ietf2(2,i)=0
 3000 continue
 
      do 3010 i=1,nelf
        do 3020 j=1,4
          if ( ietf2(1,nnf(j,i)) .eq. 0 ) then
            ietf2(1,nnf(j,i)) = i
          else
            ietf2(2,nnf(j,i)) = i
          endif
 3020   continue
 3010 continue
 
      return
      end
