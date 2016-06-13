interface
   subroutine refine(elm$vrt, XVrt, YVrt, hRefElm, Num_hRefElm)
      use LSM
      implicit real(8) (a - h, o - z)
      include 'SpMat.inc' 
      type(LgcSpMat):: elm$vrt
      integer(4) hRefElm(Num_hRefElm) 
      real(8), pointer:: XVrt(:), YVrt(:)
   end subroutine refine
end interface
