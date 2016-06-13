interface
      subroutine Get_BSec$Vrt(BndVrtLnkLst, NumVrt, NumBndVrt, NumBSec, BSecHead, BSecTail, BSec$Vrt)
         use LSM
         implicit integer(4) (a - z)
         include 'SpMat.inc'
         dimension BndVrtLnkLst(NumVrt)
         target BndVrtLnkLst
         pointer next(:)
         dimension BSecHead(NumBSec), BSecTail(NumBSec)
         allocatable aux(:)
         type(LgcSpMat) BSec$Vrt
         pointer ia(:), ja(:)
         logical EndLoop
	end subroutine
end interface
