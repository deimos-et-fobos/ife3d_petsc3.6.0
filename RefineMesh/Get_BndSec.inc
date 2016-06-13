interface
	subroutine Get_BndSec(BndEdg, NumBndEdg, SecHead, SecTail, NumBndSec)
	   implicit integer (a - z)
	   dimension BndEdg(NumBndEdg, 2)
	   pointer SecHead(:), SecTail(:)
	end subroutine
end interface
