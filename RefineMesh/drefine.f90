subroutine refine(elm$vrt, XVrt, YVrt, hRefElm, Num_hRefElm)
   use LSM
   implicit real(8) (a - h, o - z)
   include 'SpMat.inc' 
   type(LgcSpMat):: elm$vrt
   integer(4) hRefElm(Num_hRefElm),ALLOC_ERR 
   type(LgcSpMat), pointer:: et
   real(8), pointer:: XVrt(:), YVrt(:)
   pointer ia_(:), ja_(:), XVrt_(:), YVrt_(:), iElmSon(:), jElmSon(:)
   allocatable msw(:), jee(:), lp(:)

   NumElm = elm$vrt%NumRows
   NumNod = elm$vrt%NumCols
   et => .Transp. elm$vrt
   allocate(jee(3 * NumElm))
   CALL CONEL3 (elm$vrt%ja,et%ia,et%ja,jee,NumElm)
   call DESTR_spm(et)
   allocate(msw(NumElm), lp(3 * NumElm))
   msw = 0
   do i=1, Num_hRefElm
      msw(hRefElm(i))=1
   end do
   CALL DGMAP34 (XVrt,YVrt,elm$vrt%ia,elm$vrt%ja,jee,lp,msw,NumNod,NumElm,NumNod_)
   NumElm_ = NumElm + count(lp.ne.0)
   allocate(ia_(NumElm_+1), ja_(3*NumElm_), XVrt_(NumNod_), YVrt_(NumNod_))
   ia_(1:NumElm+1) = elm$vrt%ia
   ja_(1:3*NumElm) = elm$vrt%ja
   XVrt_(1:NumNod) = XVrt
   YVrt_(1:NumNod) = YVrt
   deallocate(XVrt, YVrt, STAT = ALLOC_ERR)
   call DESTR_spm(elm$vrt)
   XVrt => XVrt_
   YVrt => YVrt_
   allocate(iElmSon(NumElm + 1), jElmSon(NumElm_))
   is=0
   CALL DPARMAP (XVrt,YVrt,ia_,ja_,NumNod,NumElm,jee,lp,msw,is, iElmSon, jElmSon)
   deallocate(iElmSon, jElmSon)
   call CONSTR_spm(elm$vrt, NumElm_, ia_, ja_); elm$vrt%NumCols = NumNod_
   deallocate(msw, jee, lp)
end subroutine refine

