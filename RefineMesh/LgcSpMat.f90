!------------------------------------------------------------------------
!           CLASE LgcSpMat (Logical Sparse Matrix)
!------------------------------------------------------------------------

module LgcSpMat_Mod

   type LgcRow
      sequence
      integer(4), pointer:: Col(:)
   end type

   type LgcSpMat
      sequence
      integer(4) NumRows
      integer(4) NumCols
      type(LgcRow), pointer:: Row(:)
      integer, pointer::ia(:), ja(:)
   end type

CONTAINS

   subroutine CONSTR(this, NumRows, ia, ja)
      type(LgcSpMat) this
      integer(4), pointer, optional:: ia(:), ja(:)
      this%NumRows = NumRows
      allocate(this%Row(NumRows))
      if (.not.present(ia)) then
         allocate(this%ia(NumRows + 1))
         this%ia = 1
      else
         this%ia => ia
      end if
      if (.not.present(ja)) then
         call Alloc_ja(this)
      else
         this%ja => ja
      end if
      call Point_ja(this)
   end subroutine CONSTR

   subroutine NEW(New_, NumRows, ia, ja) 
      type(LgcSpMat), pointer:: New_
      integer(4), pointer, optional:: ia(:), ja(:)
      allocate(New_)
      if (.not.present(ia)) then
         call CONSTR(New_, NumRows)
      elseif (.not.present(ja)) then
         call CONSTR(New_, NumRows, ia)
      else
         call CONSTR(New_, NumRows, ia, ja)
      end if
   end subroutine NEW

   subroutine DESTR(this)
      type(LgcSpMat) this
      deallocate(this%ia)
      deallocate(this%ja)
      deallocate(this%Row)
   end subroutine DESTR

   subroutine DELETE(this)
      type (LgcSpMat), pointer :: this
      call DESTR(this)
      deallocate(this)
   end subroutine DELETE

   subroutine Alloc_ja(this)
      type(LgcSpMat) this
      allocate(this%ja(this%ia(this%NumRows + 1) - 1))
   end subroutine Alloc_ja

   subroutine Point_ja(this)
      type(LgcSpMat) this
      do i = 1, this%NumRows
         this%Row(i)%Col => this%ja(this%ia(i):this%ia(i + 1) - 1)
      end do
   end subroutine Point_ja

   function RowSize(this, row)
      integer RowSize, row
      type(LgcSpMat) this
      RowSize = this%ia(row + 1) - this%ia(row)
   end function RowSize

   subroutine DimRows(this, RowDim)
      type(LgcSpMat) this
      integer RowDim(this%NumRows)
      this%ia(1) = 1
      do i = 1, this%NumRows
         this%ia(i + 1) = this%ia(i) + RowDim(i)
      end do
      call Alloc_ja(this)
      call Point_ja(this)
   end subroutine DimRows

   subroutine PrintMat(this)
      type(LgcSpMat) this
      intent(in) this
      do i = 1, this%NumRows
         write(6, *) i,':', this%Row(i)%Col
      end do
   end subroutine PrintMat

   subroutine CopyMat(a, b)
      type(LgcSpMat) a, b
      intent(in) b
      intent(out) a
      call CONSTR(a, b%NumRows)
      a%NumCols = b%NumCols
      a%ia = b%ia
      call Alloc_ja(a)
      a%ja = b%ja
      call Point_ja(a)
   end subroutine CopyMat

   function TranspMat(this)
      type(LgcSpMat), pointer:: TranspMat, transp
      type(LgcSpMat), intent(in):: this
      pointer iaux(:)
      call NEW(transp, this%NumCols)
      Transp%NumCols = this%NumRows
      allocate(iaux(transp%NumRows))
      iaux = 0
      do i =1, this%NumRows
         do j = 1, RowSize(this, i)
            jcol = this%Row(i)%Col(j)
            iaux(jcol) = iaux(jcol) + 1
         end do
      end do
      call DimRows(transp, iaux)
      call Alloc_ja(transp)
      call Point_ja(transp)
      iaux = 1
      do i =1, this%NumRows
         do j = 1, RowSize(this, i)
            jcol = this%Row(i)%Col(j)
            transp%Row(jcol)%Col(iaux(jcol)) = i
            iaux(jcol) = iaux(jcol) + 1
         end do
      end do
      deallocate(iaux)
      TranspMat => transp
   end function TranspMat

   function Add (a, b)
      type(LgcSpMat) :: a, b
      type(LgcSpMat), pointer :: Add, c
      integer(4), pointer :: ix(:)  ! Multiple Switch
      intent (in) :: a, b
      integer col
      integer, pointer:: aux(:)
      if ((a%NumRows .ne. b%NumRows) .or. (a%NumCols .ne. b%NumCols)) stop 'Error: Matrices de dimensiones diferentes.'
      numRows = a%NumRows
      numCols = a%NumCols
      call NEW(c, numRows)
      c%NumCols = numCols
      allocate(ix(numCols), aux(numRows))
      ! Averigua la cantidad de no ceros en cada fila, y dimensiona c
      ix = 0
      do n = 1, numRows
         jcount = 0
         do j = 1, RowSize(a, n)
            col = a%Row(n)%Col(j)
            jcount = jcount + 1
            ix(col) = n
         end do
         do j = 1, RowSize(b, n)
            col = b%Row(n)%Col(j)
            if (ix(col) .lt. n) then
               jcount = jcount + 1
            end if
         end do
         aux(n) = jcount
      end do
      call DimRows(c, aux)
      ! Adición
      ix = 0
      do n = 1, numRows
         jp = 0
         do j = 1, RowSize(a, n)
            col = a%Row(n)%Col(j)
            jp = jp + 1
            c%Row(n)%Col(jp) = col
            ix(col) = n
         end do
         do j = 1, RowSize(b, n)
            col = b%Row(n)%Col(j)
            if (ix(col) .lt. n) then
               jp = jp + 1
               c%Row(n)%Col(jp) = col
            end if
         end do
      end do
      Add => c
      deallocate (ix, aux)
   end function Add

END MODULE LgcSpMat_Mod


module LSM
   use LgcSpMat_Mod, &
   l_constr => constr, & 
   l_new => new, &
   l_destr => destr, &
   l_delete => delete, &
   l_alloc_ja => alloc_ja, &
   l_point_ja => point_ja, &
   l_RowSize => RowSize, &
   l_DimRows => DimRows, &
   l_Print => PrintMat, &
   l_Copy => CopyMat, &
   l_Transp => TranspMat, &
   l_Add => Add
end module LSM
