program RefineMesh
   use LSM
   implicit real(8) (a - h, o - z)
   include 'SpMat.inc'
   include 'Get_BndSec.inc'
   include 'Get_BSec$Vrt.inc'
   include 'refine.inc'
   type(LgcSpMat):: elm$vrt, DirSec$Vrt, NeuSec$Vrt
   character(200) WorkDir, HeaderFile, ElemFile, CoordFile, DirEdgFile, NeuEdgFile, RefElmFile
   character(200) ElemFile_n, CoordFile_n, DirEdgFile_n, NeuEdgFile_n, RefElmFile_n, vwmFile_n
   character(200) vwmFile
   real(8), pointer::  XVrt(:), YVrt(:)
   integer, allocatable:: DirEdg(:,:), NeuEdg(:,:), RefElm(:)
   integer, pointer:: DirSecHead(:), DirSecTail(:), NeuSecHead(:), NeuSecTail(:)
   integer, allocatable:: BndVrtLnkLst(:)
   character(2) AdpStp, Str2, zero

   !WorkDir = 'C:\Users\mario\WorkDir\Maestrias\adapt4oct\datos elasticidad 2d\'
   WorkDir = ''
   HeaderFile = trim(WorkDir) // 'cabecera.dat'
   ElemFile = trim(WorkDir) // 'elements3.dat'
   CoordFile = trim(WorkDir) // 'coordinates.dat'
   DirEdgFile = trim(WorkDir) // 'dirichlet.dat'
   NeuEdgFile = trim(WorkDir) // 'neumann.dat'
   RefElmFile = trim(WorkDir) // 'mark_index.dat'
   vwmFile = trim(WorkDir) // 'RefMesh.vwm'

   !Lectura de datos
   call Read_Header(HeaderFile, iAdpStp, NumElm, NumVrt, NumNeuEdg, NumDirEdg, NumRefElm)
   call Read_Elem(ElemFile, NumElm, NumVrt, elm$vrt)
   allocate(XVrt(NumVrt), YVrt(NumVrt))
   call Read_Coord(CoordFile, NumVrt, XVrt, YVrt)
   call SortVrtCCW (elm$vrt, XVrt, YVrt) !Innecesaria cuando se genera la malla con Enredo.
   allocate(DirEdg(NumDirEdg, 2), NeuEdg(NumNeuEdg, 2)) 
   call Read_DirEdg(DirEdgFile, NumDirEdg, DirEdg)
   call Read_NeuEdg(NeuEdgFile, NumNeuEdg, NeuEdg)
   allocate(RefElm(NumRefElm))
   call Read_RefElm(RefElmFile, NumRefElm, RefElm)

   !Redireccionamiento hacia un subdirectorio, para guardar el historial
   WorkDir = 'Historial/'

   !Almacenamiento de los datos originales con subindice 0.
!   if (iAdpStp .eq. 0) then
!      zero = Str2(0)
!      vwmFile_n =  trim(WorkDir) // 'RefMesh_' // zero // '.vwm'
!      ElemFile_n = trim(WorkDir) // 'elements3_' // zero // '.dat'
!      CoordFile_n = trim(WorkDir) // 'coordinates_' // zero // '.dat'
!      DirEdgFile_n = trim(WorkDir) // 'dirichlet_' // zero // '.dat'
!      NeuEdgFile_n = trim(WorkDir) // 'neumann_' // zero // '.dat'
!      RefElmFile_n = trim(WorkDir) // 'mark_index_' // zero // '.dat'
!      call Save_vwmMesh(vwmFile_n, elm$vrt, XVrt, YVrt)
!      call Save_Elem(ElemFile_n, elm$vrt)
!      call Save_Coord(CoordFile_n, XVrt, YVrt, NumVrt)
!      call Get_BndSec(DirEdg, NumDirEdg, DirSecHead, DirSecTail, NumDirSec)
!      call Get_BndSec(NeuEdg, NumNeuEdg, NeuSecHead, NeuSecTail, NumNeuSec)
!      allocate(BndVrtLnkLst(NumVrt))
!      call Get_BndVrtLnkLst(elm$vrt, BndVrtLnkLst)
!      NumBndVrt = count(BndVrtLnkLst .ne. 0)
!      call Get_BSec$Vrt(BndVrtLnkLst, NumVrt, NumBndVrt, NumDirSec, DirSecHead, DirSecTail, DirSec$Vrt)
!      call Get_BSec$Vrt(BndVrtLnkLst, NumVrt, NumBndVrt, 0, NeuSecHead, NeuSecTail, NeuSec$Vrt)
!      deallocate(BndVrtLnkLst)
!      call Save_DirEdg(DirEdgFile_n, DirSec$Vrt)
!      call Save_NeuEdg(NeuEdgFile_n, NeuSec$Vrt)
!      call Save_RefElm(RefElmFile_n, RefElm, NumRefElm)
!   end if

   !Refinamiento de la malla y obtencion de los sectores con C.B. tipo Dir. y Neu.
   call Get_BndSec(DirEdg, NumDirEdg, DirSecHead, DirSecTail, NumDirSec)
   call Get_BndSec(NeuEdg, NumNeuEdg, NeuSecHead, NeuSecTail, NumNeuSec)
   call refine(elm$vrt, XVrt, YVrt, RefElm, NumRefElm)
   NumElm = elm$vrt%NumRows
   NumVrt = elm$vrt%NumCols
   allocate(BndVrtLnkLst(NumVrt))
   call Get_BndVrtLnkLst(elm$vrt, BndVrtLnkLst)
   NumBndVrt = count(BndVrtLnkLst .ne. 0)
   call Get_BSec$Vrt(BndVrtLnkLst, NumVrt, NumBndVrt, NumDirSec, DirSecHead, DirSecTail, DirSec$Vrt)
   call Get_BSec$Vrt(BndVrtLnkLst, NumVrt, NumBndVrt, 0, NeuSecHead, NeuSecTail, NeuSec$Vrt)

   !Almacenamiento de los datos de la malla refinada
   AdpStp =Str2(iAdpStp + 1)
   vwmFile_n =  trim(WorkDir) // 'RefMesh_' // AdpStp // '.vwm'
   ElemFile_n = trim(WorkDir) // 'elements3_' // AdpStp // '.dat'
   CoordFile_n = trim(WorkDir) // 'coordinates_' // AdpStp // '.dat'
   DirEdgFile_n = trim(WorkDir) // 'dirichlet_' // AdpStp // '.dat'
   NeuEdgFile_n = trim(WorkDir) // 'neumann_' // AdpStp // '.dat'
   RefElmFile_n = trim(WorkDir) // 'mark_index_' // AdpStp // '.dat'
   call Save_Elem(ElemFile_n, elm$vrt)
   call Save_Coord(CoordFile_n, XVrt, YVrt, NumVrt)
   call Save_DirEdg(DirEdgFile_n, DirSec$Vrt)
   call Save_NeuEdg(NeuEdgFile_n, NeuSec$Vrt)
   call Save_RefElm(RefElmFile_n, RefElm, NumRefElm)
   call Save_vwmMesh(vwmFile_n, elm$vrt, XVrt, YVrt)

   !Sobreimpresion de los archiivos de entrada para que Octave resuelva el problema de FEM con la malla refinada.
   call Save_Elem(ElemFile, elm$vrt)
   call Save_Coord(CoordFile, XVrt, YVrt, NumVrt)
   call Save_DirEdg(DirEdgFile, DirSec$Vrt)
   call Save_NeuEdg(NeuEdgFile, NeuSec$Vrt)
   call Save_vwmMesh(vwmFile, elm$vrt, XVrt, YVrt)

end program RefineMesh


subroutine Read_Header(HeaderFile, iAdpStp, NumElm, NumVrt, NumDirEdg, NumNeuEdg, NumRefElm)
   implicit real(8) (a - h, o - z)
   character*(*) HeaderFile
   character(200) InpFile
   InpFile = HeaderFile
   open (unit = 1, file = InpFile, status = 'old')
      read(1, *) iAdpStp
      read(1, *) NumElm
      read(1, *) NumVrt
      read(1, *) NumDirEdg
      read(1, *) NumNeuEdg
      read(1, *) NumRefElm
   close(1)
end subroutine Read_Header

subroutine Read_Elem(ElemFile, NumElm, NumVrt, elm$vrt)
   use LSM
   implicit real(8) (a - h, o - z)
   include 'SpMat.inc' 
   type(LgcSpMat):: elm$vrt
   character*(*) ElemFile
   character(200) InpFile
   integer(4), allocatable:: rsz(:)
   InpFile = ElemFile
   call CONSTR_spm(elm$vrt, NumElm); elm$vrt%NumCols = NumVrt
   open (unit = 1, file = InpFile, status = 'old')
      allocate(rsz(NumElm))
      rsz = 3
      call DimRows(elm$vrt, rsz)
      deallocate(rsz)
      do i = 1, NumElm
         read(1, *) elm$vrt%row(i)%col(1:3)
      end do
   close(1)
end  subroutine Read_Elem

subroutine Read_Coord(CoordFile, NumVrt, XVrt, YVrt)
   implicit real(8) (a - h, o - z)
   character*(*) CoordFile
   character(200) InpFile
   real(8) XVrt(NumVrt), YVrt(NumVrt)
   InpFile = CoordFile
   open (unit = 1, file = InpFile, status = 'old')
      do i = 1, NumVrt
         read(1, *) XVrt(i), YVrt(i)
      end do
   close(1)
end subroutine Read_Coord

subroutine Read_DirEdg(DirEdgFile, NumDirEdg, DirEdg)
   character*(*) DirEdgFile
   character(200) InpFile
   integer DirEdg(NumDirEdg, 2)
   InpFile = DirEdgFile
   open (unit = 1, file = InpFile, status = 'old')
      do i = 1, NumDirEdg
         read(1, *) DirEdg(i, 1), DirEdg(i, 2)
      end do
   close(1)
end subroutine Read_DirEdg
      
subroutine Read_NeuEdg(NeuEdgFile, NumNeuEdg, NeuEdg)
   character*(*) NeuEdgFile
   character(200) InpFile
   integer NeuEdg(NumNeuEdg, 2)
   InpFile = NeuEdgFile
   open (unit = 1, file = InpFile, status = 'old')
      do i = 1, NumNeuEdg
         read(1, *) NeuEdg(i, 1), NeuEdg(i, 2)
      end do
   close(1)
end subroutine Read_NeuEdg

subroutine Read_RefElm(RefElmFile, NumRefElm, RefElm)
   character*(*) RefElmFile
   character(200) InpFile
   integer RefElm(NumRefElm)
   InpFile = RefElmFile
   open (unit = 1, file = InpFile, status = 'old')
      do i = 1, NumRefElm
         read(1, *) RefElm(i)
      end do
   close(1)
end subroutine Read_RefElm

subroutine Get_BndSec(BndEdg, NumBndEdg, SecHead, SecTail, NumBndSec)
   implicit integer (a - z)
   dimension BndEdg(NumBndEdg, 2)
   pointer SecHead(:), SecTail(:)
   count = 1
   do i = 1, NumBndEdg - 1
      if (BndEdg(i, 2) .ne. BndEdg(i + 1, 1)) then
          count = count + 1
      end if
   end do
   NumBndSec = count
   allocate(SecHead(NumBndSec), SecTail(NumBndSec))
   pnt = 1
   if (NumBndEdg .eq. 0) then
      SecHead(1) = 0
   else   
      SecHead(1) = BndEdg(1, 1)
   endif
   do i = 1, NumBndEdg - 1
      if (BndEdg(i, 2) .ne. BndEdg(i + 1, 1)) then
          SecTail(pnt) = BndEdg(i, 2)
          pnt = pnt + 1
          SecHead(pnt) = BndEdg(i + 1, 1)
      end if
   end do
   if (NumBndEdg .eq. 0) then
      SecTail(pnt) = 0
   else   
      SecTail(pnt) = BndEdg(NumBndEdg, 2)      
   endif
end subroutine

subroutine Get_BndVrtLnkLst(elm$vrt, BndVrtLnkLst)
   use LSM
   implicit integer(4) (a - z)
   include 'SpMat.inc' 
   type(LgcSpMat):: elm$vrt
   integer(4) BndVrtLnkLst(elm$vrt%NumCols)
   allocatable:: ib(:)

   NumElm = elm$vrt%NumRows
   NumVrt = elm$vrt%NumCols
   allocate(ib(NumVrt))
   CALL BORDEG (NumVrt, NumElm, elm$vrt%ia, elm$vrt%ja, BndVrtLnkLst, ib)
   deallocate(ib)
end subroutine Get_BndVrtLnkLst

subroutine Get_BSec$Vrt(BndVrtLnkLst, NumVrt, NumBndVrt, NumBSec, BSecHead, BSecTail, BSec$Vrt)
   !Genera una matriz booleana de (NumBSec x NumVrt) con la informacion de los 
   !vertices de cada seccion de frontera.
   !
   !                  /
   !                 | 1 si el vertice j pertenece a la seccion i.
   !BSec$Vrt(i,j)  = < 
   !                 | 0 en otro caso.
   !                  \
   !
   ! Nota 1: BSec$Vrt%Row(i)%Col es la lista de los vertices de la seccion i, recorridos en sentido antihorario
   !         si la frontera es la exterior, y horario para las interiores.
   ! Nota 2: BSec$Vrt%ja es la lista completa de vertices de todas las secciones de frontera.
   ! Nota 3: BSec$Vrt%ja no necesariamente esta ordenado en sentido horario o antihorario, ya que se respeta
   !         el orden en que estan dadas las secciones
   ! Nota 4: Esta subrutina se programa para generar NonSec$Vrt, usada para identificar los tubos a traves
   !         de vertices pertenecientes a ellos (datos de entrada)
   ! Nota 5: SI (HEAD.EQ.TAIL) PARA ALGUNA SECCION, ESTA DEBE SER CERRADA Y SE AGREGAN TODOS LOS VÉRTICES DE LA MISMA.

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

   allocate (aux(NumBndVrt + 1)) !NumBndVrt es el número total de vértices de frontera (exterior + interiores)
                                 !Como cada frontera es cerrada el numero total de lados de la frontera es NumBndVrt.
                                 !COMO AHORA ALMACENAMOS DOS VECES HEAD CUANDO HEAD=TAIL, DEBEMOS RESERVAR UN LUGAR MAS DE MEMORIA
   allocate(ia(NumBSec + 1))
   next => BndVrtLnkLst
   pnt = 1
   do n=1, NumBSec
      head = BSecHead(n)
      tail = BSecTail(n)
      ia(n) = pnt
      !Agregamos head a la lista fuera del loop.
      aux(pnt) =  head
      curr = next(head) 
      pnt = pnt + 1
      EndLoop = .false.  !SIEMPRE DEBE HABER AL MENOS DOS VERTICES EN EL SECTOR DE FRONTERA!!
      do while (.not. EndLoop)
         aux(pnt) =  curr
         pnt = pnt + 1
         if (curr .eq. tail) then 
            EndLoop = .true. 
         else
            curr = next(curr) 
         end if
      end do
   end do
   ia(NumBSec+1) = pnt
   allocate(ja(pnt - 1))
   ja = aux(1:pnt - 1)
   call CONSTR_spm(BSec$Vrt, NumBSec, ia, ja); BSec$Vrt%NumCols = NumVrt
   deallocate(aux)
   
end subroutine Get_BSec$Vrt
      
subroutine SortVrtCCW (elm$vrt, XVrt, YVrt)
   use LSM
   implicit real(8) (a - h, o - z)
   include 'SpMat.inc'
   type(LgcSpMat) elm$vrt
   dimension XVrt(elm$vrt%NumCols), YVrt(elm$vrt%NumCols)
   integer vrt1, vrt2, vrt3
   dimension ed1(2), ed2(2)
   NumElm = elm$vrt%NumRows
   NumVrt = elm$vrt%NumCols
   do i = 1, NumElm
      vrt1 = elm$vrt%row(i)%col(1)
      vrt2 = elm$vrt%row(i)%col(2)
      vrt3 = elm$vrt%row(i)%col(3)
      ed1(1) = XVrt(vrt2) - XVrt(vrt1); ed1(2) = YVrt(vrt2) - YVrt(vrt1)
      ed2(1) = XVrt(vrt3) - XVrt(vrt1); ed2(2) = YVrt(vrt3) - YVrt(vrt1)
      if (determ(ed1, ed2) .lt. 0) then
         elm$vrt%row(i)%col(2) = vrt3
         elm$vrt%row(i)%col(3) = vrt2
      end if 
   end do
end subroutine SortVrtCCW 

real(8) function Determ(v1, v2)
   implicit real(8) (a - h, o - z)
   dimension v1(2), v2(2)
   Determ = v1(1) * v2(2) - v1(2) * v2(1)
end function Determ

subroutine Save_EnrMesh(MeshFile, elm$vrt, XVrt, YVrt)
   use LSM
   implicit real(8) (a - h, o - z)
   include 'SpMat.inc' 
   type(LgcSpMat):: elm$vrt
   integer OutUnit
   parameter (OutUnit = 100)
   character(200) MeshFile
   dimension XVrt(elm$vrt%NumCols), YVrt(elm$vrt%NumCols)
   NumElm = elm$vrt%NumRows
   NumVrt = elm$vrt%NumCols
   open(unit=OutUnit, file=MeshFile)
      nnz = elm$vrt%ia(NumElm + 1) - 1
      write(OutUnit, fmt='(3I10)') NumVrt, NumElm, nnz
      write (OutUnit, fmt='(10I10)') (elm$vrt%ia(i), i=1, NumElm + 1)
      write (OutUnit, fmt='(10I10)') (elm$vrt%ja(i), i=1, nnz)
      write (OutUnit, *) XVrt
      write (OutUnit, *) YVrt
      !call flush(OutUnit)
   close(OutUnit)
end  subroutine Save_EnrMesh

subroutine Save_vwmMesh(vwmFile, elm$vrt, XVrt, YVrt)
   use LSM
   implicit real(8) (a-h, o-z)
   include 'SpMat.inc' 
   type(LgcSpMat) elm$vrt
   integer OutUnit
   parameter (OutUnit = 100)
   character(200) vwmFile
   dimension XVrt(elm$vrt%NumCols), YVrt(elm$vrt%NumCols)

   NumElm = elm$vrt%NumRows
   NumVrt = elm$vrt%NumCols
   OPEN (UNIT=1,FILE=vwmFile,STATUS='UNKNOWN')
   write(1,fmt='(A10)') '*DIMENSION'
   write(1,*) 2
   write(1,*) ''
   write(1,fmt='(A12)') '*COORDINATES'
   write(1,*) NumVrt
   do i=1, NumVrt
      write(1,*) i, XVrt(i), YVrt(i)
   end do
   write(1,*) ''
   write(1,*) ''
   write(1,fmt='(A15)') '*ELEMENT_GROUPS'
   write(1,*) 1
   write(1,*) 1, NumElm, ' Tri3'
   write(1,*) ''
   write(1,fmt='(A10)') '*INCIDENCE'
   write(1,fmt='(A6)') '<NONE>'
   do i=1, NumElm
      write(1,*) (elm$vrt%row(i)%col(j),j=1, 3)
   end do
   write(1,*) ''
   write(1,*) ''
   write(1,fmt='(A4)') '*END'        
   close(1)
end subroutine Save_vwmMesh

! Función para convertir en string de 2 dígitos con "leading zeros" un número entero < 100 

FUNCTION Str2(n)
CHARACTER * 2 Str2, A
   WRITE (A, 10) n
   Str2 = A
10 FORMAT (I2.2)
RETURN
END

subroutine Save_Elem(ElemFile_n, elm$vrt)
   use LSM
   character(200) ElemFile_n
   type(LgcSpMat) elm$vrt
   NumElm = elm$vrt%NumRows
   open (unit = 1, file = ElemFile_n)
      do n = 1, NumElm
         write(1, *) elm$vrt%row(n)%col
      end do
   close(1)
end subroutine Save_Elem

subroutine Save_Coord(CoordFile_n, XVrt, YVrt, NumVrt)
   implicit real(8) (a - h, o - z)
   character(200) CoordFile_n
   dimension XVrt(NumVrt), YVrt(NumVrt)
   open (unit = 1, file = CoordFile_n)
   do i = 1, NumVrt
      write(1, *) XVrt(i), YVrt(i)
   end do
   close(1)
end subroutine Save_Coord

subroutine Save_DirEdg(DirEdgFile_n, DirSec$Vrt)
   use LSM
   include 'SpMat.inc'   
   character(200)DirEdgFile_n
   type(LgcSpMat) DirSec$Vrt
   NumSec = DirSec$Vrt%NumRows
   open (unit = 1, file = DirEdgFile_n)
      do n = 1, NumSec
         do i = 1, RowSize(DirSec$Vrt, n) - 1
            write(1, *) DirSec$Vrt%row(n)%col(i), DirSec$Vrt%row(n)%col(i + 1)
         end do
      end do
   close(1)
end subroutine Save_DirEdg

subroutine Save_NeuEdg(NeuEdgFile_n, NeuSec$Vrt)
   use LSM
   include 'SpMat.inc'   
   character(200)NeuEdgFile_n
   type(LgcSpMat) NeuSec$Vrt
   NumSec = NeuSec$Vrt%NumRows
   open (unit = 1, file = NeuEdgFile_n)
      do n = 1, NumSec
         do i = 1, RowSize(NeuSec$Vrt, n) - 1
            write(1, *) NeuSec$Vrt%row(n)%col(i), NeuSec$Vrt%row(n)%col(i + 1)
         end do
      end do
   close(1)
end subroutine Save_NeuEdg

subroutine Save_RefElm(RefElmFile_n, RefElm, NumRefElm)
   character(200) RefElmFile_n
   integer RefElm(NumRefElm)
   open (unit = 1, file = RefElmFile_n)
      do i = 1, NumRefElm
         write(1, *) RefElm(i)
      end do
   close(1)
end subroutine Save_RefElm
