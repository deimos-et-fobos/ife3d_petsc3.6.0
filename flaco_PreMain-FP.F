!-----------------------------------------------------------------------
!     PROGRAMA DE PROPOSITO GENERAL PARA RESOLUCION DE SISTEMAS
!     Reducibles por Elementos
!     GENERICO
!-----------------------------------------------------------------------
!
!     Programa para particionado de problema
!
      Program PreMainFP
!
      IMPLICIT REAL*8 (A-H, O-Z)
!
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscis.h"
#include "petsc/finclude/petscis.h90"
#include "petsc/finclude/petscmat.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscvec.h90"
!
!
!     subroutine BuildlIEJEpre allocates memory for some arrays
!
      interface BuildlIEJEpre
      subroutine BuildlIEJEpre(gIE, gJE, NodT, NelT, lIE, lJE, NelL,
     &     iRank, nProcs, iFirstNode, locNodes)
      Integer gIE(*), gJE(*), iFirstNode(*), locNodes(*)
      Integer NodT, NelT, NelL, iRank, nProcs
      Integer, dimension(:), pointer :: lIE, lJE
      end subroutine
      end interface
!
!     subroutine BuildlIEJEdef allocates memory for some arrays
!
      interface BuildlIEJEdef
      subroutine BuildlIEJEdef(gIE, gJE, NodT, NelT,
     &     lIE, lJE, NelL, giEls, nGhosts, nlNodes,
     &     iRank, nProcs, isPerm, isPart, iFirstNode, locNodes,
     &     NodOUT, NodesOUT)
      Integer gIE(*), gJE(*), iFirstNode(*), locNodes(*), NodesOUT(*)
      Integer NodT, NelT, NelL, nGhosts, iRank, nProcs, NodOUT
      Integer, dimension(:), pointer :: lIE, lJE, giEls, nlNodes
      IS isPerm, isPart
      end subroutine
      end interface
!
!     Common variables
!
      Parameter (Lch=16)
      Character Str*120
      Logical NonLinearProblem
      double precision noMPI_Wtime
!
!     Vectors O(1)
!
      Allocatable rNormG(:)     ! valor tipico p/c/DoF (iDoFT)
      Allocatable TolG(:)       ! toler. p/convergencia en c/DoF (iDoFT)
      Character(Lch) DofNamesG
      Allocatable DofNamesG  (:) ! Nombres de los grados de libertad (iDofT)
      Allocatable iDofNamesG(:) ! Idem (en enteros)
      Allocatable iXupdateG(:)  ! Indicador p/actualizar coordenadas (nDim)
      Allocatable NodesOut(:)   ! Para sacar salida en unos POCOS nodos (NodOUT)
!
      Logical Sym, Iterative
      Allocatable Sym(:)        ! Symmetric linear system ??? (NSubSteps)
      Allocatable Iterative(:)  ! Solve iteratively ??? (NSubSteps)
      Allocatable MaxIter(:)    ! Max. NL? iterations ??? (NSubSteps)
      Allocatable iSolverType(:) ! Solver type ??? (NSubSteps)
      Allocatable SRP(:)        ! Sub-Relaxation parameter ??? (NSubSteps)
      Allocatable ITMAXSOL(:)   ! Max. iterations linear solver??? (NSubSteps)
      Allocatable EPS(:)        !  ??? (NSubSteps)
      Allocatable Krylov(:)     !  ??? (NSubSteps)
      Allocatable LFIL(:)       !  ??? (NSubSteps)
      Allocatable TolPre(:)     !  ??? (NSubSteps)
      Allocatable rNorm(:,:)    !  ??? (iDofT, NSubSteps)
      Allocatable Tol(:,:)      !  ??? (iDofT, NSubSteps)
      Allocatable CommonPar(:,:) ! (MaxElemLib*MaxLCommonPar, NSubSteps)
      Allocatable iElementLib(:,:) ! (5*MaxElemLib, NSubSteps)
      Allocatable iDofNames (:,:) ! (iDofT*Lch, NSubSteps))
      Allocatable iXupdate (:,:) !  (Ndim, NSubSteps))
      Allocatable MC (:)        ! ? (MaxCoup)
      Allocatable iCoupling (:,:,:) ! ? (MaxLRows,MaxLRows,MaxCoup)
!
!     Vectors O(Nel | Nod)
!
      Allocatable IE(:)      ! Conectividad de la malla (puntero, NelT+1)
      Allocatable JE(:)      ! Conectividad de la malla (nodos de c/elemento)
      Allocatable NelTG(:)   ! Nro de elementos en el grupo (NGROUPS)
      Allocatable lElType(:) ! local element type
      Allocatable lElMat(:)  ! local element material
      Allocatable Mater(:)   ! Global material element (in master, NelT)
      Character*30 NelName
      Allocatable NelName(:) ! Nombre de los elementos del grupo (NGROUPS)
      Allocatable NodElG(:)  ! Nro de nodos en c/elemento para c/grupo (NGROUPS)
!
!     This arrays are allocated in a subroutine
!     lIE:     ! Parte local de IE (puntero, NelL+1)
!     lJE:     ! Parte local de JE (nodos de c/elemento)
!     giEls:   ! Indices globales de los elementos locales
!     nlNodes: ! Lista de nodos no-locales a un proceso
!
      Integer, dimension(:), pointer :: lIE, lJE, giEls, nlNodes
!
      Allocatable lIET(:)    ! Parte local de la conectividad de la malla
!                              (puntero, Nodloc+1)
      Allocatable lJET(:)    ! Parte local de la conectividad de la malla
!                              (elementos de c/nodo)
      Allocatable lIA(:)     ! Matriz de adyacencia (punteros)
      Allocatable lJA(:)     ! Matriz de adjacencia (columnas)
      Allocatable msw(:)     ! Multiple switch (auxiliar)
      Allocatable iaux(:)    ! Auxiliar (building index sets)
      Allocatable jaux(:)    ! Auxiliar (building index sets)
      Allocatable locNodes(:)   ! Cantidad de nodos locales
      Allocatable iFirstNode(:) ! Primer nodo global en este proceso
!
!     Vectors for parameters (can be O(Nel))
!
      Allocatable iPartPar(:)   ! Partitioning of GoPs
      Allocatable IE_Par(:)     ! Pointer to real parameters
      Allocatable IE_JPar(:)    ! Pointer to integer parameters
      Allocatable lIE_Par(:)    ! Pointer to real parameters (local)
      Allocatable lIE_JPar(:)   ! Pointer to integer parameters (local)
      Allocatable Param(:)      ! Real parameters
      Allocatable JParam(:)     ! Integer parameters
      Integer giGoP, giPar, giJPar
      Allocatable giGoP(:)      ! Global index of GoP
      Allocatable giPar(:)      ! Pointer to global Param. list (real)
      Allocatable giJPar(:)     ! Pointer to global Param. list (int)
!
      integer status(MPI_STATUS_SIZE)
!
!     Adjacency matrix, partitioning
!
      Mat AdjMatrix
      MatPartitioning ParMatrix
!
!     Index sets for partitioning, permutation, permutation of elements
!
      IS isPart, isPerm, isPels
!
!     Coordinates:
!     - sequential vector for IO
!     - distributed vector with ghost nodes
!
      Vec SCoords, DCoords
!
!     Dirichlet conditions:
!     - sequential vector for IO
!     - distributed vectors with ghost nodes (one for each substep) (indices)
!     - distributed vectors with ghost nodes ( "   "   "      "   ) (values)
!
      Vec vSDir
      Vec, Allocatable :: viDir(:)
      Vec, Allocatable :: vcDir(:)
!
!     Solutions:
!     - sequential vector for IO
!     - distributed vectors with ghost nodes (one for each timestep stored)
      Vec SSol
      Vec, Allocatable :: DSols(:)
      Vec SOUT
!
!     Auxiliar vector (used for VecGhostGetLocalPart)
!
      Vec vvPTSC, vv2PTSC
!
      Vec vElems, vlElems
!
      Vec vSParam, vSJParam
      Vec, Allocatable :: vParam(:)
      Vec, Allocatable :: vJParam(:)
!
      Allocatable auxParam(:)
      Allocatable auxJParam(:)
!
!     Auxiliar pointers for accessing local parts of petsc vectors
!     [Vec | IS] GetArrayF90 ...
!
      PetscScalar, pointer :: ivPTSC(:)
      PetscInt, pointer :: iPTSC(:)
!      PetscInt, pointer :: iPart(:)
!
!     Auxiliar index sets for creating scatter-gather contexts
!
      IS isSeq, isPar
!
!     Mappings:
!     - From sequential to distributed coordinates
!     - From sequential to distributed elements
!     - From sequential to distributed unknowns
!         (also used for Dirichlet conditions)
!
      VecScatter sgcCoords, sgcElems, sgcVUnk
      VecScatter sgcParam, sgcJParam
      VecScatter sgcParam0, sgcJParam0
      VecScatter sgcNOUT
!
      Call PetscInitialize (PETSC_NULL_CHARACTER, iError)
      Call PetscMemorySetGetMaximumUsage(iError)
!
      time00 = noMPI_Wtime ()
!
!     Ask total number of proccesses
      Call MPI_Comm_size (PETSC_COMM_WORLD, nProcs, iError)
      Allocate (locNodes(nProcs))
      Allocate (iFirstNode(nProcs))
!     ID of this proccess
      Call MPI_Comm_rank (PETSC_COMM_WORLD, iRank, iError)
!
      Call PetscPrintf (PETSC_COMM_WORLD,
     &     '----------------'//char(10)//
     &     '   Solver GP:'//char(10)//
     &     '----------------'//char(10), iError)
      Str = 'PROGRAMA DE PROPOSITO GENERAL'//char(10)//
     &     'PARA RESOLUCION DE SISTEMAS'//char(10)//
     &     'Reducibles por Elementos'//char(10)//char(10)
      Call PetscPrintf (PETSC_COMM_WORLD, Str, iError)
!
!
!     Block 0:
!     Read all data from Basparam.txt
!     (master process reads and broadcast data)
!
!
!
!     Open Basparam and reads general data
!
      t0lecBP = noMPI_Wtime ()
      If (iRank .eq. 0) Then    ! Only main processor
         IO_BPar = 11
         iniError = 1
         OPEN (IO_BPar, FILE='Basparam.txt', STATUS='OLD', ERR=10)
         iniError = 0
 10      Continue
!
!     Read switch for reading from sequential files
!     or parallel (Basparam always sequential)
!
!     Defaults:
!
         iRPI = 0               ! Read sequential input file(s)
         iWPI = 0               ! Don't write input for parallel program
         iCom = 1               ! Compute, not just pre/post-process
         iRPO = 0               ! Don't read previous parallel output
         iWPO = 0               ! Write sequential output file(s)
!
         If (iniError .eq. 0) Then
            Str='*Read parallel input'
            iError = iFindStringInFile (Str, IO_BPar)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               iRPI = 0
            Else
               Read (IO_BPar, *) iRPI
            End If
            If (iRPI .eq. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Reading sequential files.'//char(10), iError)
            Else If (iRPI .lt. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Not Reading input files.'//char(10), iError)
            Else
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Reading parallel files.'//char(10), iError)
            End If
         End If
!
!     Read switch for writing parallel *input* files
!     (only makes sense when reading sequential files)
!     Mesh.txt -> Mesh_xxxx.txt
!     Inifile000.txt -> Inifile_xxxx.txt
!     Params000.txt -> Params_xxxx.txt
!
         If (iniError .eq. 0 .and. iRPI .eq. 0) Then
            Str='*Write parallel input'
            iError = iFindStringInFile (Str, IO_BPar)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               iWPI = 0
            Else
               Read (IO_BPar, *) iWPI
            End If
            If (iWPI .eq. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Will not write parallel input files.'//
     &              char(10), iError)
            Else
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Will write parallel input files.'//
     &              char(10), iError)
            End If
         End If
!
!     Read switch for actually running the solver
!     (instead of only pre/post-process input files)
!
         If (iniError .eq. 0) Then
            Str='*Compute'
            iError = iFindStringInFile (Str, IO_BPar)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               iCom = 1
            Else
               Read (IO_BPar, *) iCom
            End If
            If (iCom .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Will run the solver'//char(10), iError)
            Else
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Will NOT run the solver'//char(10), iError)
            End If
         End If
!
!     Read switch for post processing already existing parallel results files
!     (only makes sense if not running the solver)
!
         If (iniError .eq. 0 .and. iCom .eq. 0) Then
            Str='*Read parallel output'
            iError = iFindStringInFile (Str, IO_BPar)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               iRPO = 0
            Else
               Read (IO_BPar, *) iRPO
            End If
            If (iRPO .eq. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Will not post process parallel output'//
     &              char(10), iError)
            Else
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Will post-process parallel output'//
     &              char(10), iError)
            End If
         End If
!
!     Read switch for writing parallel results files
!     (only makes sense if running solver and not post-processing)
!
         If (iniError .eq. 0 .and. iCom .ne. 0 .and. iRPO .eq. 0) Then
            Str='*Write parallel output'
            iError = iFindStringInFile (Str, IO_BPar)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               iWPO = 0
            Else
               Read (IO_BPar, *) iWPO
            End If
            If (iWPO .eq. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Output files will be sequential'//
     &              char(10), iError)
            Else
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Output files will be parallel'//
     &              char(10), iError)
            End If
         End If
!
!     Read iDoFT, if not found, search in Mesh.txt
!
         If (iniError .eq. 0) Then
            Str='*NODAL DOFs'
            iError = iFindStringInFile (Str, IO_BPar)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Looking in Mesh file'//
     &              char(10), iError)
               IO_Mesh = 13
               iniError = 1
               If (iRPI .eq. 0) Then
                  Str = 'Mesh.txt'
               Else
                  Str = 'Mesh_0000.txt'
               End If
               OPEN (IO_Mesh, FILE=Str, STATUS='OLD', ERR=20)
               iniError = 0
 20            Continue
               If (iniError .ne. 0) Then
                  Call PetscPrintf (PETSC_COMM_SELF,
     &                 'Can not open Mesh file.'//char(10), iError)
               Else
                  iError = iFindStringInFile (Str, IO_Mesh)
                  If (iError .ne. 0) Then
                     Call PetscPrintf (PETSC_COMM_SELF,
     &                    Str//' not found.'//char(10), iError)
                     iniError = 1
                  Else
                     Read (IO_Mesh, *) iDoFT
                  End If
                  Close (IO_Mesh)
               End If
!
            Else
               Read (IO_BPar, *) iDoFT
            End If
         End If
!
!     Read nDim, if not found, search in Mesh.txt
!
         If (iniError .eq. 0) Then
            Str='*DIMEN'
            iError = iFindStringInFile (Str, IO_BPar)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Looking in Mesh.txt'//
     &              char(10), iError)
               IO_Mesh = 13
               iniError = 1
               If (iRPI .eq. 0) Then
                  Str = 'Mesh.txt'
               Else
                  Str = 'Mesh_0000.txt'
               End If
               OPEN (IO_Mesh, FILE=Str, STATUS='OLD', ERR=30)
               iniError = 0
 30            Continue
               If (iniError .ne. 0) Then
                  Call PetscPrintf (PETSC_COMM_SELF,
     &                 'Can not open Mesh.txt file.'//char(10), iError)
               Else
                  iError = iFindStringInFile (Str, IO_Mesh)
                  If (iError .ne. 0) Then
                     Call PetscPrintf (PETSC_COMM_SELF,
     &                    Str//' not found.'//char(10), iError)
                     iniError = 1
                  Else
                     Read (IO_Mesh, *) nDim
                  End If
                  Close (IO_Mesh)
               End If
!
            Else
               Read (IO_BPar, *) nDim
            End If
         End If
!
!     *Param W
!     Switch for writing Param.txt with solution in each step (default: no)
!
         If (iniError .eq. 0) Then
            Str='*Param W'
            iError = iFindStringInFile (Str, IO_BPar)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Will not write Param.txt in each step'//
     &              char(10), iError)
               iWDataSet = 0
            Else
               Read (IO_BPar, *) iWDataSet
            End If
         End If
!
!     *Initial Time
!     0: Tini as in Basparam(default) ; 1: as in Inifile
!
         If (iniError .eq. 0) Then
            Str='*Initial Time'
            iError = iFindStringInFile (Str, IO_BPar)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Will use Initial Time from Basparam.txt'//
     &              char(10), iError)
               initialTime = 0
            Else
               Read (IO_BPar, *) initialTime
            End If
         End If
!
!     *TimeStep (mandatory, read DelT, Tini, Tmax)
!
         If (iniError .eq. 0) Then
            Str='*TimeStep'
            iError = iFindStringInFile (Str, IO_BPar)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               iniError = 1
            Else
               Read (IO_BPar, *) DelT, Tini, Tmax
            End If
         End If
!
!     *OutputControl (Optional, read nkt[1], nStepOut[1])
!
         If (iniError .eq. 0) Then
            Str='*OutputControl'
            iError = iFindStringInFile (Str, IO_BPar)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Will output all steps'//
     &              char(10), iError)
               nkt = 1
               nStepOut = 1
            Else
               Read (IO_BPar, *) nkt, nStepOut
            End If
         End If
!
!     *NodeOutputControl (Optional, read NodOUT[0])
!
         If (iniError .eq. 0) Then
            Str='*NodeOutputControl'
            iError = iFindStringInFile (Str, IO_BPar)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Will output solution in all nodes'//
     &              char(10), iError)
               NodOUT = 0
            Else
               Read (IO_BPar, *) NodOUT
               If (NodOUT .ne. 0) Then
                  Allocate(NodesOut(NodOUT))
                  Read (IO_BPar, *) (NodesOut(i), i = 1, NodOUT)
               End If
            End If
         End If
!
!     *ElementLibraryControl (mandatory, read MaxElemLib,MaxLCommonPar
!     .......      and MaxNodEl !!!!!!!!!!!!!!!!!!!!)
!
         If (iniError .eq. 0) Then
            Str='*ElementLibraryControl'
            iError = iFindStringInFile (Str, IO_BPar)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               iniError = 1
            Else
               Read (IO_BPar, *, IOSTAT=iError)
     &              MaxElemLib, MaxLCommonPar, MaxNodEl
               If (iError .ne. 0) Then
                  Call PetscPrintf (PETSC_COMM_SELF, 'Error reading'//
     &                 Str//', possibly missing MaxNodEl !!!'//
     &                 char(10), iError)
                  iniError = 1
               End If
            End If
         End If
!
!     *Renumbering?
!
         NStepsRenum = 0
!
!     Sub-step processing: count Number
!
         If (iniError .eq. 0) Then
            Str='*SubStep'
            Rewind(IO_BPar)
            NSubSteps = iCountStringInFile (Str, IO_BPar)
            If (NSubSteps .eq. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               iniError = 1
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' At least ONE SubStep required'//
     &              char(10), iError)
            End If
         End If
!
      End If                    ! Only master process
!
!     Broadcast error indicator, stop if non-zero
!
      Call MPI_Bcast(iniError, 1, MPI_INTEGER, 0, PETSC_COMM_WORLD, I)
      If (iniError .ne. 0) Then
         Call PetscPrintf(PETSC_COMM_WORLD,
     &        ' Error Reading First Stage'//char(10), iError)
         Call PETSCFINALIZE(iError)
         Stop
      End If
!
!     Broadcast switches, and some scalars
!
      Call MPI_Bcast(iRPI, 1, MPI_INTEGER, 0, PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(iWPI, 1, MPI_INTEGER, 0, PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(iCom, 1, MPI_INTEGER, 0, PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(iRPO, 1, MPI_INTEGER, 0, PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(iWPO, 1, MPI_INTEGER, 0, PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(iDoFT,1, MPI_INTEGER, 0, PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(nDim, 1, MPI_INTEGER, 0, PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(iWDataSet,1,MPI_INTEGER,0,PETSC_COMM_WORLD,iError)
      Call MPI_Bcast(initialTime, 1,MPI_INTEGER, 0,PETSC_COMM_WORLD, I)
      Call MPI_Bcast(Delt, 1, MPI_DOUBLE_PRECISION, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(Tini, 1, MPI_DOUBLE_PRECISION, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(Tmax, 1, MPI_DOUBLE_PRECISION, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(nkt, 1, MPI_INTEGER, 0, PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(nStepOut,1,MPI_INTEGER, 0,PETSC_COMM_WORLD,iError)
      Call MPI_Bcast(NodOUT, 1, MPI_INTEGER, 0,PETSC_COMM_WORLD,iError)
      Call MPI_Bcast(MaxElemLib, 1, MPI_INTEGER, 0,PETSC_COMM_WORLD,I)
      Call MPI_Bcast(MaxLCommonPar,1,MPI_INTEGER,0,PETSC_COMM_WORLD,I)
      Call MPI_Bcast(MaxNodEl, 1, MPI_INTEGER, 0, PETSC_COMM_WORLD, I)
      Call MPI_Bcast(NStepsRenum,1,MPI_INTEGER,0,PETSC_COMM_WORLD,I)
      Call MPI_Bcast(NSubSteps,1,MPI_INTEGER,0,PETSC_COMM_WORLD,iError)
      MaxLRows = MaxNodEl * iDofT
!
      Write(Str, "('DoF per node: ',I0,', Spatial dimensions: ',I0,"//
     &     "', SubSteps: ',I0)") iDofT, nDim, NSubSteps
      Len=Len_Trim(Str)
      Call PetscPrintf(PETSC_COMM_WORLD, Str(1:Len)//char(10), iError)
!
!     Alloc memory
!
      Allocate(rNormG (iDofT))
      Allocate(TolG   (iDofT))
      Allocate( DofNamesG (iDofT))   ! only master ?
      Allocate(iXupdateG  (nDim ))
      Allocate(iDofNamesG (iDofT*Lch))
!
!     Continue reading
!
      If (iRank .eq. 0) Then
!
!     *StepContinuationControl (mandatory, read )
!
         Str='*StepContinuationControl'
         iError = iFindStringInFile (Str, IO_BPar)
         If (iError .ne. 0) Then
            Call PetscPrintf (PETSC_COMM_SELF,
     &           Str//' not found.'//char(10), iError)
            iniError = 1
         Else
            Read(IO_BPar, *) NonLinearProblem, MaxIterG,
     &           SRPG, nOldTimeSteps
            Read(IO_BPar, *) (rNormG(N)   , N = 1, iDofT)
            Read(IO_BPar, *) (TolG(N)     , N = 1, iDofT)
            Read(IO_BPar, *) (DOfNamesG(N), N = 1, iDofT)
            Read(IO_BPar, *) (iXupdateG(N), N = 1, nDim)
!
!     Convert Variable names to integer
!
            Do i = 1, iDofT
               Do j = 1, Lch
                  iDofNamesG(j+Lch*(i-1)) = IACHAR(DofNamesG(i)(j:j))
               End Do
            End Do
         End If
      End If
!
!     Collective error test
!
      Call MPI_Bcast(iniError, 1, MPI_INTEGER, 0, PETSC_COMM_WORLD, I)
      If (iniError .ne. 0) Then
         Call PetscPrintf(PETSC_COMM_WORLD,
     &        ' Error Reading StepContinuationControl'//char(10),iError)
         Call PETSCFINALIZE(iError)
         Stop
      End If
!
!     Broadcast general data (not particular to any substep)
!
      Call MPI_Bcast (NonLinearProblem, 1, MPI_LOGICAL, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast (MaxIterG,1,MPI_INTEGER, 0,PETSC_COMM_WORLD,iError)
      Call MPI_Bcast (SRPG, 1, MPI_DOUBLE_PRECISION, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast (nOldTimeSteps, 1, MPI_INTEGER, 0,
     &     PETSC_COMM_WORLD,iError)
      Call MPI_Bcast (rNormG, iDofT, MPI_DOUBLE_PRECISION, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast (TolG, iDofT, MPI_DOUBLE_PRECISION, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast (iDofNamesG, iDofT*Lch, MPI_INTEGER, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast (iXupdateG, nDim, MPI_INTEGER, 0,
     &     PETSC_COMM_WORLD,iError)
!
!     Alloc memory for variables involving sub-steps
!     
      Allocate(Sym(NSubSteps))
      Allocate(Iterative(NSubSteps))
      Allocate(MaxIter(NSubSteps))
      Allocate(iSolverType(NSubSteps))
      Allocate(SRP(NSubSteps))
      Allocate(ITMAXSOL(NSubSteps))
      Allocate(EPS(NSubSteps))
      Allocate(Krylov(NSubSteps))
      Allocate(LFIL(NSubSteps))
      Allocate(TolPre(NSubSteps))
      Allocate(rNorm (iDofT, NSubSteps))
      Allocate(Tol   (iDofT, NSubSteps))
!
      LCommonPar = MaxElemLib*MaxLCommonPar
      Allocate(CommonPar  (LCommonPar  , NSubSteps))
      Allocate(iElementLib(5*MaxElemLib, NSubSteps))
      Allocate(iDofNames  (iDofT*Lch, NSubSteps))
      Allocate(iXupdate   (Ndim     , NSubSteps))
!
!%%%%%%%%%%%% Lectura de biblioteca de elementos en Basparam %%%%%%%%%%%
!     Read Library for each sub-step
!
      if (iRank .eq. 0) Then
         Rewind(IO_BPar)
         MaxCoup = 0
         iNCoup = 0
         Do NSbp = 1, NSubSteps
            Str = '*SubStep'
            iError = iFindStringInFile (Str, IO_BPar)
!
            Call ReadLibrary (iElementLib(1,NSbp), CommonPar(1,NSbp),
     &           iDofNames(1,NSbp), rNorm(1,NSbp), Tol(1,NSbp),
     &           iXupdate(1,NSbp),
     &           Sym(NSbp), Iterative(NSbp), MaxIter(NSbp),
     &           iSolverType(NSbp), SRP(NSbp),
     &           ITMAXSOL(NSbp), EPS(NSbp), Krylov(NSbp), LFIL(NSbp),
     &           TolPre(NSbp),
     &           iDofT, MaxElemLib, MaxLCommonPar, Ndim, Lch,
     &           IO_BPar)
!     Calculo la ultima matriz de acoplamiento para Dimensionar y
!     Cuantas hay que leer
!            write (*,*) ielementlib ; write (*,*) commonpar
!            write (*,*) idofnames; write (*,*) rnorm ; write (*,*) tol
            Do L = 1, MaxElemLib
               ipELib = (4-1) * MaxElemLib + L
               MC1 = iElementLib(ipELib, NSbp)
               iCoup = abs(MC1)
               MaxCoup = max(iCoup, MaxCoup)
!     if (   MC >    0   )iNCoup  = iNCoup + 1
            End Do
         End Do
!
         Allocate (MC(MaxCoup))
         MC = 0
         Do NSbp = 1, NSubSteps
            Do L = 1, MaxElemLib
               ipELib = (4-1) * MaxElemLib + L
               MC1 = iElementLib(ipELib, NSbp)
!     iCoup=abs( MC1 )
!     if (iCoup > MaxCoup)MaxCoup = iCoup
               If (MC1 > 0) MC(MC1) = 1
            End Do
         End Do
         iNCoup = Sum(MC)
!
!%%%%%%%%%% Lectura de la matriz de acoplamiento en BasicParam %%%%%%%%%
!
         Allocate (iCoupling(MaxLRows,MaxLRows,MaxCoup))
         iCoupling = 0
         If (iNCoup > 0) Then
            Str='*Coupling Matrices'
            iError = iFindStringInFile(Str, IO_BPar)
            if (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               iniError = 1
            Else
               Do iNCou = 1, iNCoup
                  Read (IO_BPar, *) L
                  Read (IO_BPar, *) ((iCoupling (iDofR, iDofC, L),
     &                 iDofC = 1, MaxLRows), iDofR = 1, MaxLRows)
               End Do
            End If
         End If
!
!     Close Basparam.txt
!
         Close(IO_BPar)
      End If
!
!     Collective error test
!
      Call MPI_Bcast(iniError, 1, MPI_INTEGER, 0, PETSC_COMM_WORLD, I)
      If (iniError .ne. 0) Then
         Call PetscPrintf(PETSC_COMM_WORLD,
     &        ' Error Reading coupling matrix'//char(10),iError)
         Call PETSCFINALIZE(iError)
         Stop
      End If
!
!     Broadcast the rest
!
      Call MPI_Bcast(MaxCoup, 1, MPI_INTEGER, 0,PETSC_COMM_WORLD,iError)
      If (iRank .ne. 0) Then
         Allocate (iCoupling(MaxLRows, MaxLRows, MaxCoup))
      End If
!      Allocate (NodesOut(NodOUT))
!
!     Vectors regarding sub-steps
!
      Call MPI_Bcast(Sym, NSubSteps, MPI_LOGICAL, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(Iterative, NSubSteps, MPI_LOGICAL, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(MaxIter, NSubSteps, MPI_INTEGER, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(iSolverType, NSubSteps, MPI_INTEGER, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(SRP, NSubSteps, MPI_DOUBLE_PRECISION, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(ITMAXSOL, NSubSteps, MPI_INTEGER, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(EPS, NSubSteps, MPI_DOUBLE_PRECISION, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(Krylov, NSubSteps, MPI_INTEGER, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(LFIL, NSubSteps, MPI_INTEGER, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(TolPre, NSubSteps, MPI_DOUBLE_PRECISION, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(rNorm, iDoFT*NSubSteps, MPI_DOUBLE_PRECISION, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(Tol, iDoFT*NSubSteps, MPI_DOUBLE_PRECISION, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(CommonPar, LCommonPar*NSubSteps,
     &     MPI_DOUBLE_PRECISION, 0, PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(iElementLib,5*MaxElemLib*NSubSteps,MPI_INTEGER, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(iDofNames, iDoFT*Lch*NSubSteps, MPI_INTEGER, 0,
     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast(iXupdate, Ndim*NSubSteps, MPI_INTEGER, 0,
     &     PETSC_COMM_WORLD, iError)
!      call MPI_Bcast(NodesOut, NodOUT, MPI_INTEGER, 0,
!     &     PETSC_COMM_WORLD, iError)
      Call MPI_Bcast (iCoupling, MaxLRows*MaxLRows*MaxCoup,
     &     MPI_INTEGER, 0, PETSC_COMM_WORLD, iError)
!
!     Basparam.txt processing complete (End Block 0)
!
!
      tlecBP  = noMPI_Wtime () - t0lecBP
      Write(Str,"('Time reading general data (Basparam.txt): ',"//
     &     " F12.3, ' Secs')") tlecBP
      Len = Len_Trim(Str)
      Call PetscPrintf(PETSC_COMM_WORLD, Str(1:Len)//char(10), iError)
!
!      Str = 'After reading general data'
!      Call MemReport(iRank, nProcs, Str)
!
!
!     Block I:
!     Reading data from sequential file(s)
!
!
      If (iRPI .eq. 0) Then     ! Read Sequential input data
         t0lecS = noMPI_Wtime ()
         If (iRank .eq. 0) Then ! Only main processor
            IO_Mesh = 13
            iniError = 1
            Open (IO_Mesh, FILE='Mesh.txt', STATUS='OLD', ERR=40)
            iniError = 0
 40         Continue
            If (iniError .eq. 0) Then
               Str='*COORDINATES'
               iError = iFindStringInFile(Str, IO_Mesh)
               If (iError .ne. 0) Then
                  Call PetscPrintf(PETSC_COMM_SELF,
     &                 Str//' not found in Mesh.txt'//char(10), iError)
                  iniError = 1
                  Close (IO_Mesh)
               End If
               READ (IO_Mesh, *) nCoords
            End If
!
            If (iniError .eq. 0) Then
               Str='*ELEMENT GROUPS'
               iError = iFindStringInFile (Str, IO_Mesh)
               If (iError .ne. 0) Then
                  Call PetscPrintf(PETSC_COMM_SELF,
     &                 Str//' not found in Mesh.txt'//char(10), iError)
                  iniError = 1
                  Close (IO_Mesh)
               End If
            End If
         End If
!
         Call MPI_Bcast(iniError,1,MPI_INTEGER, 0, PETSC_COMM_WORLD, I)
         If (iniError .ne. 0) Then
            Call PETSCPRINTF(PETSC_COMM_WORLD,
     &           'Error reading Mesh.txt'//char(10), iError)
            Call PETSCFINALIZE(iError)
            Stop
         End If
!
!     Master continues reading file
!
         If (iRank .eq. 0) Then
!
!     Number of groups of elements
!
            READ (IO_Mesh, *) NGROUPS
!
            Allocate (NelTG(NGROUPS))
            Allocate (NelName(NGROUPS))
            Allocate (NodElG(NGROUPS))
!
            Do ng = 1, NGROUPS
!
!     For each group read index, number of elements, name of element
!
               READ (IO_Mesh, *) IDGroup, NelTG(ng), NelName(ng)
               if (IDGroup .ne. ng) then
                  write (6,*) 'IDGroup != ng!', IDGroup, ng
                  Stop
               end if
            End Do
!
!     Total elements
!
            NelT = Sum(NelTG)
!
!     Traditional sparse-matrix conectivity pointer:
!
            Allocate (IE(NelT+1))
!
            ipNel = 0
            Do ng = 1, NGROUPS
!
!     Determine nodes per element according to name of element:
!
               Len = Len_Trim(NelName(ng))
               NodElG(ng) = 0
               If ( NelName(ng)(1:Len) == 'Point')  NodElG(ng)=1
               If ( NelName(ng)(1:Len) == 'Seg2')   NodElG(ng)=2
               If ( NelName(ng)(1:Len) == 'Seg3')   NodElG(ng)=3
               If ( NelName(ng)(1:Len) == 'Tri3')   NodElG(ng)=3
               If ( NelName(ng)(1:Len) == 'Tri6')   NodElG(ng)=6
               If ( NelName(ng)(1:Len) == 'Tetra4') NodElG(ng)=4
               If ( NelName(ng)(1:Len) == 'Quad4')  NodElG(ng)=4
               If ( NelName(ng)(1:Len) == 'Quad8')  NodElG(ng)=8
               If ( NelName(ng)(1:Len) == 'Cub8')   NodElG(ng)=8
               If ( NelName(ng)(1:Len) == 'Cub20')  NodElG(ng)=20
               If ( NelName(ng)(1:Len) == 'Tetra10')NodElG(ng)=10
!
!     If "generic" read # of nodes of each element from file:
!
               If (NelName(ng)(1:7)  == 'Generic' .or.
     &              NelName(ng)(1:7) == 'generic' .or.
     &              NelName(ng)(1:7) == 'GENERIC') Then
                  READ (IO_Mesh, *) (IE(ipNel+ Nel), Nel = 1, NelTG(ng))
               Else
                  if (NodElG(ng) .eq. 0) then
                     Write(6,*) "ELEMENT TYPE NOT IMPLEMENTED"
                     Stop
                  end if
!
                  Do Nel = 1, NelTG(ng)
                     IE(ipNel+Nel) = NodElG(ng)
                  End Do
               End If
               ipNel = ipNel + NelTG(ng)
!
            End Do
            DeAllocate (NodElG)
            DeAllocate (NelName)
            DeAllocate (NelTG)
!
!     IE has now the # of node of each element, convert to pointer
!
            Length_JE = IEarrange(IE,NelT)
!
!     Allocate conectivity
!
            Allocate ( JE(Length_JE) )
!
!     ... and read from file
!
            Str='*INCIDENCE'
            iError = iFindStringInFile(Str,IO_Mesh)
            if (iError .ne. 0) then
               Write(6,*) Str, " NOT FOUND"
               Stop
            end if
            READ (IO_Mesh, *) JE
!
!     Count nodes:
!
            NodT = 0
            Do i = 1, Length_JE
               If (JE(i) .gt. NodT) NodT = JE(i)
            End Do
!
            If (nCoords .ne. NodT) then
               Write(Str,"('Number of coordinates .ne. nodes: ',"//
     &              "I0,' != ',I0)") nCoords, NodT
               Len = Len_Trim(Str)
               Call PetscPrintf(PETSC_COMM_SELF, char(10)//Str(1:Len)//
     &              char(10)//char(10), iError)
               NodT = nCoords
            end if
         End If                 ! If (Master process)
!
!     Some information
!
         Write(Str,*) 'Conectivity reading OK,'//
     &        ' proceeding with partition, mesh size: '
         Len = Len_Trim(Str)
         Call PetscPrintf(PETSC_COMM_WORLD, Str(1:Len)//char(10),iError)
         Write(Str,*) 'Total nodes & elements:', NodT, NelT
         Len = Len_Trim(Str)
         Call PetscPrintf(PETSC_COMM_WORLD, Str(1:Len)//char(10),iError)
!
!     Inform all processes the total number of nodes and elements
!
         Call MPI_Bcast (NodT, 1, MPI_INTEGER, 0,
     &        PETSC_COMM_WORLD, iError)
         Call MPI_Bcast (NelT, 1, MPI_INTEGER, 0,
     &        PETSC_COMM_WORLD, iError)
!
         If (nProcs .gt. NodT) Then
            Call PetscPrintf(PETSC_COMM_WORLD,
     &           'Error: more processes than nodes!', iError)
            Call PetscFinalize(iError)
            Stop
         End If
!
!     Distribute conectivity to other processes
!
!     Size of partitions
!
         NodL = NodT / nProcs
         If (mod(NodT, nProcs) .gt. iRank) Then
            NodL = NodL + 1
         End If
!
         Call MPI_Allgather(NodL, 1, MPI_INTEGER,
     &        locNodes, 1, MPI_INTEGER, PETSC_COMM_WORLD, iError)
!
!     All processes know the ranges assigned to each one
!
         iFirstNode(1) = 1
         Do i = 1, nProcs - 1
            iFirstNode(i+1) = iFirstNode(i) + locNodes(i)
         End Do
!
!     Build "local" conectivity for each process
!
         Call BuildlIEJEpre (IE, JE, NodT, NelT, lIE, lJE, NelL,
     &        iRank, nProcs, iFirstNode, locNodes)
!
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        'Preliminary partition:'//char(10), iError)
         Write(Str,"('Process: ',I5,', Local Nodes & Elements:',I8,I8)")
     &        iRank, NodL, NelL
         Len = Len_Trim(Str)
         Call PetscSynchronizedPrintf (PETSC_COMM_WORLD,
     &        Str(1:Len)//char(10),iError)
         Call PetscSynchronizedFlush (PETSC_COMM_WORLD, iError)
!
!     Now, build transpose in parallel (restricted to local columns)
!
         Allocate(lIET(NodL+1))
         lenlJE = lIE(NelL+1) - 1
!     The size of lJET is over estimated (no mucho)
         Allocate(lJET(lenlJE))
!
         nFirst = iFirstNode(iRank+1)
         nLast = nFirst + locNodes(iRank+1) - 1
!
         Call RestTranspose (lIE, lJE, lIET, lJET, NelL, nFirst, nLast)
!
!     Build adjacency matrix (local part):
!
         Allocate (lIA(NodL + 1))
!
!     El siguiente vector puede dar problemas de memoria,
!     es de tamaño número de nodos totales y se almacena en todos
!     los procesos, se usa en el ensamblaje simbólico, para no repetir
!     un elemento en la matriz. Una alternativa es repetirlos y
!     luego eliminarlos, posiblemente mediante un ordenamiento...
!
         Allocate (msw(NodT))
!
!     First call, only count storage space for lJA
!
         Call RestBuildAdj (lIE, lJE, lIET, lJET, nFirst, nLast,
     &        lIA, lJA, 0, msw, NodT)
         Allocate (lJA(lIA(NodL+1) - 1))
!
!     Second call: build adjacency matrix in lIA, lJA
!
         Call RestBuildAdj (lIE, lJE, lIET, lJET, nFirst, nLast,
     &        lIA, lJA, 1, msw, NodT)
!
!     First step local conectivity not longer needed
!
!         Str='Setting up partitioning'
!         Call MemReport(iRank, nProcs, Str)
         DeAllocate (lIE)       ! Allocated inside "BuildlIEJEpre"
         DeAllocate (lJE)       ! Allocated inside "BuildlIEJEpre"
         DeAllocate (lIET)
         DeAllocate (lJET)
         DeAllocate (msw)
!
!     Transform indices to "C" Style (for PETSC)
!
         Do i = 1, NodL+1
            lIA(i) = lIA(i) - 1
         End Do
         Do i = 1, lIA(NodL+1)
            lJA(i) = lJA(i) - 1
         End Do
!
!     Adjacency matrix built, call PETSc for partition (parmetis??)
!
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        char(10)//'Computing new partition...'//char(10), iError)
!
         Call MatCreateMPIAdj (PETSC_COMM_WORLD, NodL, NodT,
     &        lIA, lJA, PETSC_NULL_INTEGER, AdjMatrix, iError)
         Call MatPartitioningCreate(PETSC_COMM_WORLD, ParMatrix, iError)
         Call MatPartitioningSetAdjacency(ParMatrix, AdjMatrix, iError)
         Call MatPartitioningSetFromOptions(ParMatrix, iError)
         Call MatPartitioningApply(ParMatrix, isPart, iError)
!         Str='After partitioning apply'
!         Call MemReport(iRank, nProcs, Str)
         Call MatPartitioningDestroy(ParMatrix, iError)
         Call MatDestroy(AdjMatrix, iError)
         Call ISPartitioningToNumbering(isPart, isPerm, iError)
!
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        '...ready. Now splitting mesh.'//char(10), iError)
!
!     Call ISInvertPermutation(isPerm, NodL, isPermI, iError)
         DeAllocate(lIA)
         DeAllocate(lJA)
!
!     iPart has the relocation of each node, each proccess holds
!     the "local" data, according to the first trivial partition
!     iPTSC has the renumeration, isPerm(old#) is the new#
!     and isPermI(newnode) -> old numeration
!
!
!     Re-compute local connectivities according to new node-partition
!     This time the elements are assigned to a unique process
!     (the one that owns its *first node*)
!     Compute also which non-local nodes are needed in each process
!
         Call BuildlIEJEdef(IE, JE, NodT, NelT,
     &        lIE, lJE, NelL, giEls, NnlNodes, nlNodes,
     &        iRank, nProcs, isPerm, isPart, iFirstNode, locNodes,
     &        NodOUT, NodesOut)
!
!     Number of nodes in each partition *can* change !!
!     Inside "BuildlIEJEdef" locNodes() and iFirstNode() are re-built
!
         NodL = locNodes(iRank + 1)
         nFirst = iFirstNode(iRank+1)
         Call ISDestroy(isPart, iError)
!
         If (iRank .eq. 0) Then
            DeAllocate(IE)
            DeAllocate(JE)
         End If
!
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        char(10)//'Final partition:'//char(10), iError)
         Write(Str,"('Process: ',I5,', Local Nodes:',I8,"//
     &        "'  Ghost nodes:',I7,',   Elements:',I8)")
     &        iRank, NodL, NnlNodes, NelL
         Len = Len_Trim(Str)
         Call PetscSynchronizedPrintf (PETSC_COMM_WORLD,
     &        Str(1:Len)//char(10), iError)
         Call PetscSynchronizedFlush (PETSC_COMM_WORLD, iError)
!
!     Create and distribute local part of coordinates
!
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        char(10)//'Reading coordinates...'//char(10), iError)
         If (iRank .eq. 0) Then
!
!     Read coordinates and distribute local parts
!
            Str='*NODAL DOFs'
            iError = iFindStringInFile(Str, IO_Mesh)
            if (iError .ne. 0) then
               Write(6,*) Str, "NOT FOUND"
               Stop
            End if
            READ (IO_Mesh, *) iDoFTmesh
!
            Str='*DIMEN'
            iError = iFindStringInFile(Str, IO_Mesh)
            if (iError .ne. 0) then
               Write(6,*) Str, "NOT FOUND"
               Stop
            End if
            READ (IO_Mesh, *) nDimmesh
!
            If (iDoFTmesh .ne. iDoFT .or.
     &           nDimmesh .ne. nDim) Then
               Write(6,*) 'iDoFT and/or nDim differ in'//
     &              'Basparam.txt and Mesh.txt files:'
               Write(6,*) 'iDoFT:', iDoFT, iDoFTmesh
               Write(6,*) 'nDim:', nDim, nDimmesh
               Stop
            End If
!
            Str='*COORDINATES'
            iError = iFindStringInFile(Str, IO_Mesh)
            if (iError .ne. 0) then
               Write(6,*) Str, "NOT FOUND"
               Stop
            End if
            READ (IO_Mesh, *) nCoords
            if (nCoords .ne. NodT) then
               write(6,*) "Number of coordinates .ne. nodes",
     &              nCoords, NodT
               Stop
            end if
            lenSCoo = nDim * NodT
         Else
            lenSCoo = 0
         End If
!
!     Broadcast general data: Degrees of Freedom per node, spatial dimensions
!
!         Call MPI_Bcast (iDoFT, 1, MPI_INTEGER, 0,
!     &        PETSC_COMM_WORLD, iError)
!         Call MPI_Bcast (nDim, 1, MPI_INTEGER, 0,
!     &        PETSC_COMM_WORLD, iError)
!
!     Create PETSC Vec for sequential coordinates for reading:
!     (parallel vector, but all components in root process
!
         Call VecCreateMPI (PETSC_COMM_WORLD, lenSCoo, nDim*NodT,
     &        SCoords, iError)
!
         If (iRank .eq. 0) Then
            Call VecGetArrayF90(SCoords, ivPTSC, iError)
            Read(IO_Mesh,*) (ivPTSC(i), i = 1, Nodt*nDim)
            Call VecRestoreArrayF90(SCoords, ivPTSC, iError)
         end if
!
!     Index set for sequential coordinates
!     Local part of the index sets should be equal when
!     creating a scatter-gather context
!
         Call ISCreateStride(PETSC_COMM_WORLD,
     &        NodL*nDim, (nFirst-1)*nDim, 1, isSeq, iError)
!
!     Distributed coordinates vector (with ghost values!)
!
         Allocate (iaux(NnlNodes*nDim))
         Do i = 1, NnlNodes
            Do j = 1, nDim
               iaux((i-1)*nDim+j) = (nlNodes(i)-1)*nDim + j - 1
            End Do
         End Do
         Call VecCreateGhost (PETSC_COMM_WORLD,
     &        nDim*NodL, nDim*NodT, nDim*NnlNodes, iaux,
     &        DCoords, iError)
         DeAllocate(iaux)
!
!     Build index set for loading distributed coordinates
!     from sequential vector
!
         Allocate (iaux(NodL*nDim))
         Call ISGetIndicesF90 (isPerm, iPTSC, iError)
         Do i = 1, NodL
            Do j = 1, nDim
               iaux((i-1)*nDim+j) = iPTSC(i)*nDim + j - 1
            End Do
         End Do
         Call ISRestoreIndicesF90(isPerm, iPTSC, iError)
         Call ISCreateGeneral(PETSC_COMM_WORLD, NodL*nDim, iaux,
     &        PETSC_USE_POINTER, isPar, iError)
!
!     Create scatter-gather context for distributing coordinates
!
         Call VecScatterCreate (SCoords, isSeq, DCoords, isPar,
     &        sgcCoords, iError)
         Call ISDestroy(isPar, iError)
         DeAllocate(iaux)
         Call ISDestroy(isSeq, iError)
!
!     Distribute coordinates (from original vector read from file
!     to reordered and partitioned vector)
!
         Call VecScatterBegin(sgcCoords, SCoords, DCoords,
     &        INSERT_VALUES, SCATTER_FORWARD, iError)
         Call VecScatterEnd(sgcCoords, SCoords, DCoords,
     &        INSERT_VALUES, SCATTER_FORWARD, iError)
!         Str='After distributing coordinates'
!         Call MemReport(iRank, nProcs, Str)
!
!     Sequential coordinates vector no longer needed
!     Same for scatter-gather context to distribute them
!     Unless we need to write sequentially the coordinates
!     (not used currently)
!
!         If (iWPO .eq. 1) Then
            Call VecDestroy (SCoords, iError)
            Call VecScatterDestroy (sgcCoords, iError)
!         End If
!
!     Now use the VecGhost and its facilities to load the ghost values
!
         Call VecGhostUpdateBegin(DCoords,
     &        INSERT_VALUES, SCATTER_FORWARD, iError)
         Call VecGhostUpdateEnd(DCoords,
     &        INSERT_VALUES, SCATTER_FORWARD, iError)
!
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        '...ready.'//char(10), iError)
!
!     Continue reading Mesh.txt, now ELEMENT TYPE and ELEMENT MAT
!
         Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &        'Reading element type and material...'//char(10), iError)
         If (iRank .eq. 0) Then
            nn = NelT
         Else
            nn = 0
         End If
!
         Call VecCreateMPI (PETSC_COMM_WORLD, nn, NelT, vElems, iError)
         If (iRank .eq. 0) Then
            Str='*ELEMENT TYPE'
            iError = iFindStringInFile(Str, IO_Mesh)
            if (iError .ne. 0) then
               Write(6,*) Str, "NOT FOUND"
               Stop
            end if
            Call VecGetArrayF90(vElems, ivPTSC, iError)
            Read (IO_Mesh, *) (ivPTSC(i), i = 1, NelT)
            Call VecRestoreArrayF90(vElems, ivPTSC, iError)
         End If
!
!     Distribute element types (from original vector read from file
!     to reordered and partitioned vector)
!     Convert to "C" style indexing
!
         Do i = 1, NelL
            giEls(i) = giEls(i) - 1
         End Do
         Call ISCreateGeneral (PETSC_COMM_WORLD, NelL, giEls,
     &        PETSC_COPY_VALUES, isPels, iError)
         DeAllocate(giEls)
         Call VecCreateMPI (PETSC_COMM_WORLD,NelL,NelT,vlElems,iError)
         Call VecGetOwnershipRange(vlElems, nFirstE, nLastE, iError)
         Call ISCreateStride (PETSC_COMM_WORLD, NelL, nFirstE, 1,
     &        isPar, iError)
         Call VecScatterCreate (vElems, isPels, vlElems, isPar,
     &        sgcElems, iError)
         Call ISDestroy(isPar, iError)
!
         Call VecScatterBegin(sgcElems, vElems, vlElems,
     &        INSERT_VALUES, SCATTER_FORWARD, iError)
         Call VecScatterEnd(sgcElems, vElems, vlElems,
     &        INSERT_VALUES, SCATTER_FORWARD, iError)
!     Pass data from PETSC vectors to integer local vectors
         Allocate(lElType(NelL))
         If (NelL .gt. 0) Then
            Call VecGetArrayF90(vlElems, ivPTSC, iError)
            Do i = 1, NelL
!     write (*,*) irank, ivptsc(i)
               lElType(i) = ivPTSC(i)
            End Do
            Call VecRestoreArrayF90(vlElems, ivPTSC, iError)
         End If
!
!     Now ELEMENT MAT (same pattern, use also petsc vectors vElems, vlElems)
!
         If (iRank .eq. 0) Then
            Str='*ELEMENT MAT'
            iError = iFindStringInFile(Str, IO_Mesh)
            if (iError .ne. 0) then
               Write(6,*) Str, "NOT FOUND"
               Stop
            end if
            Call VecGetArrayF90(vElems, ivPTSC, iError)
            Read (IO_Mesh, *) (ivPTSC(i), i = 1, NelT)
            Call VecRestoreArrayF90(vElems, ivPTSC, iError)
         End If
!
         Call VecScatterBegin(sgcElems, vElems, vlElems,
     &        INSERT_VALUES, SCATTER_FORWARD, iError)
         Call VecScatterEnd(sgcElems, vElems, vlElems,
     &        INSERT_VALUES, SCATTER_FORWARD, iError)
!     Pass data from PETSC vectors to integer local vectors
         Allocate(lElMat(NelL))
         If (NelL .gt. 0) Then
            Call VecGetArrayF90(vlElems, ivPTSC, iError)
            Do i = 1, NelL
!     write (*,*) irank, ivptsc(i)
               lElMat(i) = ivPTSC(i)
            End Do
         End If
         Call VecRestoreArrayF90(vlElems, ivPTSC, iError)
!
!         Str='After distributing elements'
!         Call MemReport(iRank, nProcs, Str)
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        '...ready.'//char(10), iError)
!
!     These vectors will be needed to process Param.txt
!     Call VecScatterDestroy(sgcElems, iError)
!     Call VecDestroy(vlElems, iError)
!     Call VecDestroy(vElems, iError)
!
!     Dirichlet Conditions, same as unknowns vector, and similar to
!     coordinates, should be a parallel vector with ghost values
!
         Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &        'Reading Dirichlet Conditions...'//char(10), iError)
         Allocate (viDir(NSubSteps))
         Allocate (vcDir(NSubSteps))
!
!     First, we create the "sequential" vector for reading
!     (distributed, but all values in master)
!
         nn = 0
         If (iRank .eq. 0) nn = NodT*iDoFT
         Call VecCreateMPI(PETSC_COMM_WORLD, nn,NodT*iDoFT,vSDir,iError)
!
!     Find tag in input file, read later
!
         If (iRank .eq. 0) Then
            Str='*DIRICHLET CONDITIONS'
            iError = iFindStringInFile(Str, IO_Mesh)
            if (iError .ne. 0) then
               Write(6,*) Str, "NOT FOUND"
               Stop
            end if
         End If
!
!     And set the elements to transfer to the distributed vector
!
         Call ISCreateStride(PETSC_COMM_WORLD, NodL*iDoFT,
     &        (nFirst-1)*iDoFT, 1, isSeq, iError)
!
!     Create the vectors with the ghost values
!
         Allocate (iaux(NnlNodes*iDoFT))
         Do i = 1, NnlNodes
            Do j = 1, iDoFT
               iaux((i-1)*iDoFT+j) = (nlNodes(i)-1)*iDoFT + j - 1
            End Do
         End Do
!
!     all viDir(?) and vcDir(?) vectors use the same layout
!     We create viDir(1) and duplicate it (the values are not copied!)
!
         Call VecCreateGhost (PETSC_COMM_WORLD,
     &        NodL*iDoFT, NodT*iDoFT, NnlNodes*iDoFT, iaux,
     &        viDir(1), iError)
         DeAllocate(iaux)
!
         Do iSS = 1, NSubSteps
            If (iSS .gt. 1) Then
               Call VecDuplicate(viDir(1), viDir(iSS), iError)
            End If
            Call VecDuplicate(viDir(1), vcDir(iSS), iError)
         End Do
!
!     Build index set to transfer values from file
!     vSDir -> viDir
!     isSeq -> isPar
!
         Allocate(iaux(NodL*iDoFT))
         Call ISGetIndicesF90 (isPerm, iPTSC, iError)
         Do i = 1, NodL
            Do j = 1, iDoFT
               iaux((i-1)*iDoFT+j) = iPTSC(i)*iDoFT + j - 1
            End Do
         End Do
         Call ISRestoreIndicesF90(isPerm, iPTSC, iError)
         Call ISCreateGeneral (PETSC_COMM_WORLD, NodL*iDoFT, iaux,
     &        PETSC_USE_POINTER, isPar, iError)
!
!     And create scatter-gather context sgcVUnk
!
         Call VecScatterCreate (vSDir, isSeq, viDir, isPar,
     &        sgcVUnk, iError)
         Call ISDestroy(isSeq, iError)
         Call ISDestroy(isPar, iError)
         DeAllocate(iaux)
!
!     Read and distribute the data NOW, first the indicators
!
         Do iSS = 1, NSubSteps
            If (iRank .eq. 0) Then
               Call VecGetArrayF90(vSDir, ivPTSC, iError)
               Read (IO_Mesh, *) (ivPTSC(i), i = 1, NodT*iDoFT)
               Call VecRestoreArrayF90(vSDir, ivPTSC, iError)
            End If
!
            Call VecScatterBegin (sgcVUnk, vSDir, viDir(iSS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
            Call VecScatterEnd (sgcVUnk, vSDir, viDir(iSS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
            Call VecGhostUpdateBegin(viDir(iSS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
            Call VecGhostUpdateEnd(viDir(iSS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)

         End Do
!
!     Do the same for the *values* of the Dirichlet Conditions
!
         Do iSS = 1, NSubSteps
            If (iRank .eq. 0) Then
               Call VecGetArrayF90(vSDir, ivPTSC, iError)
               Read (IO_Mesh, *) (ivPTSC(i), i = 1, NodT*iDoFT)
               Call VecRestoreArrayF90 (vSDir, ivPTSC, iError)
            End If
!
            Call VecScatterBegin (sgcVUnk, vSDir, vcDir(iSS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
            Call VecScatterEnd (sgcVUnk, vSDir, vcDir(iSS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
            Call VecGhostUpdateBegin(vcDir(iSS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
            Call VecGhostUpdateEnd(vcDir(iSS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
         End Do
!
!     Sequential vector won't be needed again
!
         Call VecDestroy(vSDir, iError)
!
!     We do keep sgcVUnk for reading Inifile.txt...
!
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        '...ready.'//char(10), iError)
         If (iRank .eq. 0) Then
            Close(IO_Mesh)
         End If
!         Str='After distributing Dirichlet Conditions'
         Str='After processing mesh data'
         Call MemReport(iRank, nProcs, Str)
!
!     Mesh.txt ready, proceed with Param.txt
!
         Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &        'Reading parameters...'//char(10), iError)
         If (iRank .eq. 0) Then ! Only main processor
            IO_Par = 14
            iniError = 1
            Open (IO_Par, FILE='Param.txt', STATUS='OLD', ERR=60)
            iniError = 0
 60         Continue
            If (iniError .eq. 0) Then
               Str='*Parameter Groups'
               iError = iFindStringInFile (Str, IO_Par)
               If (iError .ne. 0) Then
                  iniError = 1
                  Close (IO_Par)
               Else
                  READ (IO_Par, *) MaxId_DataSet
               End If
            End If
         End If
!
         Call MPI_Bcast(iniError,1,MPI_INTEGER, 0, PETSC_COMM_WORLD, I)
         If (iniError .ne. 0) Then
            Call PETSCPRINTF(PETSC_COMM_WORLD,
     &           'Error reading Param.txt'//char(10), iError)
            Call PETSCFINALIZE(iError)
            Stop
         End If
!
         Call MPI_Bcast(MaxId_Dataset, 1, MPI_INTEGER, 0,
     &        PETSC_COMM_WORLD, iError)
!
!     Master continues reading file
!
         If (iRank .eq. 0) Then
!
!     Number of groups of parameters
!
            Allocate(IE_Par(MaxId_DataSet + 1))
            Allocate(IE_JPar(MaxId_DataSet + 1))
!
!     Parametros reales
!
            Str = '*Real Parameters'
            iError = iFindStringInFile (Str, IO_Par)
            if (iError .ne. 0) then
               write(6,*) Str//' NOT FOUND'
               Stop
            end if
!
            Read (IO_Par, *) (IE_Par(I), I = 1, MaxId_DataSet)
            Length_Param = IEarrange(IE_Par, MaxId_DataSet)
         End If
!
         Call MPI_Bcast(Length_Param, 1, MPI_INTEGER, 0,
     &        PETSC_COMM_WORLD, iError)
         nn = 0
         If (iRank .eq. 0) nn = Length_Param
         Call VecCreateMPI(PETSC_COMM_WORLD, nn, PETSC_DETERMINE,
     &        vSParam, iError)
!
         If (iRank .eq. 0) Then
!     Delay reading until VecScatter context is created
!
!     Parametros enteros
!
            Str = '*Integer Parameters'
            iError = iFindStringInFile (Str, IO_Par)
            if (iError .ne. 0) then
               write (6, *) Str//' NOT FOUND'
               Stop
            end if
            Read (IO_Par, *) (IE_JPar(I), I = 1, MaxId_DataSet)
            Length_JParam = IEarrange(IE_JPar, MaxId_DataSet)
         End If
!
          Call MPI_Bcast(Length_JParam, 1, MPI_INTEGER, 0,
     &        PETSC_COMM_WORLD, iError)
!
        nn = 0
         If (iRank .eq. 0) nn = Length_JParam
         Call VecCreateMPI(PETSC_COMM_WORLD, nn, PETSC_DETERMINE,
     &        vSJParam, iError)
!
!     We use vElems, (vlElems), sgcElems previously computed to distribute data
!     vElems should contain the element material (fully in master)
!
         If (iRank .eq. 0) Then
            Allocate(Mater(NelT))
            Call VecGetArrayF90(vElems, ivPTSC, iError)
            Do i = 1, NelT
               Mater(i) = INT(ivPTSC(i))
            End Do
            Call VecRestoreArrayF90(vElems, ivPTSC, iError)
         End If
!
!     Build the "partition" vector
!
         Call VecGetArrayF90(vlElems, ivPTSC, iError)
         Do i = 1, NelL
            ivPTSC(i) = iRank
         End Do
         Call VecRestoreArrayF90(vlElems, ivPTSC, iError)
!
!     Send to master
!
         Call VecScatterBegin(sgcElems, vlElems, vElems,
     &        INSERT_VALUES, SCATTER_REVERSE, iError)
         Call VecScatterEnd(sgcElems, vlElems, vElems,
     &        INSERT_VALUES, SCATTER_REVERSE, iError)
!
!     Master counts data to be sent to workers
!     
         If (iRank .eq. 0) Then
            Allocate (msw(MaxId_DataSet))
            Allocate (iPartPar(MaxId_DataSet))
            iPartPar = -1       ! Scalar -> vector assignment
            Call VecGetArrayF90(vElems, ivPTSC, iError)
            Do iother = nProcs-1, 0, -1
               lenlIE = 1
               lenlJE = 0
               lenlJJE = 0
               msw = 0
!     Sweeps all elements
               Do iel = 1, NelT
!     Check for element assigned to iother process
                  If (ivPTSC(iel) .ne. iother) Cycle
!     Get element material
                  Matiel = Mater(iel)
!     Must store locally this parameters if not marked
                  If (msw(Matiel) .eq. 0) Then
!     Position where this parameters are stored locally
                     msw(Matiel) = lenlIE
                     lenlIE = lenlIE + 1
                     lenlJE = lenlJE + IE_Par(Matiel+1) - IE_Par(Matiel)
                     lenlJJE= lenlJJE+ IE_JPar(Matiel+1)-IE_JPar(Matiel)
                  End If
!     Check for GoP assignment
                  If (iPartPar(Matiel) .lt. 0) Then
!
!     Still not assigned, assign to this process
!     index in global (master owned, read from file) vector: Matiel
!     index in process iother: msw(Matiel)
!
                     iPartPar(Matiel) = iother
                  End If
               End Do   ! Over full element list
!
!     Sizes computed, if iother .eq. 0 must add also the not-assigned GoPs
!
               If (iother .eq. 0) Then
                  Do i = 1, MaxId_DataSet
                     If (iPartPar(i) .lt. 0) Then
                        iPartPar(i) = iother ! 0
!     Sum sizes even when not used
                        msw(i) = lenlIE
                        lenlIE = lenlIE + 1
                        lenlJE = lenlJE + IE_Par(i+1) - IE_Par(i)
                        lenlJJE = lenlJJE + IE_JPar(i+1) - IE_JPar(i)
                     End If
                  End Do
               End If
!
!     Alloc memory and do it again storing results
!
               Allocate(lIE_Par(lenlIE))
               Allocate(lIE_JPar(lenlIE))
!               Allocate(lParam(lenlJE))
!               Allocate(lJParam(lenlJJE))
               nGoPs = lenlIE - 1
!
!     giPars and giJPars contains the global index of the enumeration
!     of the Group of Parameters (GoP) in vectors Param and JParam,
!     for those GoP assigned to this process
!
!     The GoPs stored locally but assigned to another process have "0"
!     in giPars and giJPars
!
               Allocate(giGoP(nGoPs))
               Allocate(giPar(nGoPs))
               Allocate(giJPar(nGoPs))
               lenlIE = 1
               lIE_Par(1) = 1
               lIE_JPar(1) = 1
               lenlJE = 0
               lenlJJE = 0
!     Sweeps all elements
               Do iel = 1, NelT
!     Check for element assigned to iother process
                  If (ivPTSC(iel) .ne. iother) Cycle
!     Get element material
                  Matiel = Mater(iel)
!
!     Here we can renumber the element Material (Mater(iel))
!     to point to the local vectors
!
                  Mater(iel) = msw(Matiel)
!
!     Must store locally this GoP when marked with lenlIE
!     (if msw(Matiel) .lt. lenlIE) it's already stored
!     index in global (master owned, read from file) vector: Matiel
!     index in process iother: msw(Matiel)
!
                  If (msw(Matiel) .eq. lenlIE) Then
                     iea = IE_Par(Matiel)
                     ieb = IE_Par(Matiel+1)
                     lenlJE = lenlJE + ieb - iea
                     ieja = IE_JPar(Matiel)
                     iejb = IE_JPar(Matiel+1)
                     lenlJJE = lenlJJE + iejb - ieja
                     If (iPartPar(Matiel) .eq. iother) Then
                        giGoP(lenlIE) = Matiel
                        giPar(lenlIE) = iea
                        giJPar(lenlIE) = ieja
                     Else
                        giGop(lenlIE) = -Matiel
                        giPar(lenlIE) = iea
                        giJPar(lenlIE) = ieja
                     End If
                     lenlIE = lenlIE + 1
                     lIE_Par (lenlIE) = lenlJE + 1
                     lIE_JPar(lenlIE) = lenlJJE + 1
                  End If
               End Do   ! Over full element list
!
!     Local GoPs ready, if (iother .eq. 0)
!     must store also the not-assigned GoPs
!     iPartPar already loaded, so we identify these GoPs using msw
!
               If (iother .eq. 0) Then
                  Do iGoP = 1, MaxId_DataSet
                     If (msw(iGoP) .eq. lenlIE) Then
                        iea = IE_Par(iGoP)
                        ieb = IE_Par(iGoP+1)
                        lenlJE = lenlJE + ieb - iea
                        ieja = IE_JPar(iGoP)
                        iejb = IE_JPar(iGoP+1)
                        lenlJJE = lenlJJE + iejb - ieja
!
                        giGoP(lenlIE) = iGoP
                        giPar(lenlIE) = iea
                        giJPar(lenlIE) = ieja
!
                        lenlIE = lenlIE + 1
                        lIE_Par (lenlIE) = lenlJE + 1
                        lIE_JPar(lenlIE) = lenlJJE + 1
                     End If
                  End Do
               End If
!
!     Local parameters indexes built, send to workers
!
               nGoPs = lenlIE - 1
               If (iother .gt. 0) Then ! Send to workers, do not auto-send
                  Call MPI_Send (nGops, 1, MPI_INTEGER,
     &                 iother, 1, MPI_COMM_WORLD, iError) ! IE size
                  Call MPI_Send (lenlJE, 1, MPI_INTEGER,
     &                 iother, 2, MPI_COMM_WORLD, iError) ! JE size
                  Call MPI_Send (lenlJJE, 1,MPI_INTEGER,
     &                 iother, 3, MPI_COMM_WORLD, iError) ! JEJ size
                  Call MPI_Send (lIE_Par, lenlIE, MPI_INTEGER,
     &                 iother, 4, MPI_COMM_WORLD, iError) ! IE Par
                  Call MPI_Send (lIE_JPar, lenlIE, MPI_INTEGER,
     &                 iother, 5, MPI_COMM_WORLD, iError) ! IE JPar
                  Call MPI_Send (giGoP, nGoPs, MPI_INTEGER,
     &                 iother, 8, MPI_COMM_WORLD, iError) ! giGoP
                  Call MPI_Send (giPar, nGoPs, MPI_INTEGER,
     &                 iother, 9, MPI_COMM_WORLD, iError) ! giPar
                  Call MPI_Send (giJPar, nGoPs, MPI_INTEGER,
     &                 iother, 10, MPI_COMM_WORLD, iError) ! giJPar
                  DeAllocate(lIE_Par)
                  DeAllocate(lIE_JPar)
                  DeAllocate(giGoP)
                  DeAllocate(giPar)
                  DeAllocate(giJPar)
               End If           ! Send to workers, do not auto-send
!
            End Do              ! Loop over processes
            Call VecRestoreArrayF90(vElems, ivPTSC, iError)
!
!     List of element materials,
!     renumbered to point to local vectors is ready in vector Mater,
!     Store in vector vElems, previous to scatter
!
            Call VecGetArrayF90(vElems, ivPTSC, iError)
            Do iel = 1, NelT
               ivPTSC(iel) = Mater(iel)
            End Do
            Call VecRestoreArrayF90(vElems, ivPTSC, iError)
!
            DeAllocate(Mater)
!
         Else                   ! Workers
!
!     Receive from master locally needed "parameters"
!     (in fact: only sizes and pointers, all except actual numerical values)
!
            Call MPI_Recv (nGoPs, 1, MPI_INTEGER,
     &           0, 1, MPI_COMM_WORLD, status, iError)
            lenlIE = nGoPs + 1
            Call MPI_Recv (lenlJE, 1, MPI_INTEGER,
     &           0, 2, MPI_COMM_WORLD, status, iError)
            Call MPI_Recv (lenlJJE, 1, MPI_INTEGER,
     &           0, 3, MPI_COMM_WORLD, status, iError)
            Allocate(lIE_Par(lenlIE))
            Allocate(lIE_JPar(lenlIE))
            Allocate(giGoP(nGoPs))
            Allocate(giPar(nGoPs))
            Allocate(giJPar(nGoPs))
            Call MPI_Recv(lIE_Par, lenlIE, MPI_INTEGER,
     &           0, 4, MPI_COMM_WORLD, status, iError)
            Call MPI_Recv(lIE_JPar, lenlIE, MPI_INTEGER,
     &           0, 5, MPI_COMM_WORLD, status, iError)
            Call MPI_Recv(giGoP, nGoPs, MPI_INTEGER,
     &           0, 8, MPI_COMM_WORLD, status, iError)
            Call MPI_Recv(giPar, nGoPs, MPI_INTEGER,
     &           0, 9, MPI_COMM_WORLD, status, iError)
            Call MPI_Recv(giJPar, nGoPs, MPI_INTEGER,
     &           0, 10, MPI_COMM_WORLD, status, iError)
!
         End If                    ! Master Process / Others
!
!     Get list of element materials, renumbered vElems -> vlElems
!
         Call VecScatterBegin(sgcElems, vElems, vlElems,
     &        INSERT_VALUES, SCATTER_FORWARD, iError)
         Call VecScatterEnd(sgcElems, vElems, vlElems,
     &        INSERT_VALUES, SCATTER_FORWARD, iError)
!
!     And pass to local vector
!
         Call VecGetArrayF90(vlElems, ivPTSC, iError)
         Do iel = 1, NelL
            lElMat(iel) = INT(ivPTSC(iel))
         End Do
         Call VecRestoreArrayF90(vlElems, ivPTSC, iError)
!
!     Create vectors for local parameters
!
         Allocate(vParam(nOldTimeSteps+1))
         Allocate(vJParam(nOldTimeSteps+1))
!     Pre-alloc memory for vectors, so that they are sequentially stored
         Allocate(auxParam((nOldTimeSteps+1)*lenlJE))
         Allocate(auxJParam((nOldTimeSteps+1)*lenlJJE))
!
!         Call VecCreateSeq(PETSC_COMM_SELF, lenlJE, vParam(1), iError)
!         Call VecCreateSeq(PETSC_COMM_SELF, lenlJJE, vJParam(1), iError)
!         Do iTS = 1, nOldTimeSteps
!            Call VecDuplicate(vParam(1), vParam(iTS+1), iError)
!            Call VecDuplicate(vJParam(1), vJParam(iTS+1), iError)
!         End Do
         Do iTS = 1, nOldTimeSteps + 1
            Call VecCreateSeqWithArray(PETSC_COMM_SELF, lenlJE,
     &           auxParam(1+lenlJE*(iTS-1)), vParam(iTS), iError)
            Call VecCreateSeqWithArray(PETSC_COMM_SELF, lenlJJE,
     &           auxJParam(1+lenlJJE*(iTS-1)), vJParam(iTS), iError)
         End Do
!
c$$$         Do iTS = 1, nOldTimeSteps
!
!     Build index sets to distribute REAL parameters
!     (nGoPs: Number of Groups of Parameters stored locally)
!     First: GoP that belongs to other process,
!     but are also needed here (marked with giGoP < 0)
!
            Allocate(iaux(lenlJE))
            Allocate(jaux(lenlJE))
            isizP = 0           ! Index size for distributing Param
            Do i = 1, nGoPs
               If (giGoP(i) .lt. 0) Then
                  Do k = lIE_Par(i), lIE_Par(i+1) - 1
                     isizP = isizP + 1
                     iaux(isizP) = k - 1 ! Index over local Param
                     jaux(isizP) = ! Index over global Param
     &                    giPar(i) + k - lIE_Par(i) - 1
c$$$     &                    + (iTS-1) * Length_Param
                  End Do
               End If
            End Do
!
            Call ISCreateGeneral(PETSC_COMM_SELF, isizP, jaux,
     &           PETSC_USE_POINTER, isSeq, iError)
            Call ISCreateGeneral(PETSC_COMM_SELF, isizP, iaux,
     &           PETSC_USE_POINTER, isPar, iError)
!
!     Create S-G context
!
            Call VecScatterCreate (vSParam, isSeq, vParam(1), isPar,
     &           sgcParam0, iError)
            call ISDestroy(isSeq, iError)
            call ISDestroy(isPar, iError)
!
!     Apply (distribute)
!
c$$$            Call VecScatterBegin(sgcParam, vSParam, vParam(iTS),
c$$$     &           INSERT_VALUES, SCATTER_FORWARD, iError)
c$$$            Call VecScatterEnd(sgcParam, vSParam, vParam(iTS),
c$$$     &           INSERT_VALUES, SCATTER_FORWARD, iError)
c$$$            Call VecScatterDestroy(sgcParam, iError)
!
!     Now GoPs assigned to this process (those with giGoP > 0)
!
            isizP = 0           ! Index size for distributing Param
            Do i = 1, nGoPs
               If (giGoP(i) .gt. 0) Then
                  Do k = lIE_Par(i), lIE_Par(i+1) - 1
                     isizP = isizP + 1
                     iaux(isizP) = k - 1 ! Index over local Param
                     jaux(isizP) = ! Index over global Param
     &                    giPar(i) + k - lIE_Par(i) - 1
c$$$     &                    + (iTS-1) * Length_Param
                  End Do
               End If
            End Do
!
            Call ISCreateGeneral(PETSC_COMM_SELF, isizP, jaux,
     &           PETSC_USE_POINTER, isSeq, iError)
            Call ISCreateGeneral(PETSC_COMM_SELF, isizP, iaux,
     &           PETSC_USE_POINTER, isPar, iError)
!
!     Create S-G context
!
            Call VecScatterCreate (vSParam, isSeq, vParam(1), isPar,
     &           sgcParam, iError)
            call ISDestroy(isSeq, iError)
            call ISDestroy(isPar, iError)
            DeAllocate(iaux)
            DeAllocate(jaux)
!
!     Apply (distribute)
!
c$$$            Call VecScatterBegin(sgcParam, vSParam, vParam(iTS),
c$$$     &           INSERT_VALUES, SCATTER_FORWARD, iError)
c$$$            Call VecScatterEnd(sgcParam, vSParam, vParam(iTS),
c$$$     &           INSERT_VALUES, SCATTER_FORWARD, iError)   
!
!     Repeat for JParam
!     First: GoP that belongs to other process,
!     but are also needed here (marked with giGoP < 0)
!
            Allocate(iaux(lenlJJE))
            Allocate(jaux(lenlJJE))
            isizP = 0           ! Index size for distributing Param
            Do i = 1, nGoPs
               If (giGoP(i) .lt. 0) Then
                  Do k = lIE_JPar(i), lIE_JPar(i+1) - 1
                     isizP = isizP + 1
                     iaux(isizP) = k -1 ! Index over local Param
                     jaux(isizP) = ! Index over global Param
     &                    giJPar(i) + k - lIE_JPar(i) - 1
c$$$     &                    + (iTS-1) * Length_JParam
                  End Do
               End If
            End Do
!
            Call ISCreateGeneral(PETSC_COMM_SELF, isizP, jaux,
     &           PETSC_USE_POINTER, isSeq, iError)
            Call ISCreateGeneral(PETSC_COMM_SELF, isizP, iaux,
     &           PETSC_USE_POINTER, isPar, iError)
!
!     Create S-G context
!
            Call VecScatterCreate (vSJParam, isSeq, vJParam(1), isPar,
     &           sgcJParam0, iError)
            call ISDestroy(isSeq, iError)
            call ISDestroy(isPar, iError)
!
!     Apply (distribute)
!
c$$$            Call VecScatterBegin(sgcJParam, vSJParam, vJParam(iTS),
c$$$     &           INSERT_VALUES, SCATTER_FORWARD, iError)
c$$$            Call VecScatterEnd(sgcJParam, vSJParam, vJParam(iTS),
c$$$     &           INSERT_VALUES, SCATTER_FORWARD, iError)
c$$$            Call VecScatterDestroy(sgcJParam, iError)
!
!     Now GoPs assigned to this process (those with giGoP > 0)
!
            isizP = 0           ! Index size for distributing Param
            Do i = 1, nGoPs
               If (giGoP(i) .gt. 0) Then
                  Do k = lIE_JPar(i), lIE_JPar(i+1) - 1
                     isizP = isizP + 1
                     iaux(isizP) = k -1 ! Index over local Param
                     jaux(isizP) = ! Index over global Param
     &                    giJPar(i) + k - lIE_JPar(i) - 1
c$$$     &                    + (iTS-1) * Length_JParam
                  End Do
               End If
            End Do
!
            Call ISCreateGeneral(PETSC_COMM_SELF, isizP, jaux,
     &           PETSC_USE_POINTER, isSeq, iError)
            Call ISCreateGeneral(PETSC_COMM_SELF, isizP, iaux,
     &           PETSC_USE_POINTER, isPar, iError)
!
!     Create S-G context
!
            Call VecScatterCreate (vSJParam, isSeq, vJParam(1), isPar,
     &           sgcJParam, iError)
            call ISDestroy(isSeq, iError)
            call ISDestroy(isPar, iError)
            DeAllocate(iaux)
            DeAllocate(jaux)
!
!     Apply (distribute)
!
c$$$            Call VecScatterBegin(sgcJParam, vSJParam, vJParam(iTS),
c$$$     &           INSERT_VALUES, SCATTER_FORWARD, iError)
c$$$            Call VecScatterEnd(sgcJParam, vSJParam, vJParam(iTS),
c$$$     &           INSERT_VALUES, SCATTER_FORWARD, iError)
!
!     Loop over time steps
!
c$$$         End Do
!         Str='Distributing Parameters'
!         Call MemReport(iRank, nProcs, Str)
!
!     Now read and distribute
!
         If (iRank .eq. 0) Then
            Str = '*Real Parameters'
            iError = iFindStringInFile (Str, IO_Par)
            if (iError .ne. 0) then
               write(6,*) Str//' NOT FOUND'
               Stop
            end if
!     Read again, just for positioning in the file
            Read (IO_Par, *) (IE_Par(I), I = 1, MaxId_DataSet)
!     Most probably IE_Par will not be used, but you never know...
            Length_Param = IEarrange(IE_Par, MaxId_DataSet)
         End If
!
         Do iTS = 1, nOldTimeSteps
            If (iRank .eq. 0 .and. Length_Param .gt. 0) Then
               Call VecGetArrayF90(vSParam, ivPTSC, iError)
               Read (IO_Par, *) (ivPTSC(I),
     &              I = 1, Length_Param)
               Call VecRestoreArrayF90(vSParam, ivPTSC, iError)
            End If
!     Distribute values needed but assigned to another process
            Call VecScatterBegin(sgcParam0, vSParam, vParam(iTS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
            Call VecScatterEnd(sgcParam0, vSParam, vParam(iTS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
!     Distribute values assigned to this process
            Call VecScatterBegin(sgcParam, vSParam, vParam(iTS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
            Call VecScatterEnd(sgcParam, vSParam, vParam(iTS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
         End Do
!     This won't be needed again
         Call VecScatterDestroy(sgcParam0, iError)
!
!     Now read and distribute the Integer Parameters
!
         If (iRank .eq. 0) Then
            Str = '*Integer Parameters'
            iError = iFindStringInFile (Str, IO_Par)
            if (iError .ne. 0) then
               write(6,*) Str//' NOT FOUND'
               Stop
            end if
!     Read again, just for positioning in the file
            Read (IO_Par, *) (IE_JPar(I), I = 1, MaxId_DataSet)
!     Most probably IE_JPar will not be used, but you never know...
            Length_JParam = IEarrange(IE_JPar, MaxId_DataSet)
         End If
!
         Do iTS = 1, nOldTimeSteps
            If (iRank .eq. 0 .and. Length_JParam .gt. 0) Then
               Call VecGetArrayF90(vSJParam, ivPTSC, iError)
               Read (IO_Par, *) (ivPTSC(I),
     &              I = 1, Length_JParam)
               Call VecRestoreArrayF90(vSJParam, ivPTSC, iError)
            End If
!     Distribute values needed but assigned to another process
            Call VecScatterBegin(sgcJParam0, vSJParam, vJParam(iTS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
            Call VecScatterEnd(sgcJParam0, vSJParam, vJParam(iTS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
!     Distribute values assigned to this process
            Call VecScatterBegin(sgcJParam, vSJParam, vJParam(iTS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
            Call VecScatterEnd(sgcJParam, vSJParam, vJParam(iTS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
         End Do
!     Finished reading parameters
         If (iRank .eq. 0) Then
            Close(IO_Par)
         End If
!
!     This won't be needed again
         Call VecScatterDestroy(sgcJParam0, iError)
!
!     Copy also last set of parameters over current
!
         Call VecCopy (vParam(nOldTimeSteps), vParam(nOldTimeSteps+1),
     &        iError)
         Call VecCopy (vJParam(nOldTimeSteps), vJParam(nOldTimeSteps+1),
     &        iError)
!
!     Sequential Parameters vector no longer needed
!     Same for scatter-gather context to distribute them
!     Unless the output is sequential 
!
         If (iWPO .eq. 1) Then
            Call VecDestroy (vSParam, iError)
            Call VecDestroy (vSJParam, iError)
            Call VecScatterDestroy (sgcParam, iError)
            Call VecScatterDestroy (sgcJParam, iError)
         End If
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        '...ready.'//char(10), iError)
!
!     Read and distribute initial conditions
!     Inifile.txt   nOldTimeSteps
!
         Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &        'Reading initial conditions...'//char(10), iError)
!
         If (iRank .eq. 0) Then ! Only main processor
            IO_Ini = 15
            iniError = 1
            Open (IO_Ini, FILE='IniFile.txt', STATUS='OLD', ERR=80)
            iniError = 0
 80         Continue
            If (iniError .eq. 0) Then
               Str='*Initial Conditions'
               iError = iFindStringInFile (Str, IO_Ini)
               If (iError .ne. 0) Then
                  iniError = 1
                  Close (IO_Ini)
               End If
            End If
         End If
!
         Call MPI_Bcast(iniError,1,MPI_INTEGER, 0, PETSC_COMM_WORLD, I)
         If (iniError .ne. 0) Then
            Call PetscPrintf (PETSC_COMM_WORLD,
     &           'Error reading IniFile.txt'//char(10), iError)
            Call PetscFinalize(iError)
            Stop
         End If
!
         Allocate (DSols(nOldTimeSteps))
!
!     First, we create the "sequential" vector for reading
!     (distributed, but all values in master)
!
         nn = 0
         If (iRank .eq. 0) nn = NodT*iDoFT
         Call VecCreateMPI(PETSC_COMM_WORLD, nn,NodT*iDoFT,SSol,iError)
!
!     Create distributed values, same layout as dirichlet vectors
!
         Do iTS = 1, nOldTimeSteps
            Call VecDuplicate(viDir(1), DSols(iTS), iError)
         End Do
!
!     Read and distribute the data NOW
!
         Do iTS = 1, nOldTimeSteps
            If (iRank .eq. 0) Then
               Call VecGetArrayF90(SSol, ivPTSC, iError)
               Read (IO_Ini, *) (ivPTSC(i), i = 1, NodT*iDoFT)
               Call VecRestoreArrayF90(SSol, ivPTSC, iError)
            End If
!
            Call VecScatterBegin (sgcVUnk, SSol, DSols(iTS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
            Call VecScatterEnd (sgcVUnk, SSol, DSols(iTS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
            Call VecGhostUpdateBegin(DSols(iTS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
            Call VecGhostUpdateEnd(DSols(iTS),
     &           INSERT_VALUES, SCATTER_FORWARD, iError)
         End Do
!
!         Str='Reading IniFile.txt'
!         Call MemReport(iRank, nProcs, Str)
         If (iWPO .eq. 1) Then
            Call VecDestroy (SSol, iError)
            Call VecScatterDestroy (sgcVUnk, iError)
         End If
!
!     Time ?
!
         If (iRank .eq. 0) Then
            Str='*Time'
            iError = iFindStringInFile (Str, IO_Ini)
            If (iError .ne. 0) Then
               If (initialTime .ne. 0) Then
                  Call PetscPrintf (PETSC_COMM_SELF,
     &                 Str//' not found.'//char(10), iError)
                  Call PetscPrintf (PETSC_COMM_SELF,
     &                 ' Will use Initial Time from Basparam.txt'//
     &                 char(10), iError)
                  initialTime = 0
               End If
            Else
               Read (IO_Ini, *) TiniIF, DelTIF
            End If
            Close (IO_Ini)
         End If
!
         Call MPI_Bcast(initialTime, 1, MPI_INTEGER, 0,
     &        PETSC_COMM_WORLD, iError)
         Call MPI_Bcast(TiniIF, 1, MPI_DOUBLE_PRECISION, 0,
     &        PETSC_COMM_WORLD, iError)
         Call MPI_Bcast(DelTIF, 1, MPI_DOUBLE_PRECISION, 0,
     &        PETSC_COMM_WORLD, iError)
!
         If (initialTime .ne. 0) Then
            Tini = TiniIF
         End If
!
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        '...ready.'//char(10), iError)
!
!     Ready reading IniFile.txt
!
         tlecS = noMPI_Wtime () - t0lecS
         Write(Str,"('Time reading sequential data',A1,"//
     &        "'(Mesh.txt, Params.txt, IniFile.txt):',"//
     &        " F12.3, ' Secs')") char(10), tlecS
         Len = Len_Trim(Str)
         Call PetscPrintf(PETSC_COMM_WORLD, Str(1:Len)//char(10),iError)
!
         Str='After sequential read'
         Call MemReport(iRank, nProcs, Str)
!
      End If   ! Block I: Reading from sequential file(s)
!
!
!     Block I - b:
!     Write parallel input files
!
!
      If (iRPI .eq. 0 .and. iWPI .gt. 0) Then ! Read Seq/Write Par INPUT
         t0wP = noMPI_Wtime ()
!
!     Write local meshes:
!
         Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &        'Writing parallel input files...'//char(10), iError)
         Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &        'Mesh_xxxx.txt ...'//char(10), iError)
!
         Write(Str,"('Mesh_',I0.4,'.txt')") iRank
         IO_MeshOut = 23
         Open(IO_MeshOut, FILE=Str, STATUS='UNKNOWN')
!
         Write(IO_MeshOut, "('This is part of a distributed mesh')")
         Write(IO_MeshOut,
     &        "('Global Mesh Sizes: # of nodes, # of elements')")
         Write(IO_MeshOut, "('Local Mesh Sizes: # of local nodes, "//
     &        "# of ghost nodes, # of local elements')")
         Write(IO_MeshOut, "('*Global Mesh Sizes')")
         Write(IO_MeshOut, *) NodT, NelT
         Write(IO_MeshOut, "('*Local Mesh Sizes')")
         Write(IO_MeshOut, *) NodL, NnlNodes, NelL
         Write(IO_MeshOut, "('*Ghost nodes')")
         Write(IO_MeshOut, "(10(x,I0))") (nlNodes(I), I = 1, NnlNodes)
         Write(IO_MeshOut, "('*Nodal Permutation')")
!
         Call ISGetIndicesF90 (isPerm, iPTSC, iError)
         Write(IO_MeshOut, "(10(x,I0))") (iPTSC(I), I = 1, NodL)
         Call ISRestoreIndicesF90(isPerm, iPTSC, iError)
!
         Write(IO_MeshOut, "('*Element Permutation (inverse)')")
         If (NelL .gt. 0) Then
            Call ISGetIndicesF90 (isPels, iPTSC, iError)
            Write(IO_MeshOut, "(10(x,I0))") ((iPTSC(I)+1), I = 1, NelL)
            Call ISRestoreIndicesF90(isPels, iPTSC, iError)
         End If
         Write(IO_MeshOut, "('*NODAL DOFs')")
         Write(IO_MeshOut, "(I0)") iDoFT
         Write(IO_MeshOut, "('*DIMEN')")
         Write(IO_MeshOut, "(I0)") nDim
         Write(IO_MeshOut, "('*COORDINATES')")
         Write(IO_MeshOut, "(' ',I0)") NodL + NnlNodes
         Call VecGhostGetLocalForm(DCoords, vvPTSC, iError)
         Call VecGetArrayF90(vvPTSC, ivPTSC, iError)
         Do i = 1, NodL + NnlNodes
!            Write (IO_MeshOut, *) (ivPTSC((i-1)*nDim+j), j = 1, nDim)
            Do j = 1, nDim
               Write (IO_MeshOut, '(x,ES16.8E3)', advance='no')
     &              ivPTSC((i - 1) * nDim + j)
            End Do
            Write (IO_MeshOut, "('')")
         End Do
         call VecRestoreArrayF90(vvPTSC, ivPTSC, iError)
         call VecGhostRestoreLocalForm(DCoords, vvPTSC, iError)
!
!     Vector of global indices of non-local nodes no longer needed ?
!     Needed! It holds the global rows for assembly !
!         DeAllocate(nlNodes)
         Write(IO_MeshOut, "('*ELEMENT GROUPS')")
         Write(IO_MeshOut, "('  1')")
         Write(IO_MeshOut, "('  1 ', I0, ' Generic')") NelL
         Do i = 1, NelL
            Write (IO_MeshOut, *) lIE(i+1) - lIE(i)
         End Do
         Write(IO_MeshOut, "('*INCIDENCE')")
         Do i = 1, NelL
            Write (IO_MeshOut, *) (lJE(j), j = lIE(i), lIE(i+1) - 1)
         End Do
         Write(IO_MeshOut, "('*ELEMENT TYPE')")
         Do i = 1, NelL
            Write (IO_MeshOut, *) lElType(i)
         End Do
         Write(IO_MeshOut, "('*ELEMENT MAT')")
         Do i = 1, NelL
            Write (IO_MeshOut, *) lElMat(i)
         End Do
         Write(IO_MeshOut, "('*DIRICHLET CONDITIONS')")
         Do iSS = 1, NSubSteps
            Call VecGhostGetLocalForm(viDir(iSS), vvPTSC, iError)
            Call VecGetArrayF90(vvPTSC, ivPTSC, iError)
            Do i = 1, NodL + NnlNodes
               Do j = 1, iDoFT
                  Write (IO_MeshOut, "(x,I0)", advance='no')
     &                 INT(ivPTSC((i-1)*iDoFT+j))
               End Do
               Write (IO_MeshOut, "('')")
            End Do
            Call VecRestoreArrayF90(vvPTSC, ivPTSC, iError)
            Call VecGhostRestoreLocalForm(viDir(iSS), vvPTSC, iError)
         End Do
         Do iSS = 1, NSubSteps
            Call VecGhostGetLocalForm(vcDir(iSS), vvPTSC, iError)
            Call VecGetArrayF90(vvPTSC, ivPTSC, iError)
            Do i = 1, NodL + NnlNodes
               Do j = 1, iDoFT
                  Write (IO_MeshOut, "(x,ES16.8E3)", advance='no')
     &                 ivPTSC((i-1)*iDoFT+j)
               End Do
               Write (IO_MeshOut, "('')")
            End Do
            Call VecRestoreArrayF90(vvPTSC, ivPTSC, iError)
            Call VecGhostRestoreLocalForm(vcDir(iSS), vvPTSC, iError)
         End Do
!
         Write(IO_MeshOut, "('*END')")
!
         Close(IO_MeshOut)
!
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        '...ready.'//char(10), iError)
!
c$$$!
c$$$!     Write local meshes: (viewmesh format)
c$$$!
c$$$      Call PetscPrintf (PETSC_COMM_WORLD,
c$$$     &     char(10)//'Writing local meshes...'//char(10), iError)
c$$$!
c$$$      Write(Str,"('Mesh_',I0.4,'.txt')") iRank
c$$$      IO_MeshOut = 23
c$$$      Open(IO_MeshOut, FILE=Str, STATUS='UNKNOWN')
c$$$!
c$$$      Call VecGhostGetLocalForm(DCoords, vvPTSC, iError);
c$$$      Call VecGetArrayF90(vvPTSC, ivPTSC, iError)
c$$$!
c$$$      Write(IO_MeshOut, "('*COORDINATES')")
c$$$      Write(IO_MeshOut, "(I0)") NodL + NnlNodes
c$$$      Do i = 1, NodL +  NnlNodes
c$$$         Write (IO_MeshOut, *) i, (ivPTSC((i-1)*nDim+j), j = 1, nDim)
c$$$      End Do
c$$$      Call VecRestoreArrayF90(vvPTSC, ivPTSC, iError)
c$$$      Call VecGhostRestoreLocalForm(DCoords, vvPTSC, iError);
c$$$      Write(IO_MeshOut, "('*ELEMENT_GROUPS')")
c$$$      Write(IO_MeshOut, "(' 1')")
c$$$      Write(IO_MeshOut, "(' 1 ',I0,' Tri3')") NelL
c$$$      Write(IO_MeshOut, "('*INCIDENCES')")
c$$$      Do i = 1, NelL
c$$$         Write (IO_MeshOut, *) (lJE(j), j = lIE(i), lIE(i+1) - 1)
c$$$      End Do
c$$$      Write(IO_MeshOut, "('*END')")
c$$$      Close(IO_MeshOut)
c$$$!
c$$$      Call PetscPrintf (PETSC_COMM_WORLD,
c$$$     &     '...ready.'//char(10), iError)
!
!      View Distributed coordinates
!
c$$$      Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
c$$$     &     'Reordered and distributed coordinates:'//char(10), iError)
c$$$      call VecGhostGetLocalForm(DCoords, vvPTSC, iError);
c$$$      call VecGetArrayF90(vvPTSC, ivPTSC, iError)
c$$$!
c$$$!     View coordinates (including ghost nodes)
c$$$!
c$$$      do i = 1, NodL + NnlNodes
c$$$         write(Str,*) iRank, ':', (ivPTSC((i-1)*nDim+j),j=1,nDim)
c$$$         Len = Len_Trim(Str)
c$$$         call PetscSynchronizedPrintf (PETSC_COMM_WORLD,
c$$$     &        Str(1:Len)//char(10), iError)
c$$$      end do
c$$$      call PetscSynchronizedFlush(PETSC_COMM_WORLD, iError)
c$$$!
c$$$      call VecRestoreArrayF90(vvPTSC, ivPTSC, iError)
c$$$      call VecGhostRestoreLocalForm(DCoords, vvPTSC, iError);
!
!
!     Write Parallel Param_xxxx.txt files
!
         Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &        'Param_xxxx.txt ...'//char(10), iError)
         !
         Write(Str,"('Param_',I0.4,'.txt')") iRank
         IO_ParOut = 24
         Open(IO_ParOut, FILE=Str, STATUS='UNKNOWN')
!
         Write(IO_ParOut,
     &        "('This is part of a distributed parameter list')")
         Write(IO_ParOut,
     &        "('Global Mesh Sizes: # of nodes, # of elements')")
         Write(IO_ParOut, "('Local Mesh Sizes: # of local nodes, "//
     &        "# of ghost nodes, # of local elements')")
         Write(IO_ParOut, "('Global parameters: # of Param. groups, "//
     &        "# of Real Params, # of Integer Params')")
         Write(IO_ParOut, "('Local parameters: # of Param. groups, "//
     &        "# of Real Params, # of Integer Params')")
!
         Write(IO_ParOut, "('*Global Mesh Sizes')")
         Write(IO_ParOut, *) NodT, NelT
         Write(IO_ParOut, "('*Local Mesh Sizes')")
         Write(IO_ParOut, *) NodL, NnlNodes, NelL
         Write(IO_ParOut, "('*Global parameters')")
         Write(IO_ParOut, *) MaxId_Dataset, Length_Param, Length_JParam
         Write(IO_ParOut, "('*Local parameters')")
         Write(IO_ParOut, *) nGoPs, lenlJE, lenlJJE
         Write(IO_ParOut, "('*Parameters mapping')")
         Write(IO_ParOut, "(10(x,I0))") (giGoP(i), i = 1, nGoPs)
         Write(IO_ParOut, "('*Pointers to Real Parameters')")
         Write(IO_ParOut, "(10(x,I0))") (giPar(i), i = 1, nGoPs)
         Write(IO_ParOut, "('*Pointers to Integer Parameters')")
         Write(IO_ParOut, "(10(x,I0))") (giJPar(i), i = 1, nGoPs)
!
!     Now the standard param.txt
!
         Write(IO_ParOut, "('*Parameter Groups')")
         Write(IO_ParOut, *) nGoPs
         Write(IO_ParOut, "('*Real Parameters')")
         Write(IO_ParOut, "(10(x,I0))")
     &        (lIE_Par(i+1)-lIE_Par(i), i = 1, nGoPs)
         Do iTS = 1, nOldTimeSteps
            Call VecGetArrayF90(vParam(iTS), ivPTSC, iError)
            Write(IO_ParOut, "(5(x,ES16.8E3))")
     &           (ivPTSC(i), i = 1, lenlJE)
            Call VecRestoreArrayF90(vParam(iTS), ivPTSC, iError)
         End Do
         Write(IO_ParOut, "('*Integer Parameters')")
         Write(IO_ParOut, "(10(x,I0))")
     &        (lIE_JPar(i+1)-lIE_JPar(i), i = 1, nGoPs)
         Do iTS = 1, nOldTimeSteps
            Call VecGetArrayF90(vJParam(iTS), ivPTSC, iError)
            Write (IO_ParOut, "(10(x,I0))")
     &           (INT(ivPTSC(i)), i = 1, lenlJJE)
            Call VecRestoreArrayF90(vJParam(iTS), ivPTSC, iError)
         End Do
         Write(IO_ParOut, "('*END')")
         Close(IO_ParOut)
!
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        '...ready.'//char(10), iError)
!
!     Write Parallel IniFile_xxxx.txt files
!     Same Header as in Mesh_xxxx.txt
!
         Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &        'IniFile_xxxx.txt ...'//char(10), iError)
!
         Write(Str,"('IniFile_',I0.4,'.txt')") iRank
         IO_IniOut = 25
         Open(IO_IniOut, FILE=Str, STATUS='UNKNOWN')
         Write(IO_IniOut, "('This is part of a distributed solution')")
         Write(IO_IniOut,
     &        "('Global Mesh Sizes: # of nodes, # of elements')")
         Write(IO_IniOut, "('Local Mesh Sizes: # of local nodes, "//
     &        "# of ghost nodes, # of local elements')")
         Write(IO_IniOut, "('*Global Mesh Sizes')")
         Write(IO_IniOut, *) NodT, NelT
         Write(IO_IniOut, "('*Local Mesh Sizes')")
         Write(IO_IniOut, *) NodL, NnlNodes, NelL
         Write(IO_IniOut, "('*Ghost nodes')")
         Write(IO_IniOut, "(10(x,I0))") (nlNodes(I), I = 1, NnlNodes)
         Write(IO_IniOut, "('*Nodal Permutation')")
!
         Call ISGetIndicesF90 (isPerm, iPTSC, iError)
         Write(IO_IniOut, "(10(x,I0))") (iPTSC(I), I = 1, NodL)
         Call ISRestoreIndicesF90(isPerm, iPTSC, iError)
!
         Write(IO_IniOut, "('*NODAL DOFs')")
         Write(IO_IniOut, "(I0)") iDoFT
         Write(IO_IniOut, "('*DIMEN')")
         Write(IO_IniOut, "(I0)") nDim
!
!     Now standard Inifile (with ghost nodes)
!
         Write(IO_IniOut, "('*Initial Conditions')")
         Do iTS = 1, nOldTimeSteps
            Call VecGhostGetLocalForm(DSols(iTS), vvPTSC, iError)
            Call VecGetArrayF90(vvPTSC, ivPTSC, iError)
            Do i = 1, NodL + NnlNodes
               Do j = 1, iDoFT
                  Write (IO_IniOut, '(x,ES16.8E3)', advance='no')
     &                 ivPTSC((i - 1) * iDofT + j)
               End Do
               Write (IO_IniOut, "('')")
            End Do
            call VecRestoreArrayF90(vvPTSC, ivPTSC, iError)
            call VecGhostRestoreLocalForm(DSols(iTS), vvPTSC, iError)
         End Do
         Write(IO_IniOut, "('')")
         Write(IO_IniOut, "('*Time   [ T , DT ]')")
         Write(IO_IniOut, *) TiniIF, DelTIF
         Close(IO_IniOut)
!
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        '...ready.'//char(10), iError)
!
         twP = noMPI_Wtime () - t0wP
         Write(Str,"('Time writing parallel data',A1,"//
     &        "'(Mesh...txt, Params...txt, IniFile...txt):',"//
     &        " F12.3, ' Secs')") char(10), twP
         Len = Len_Trim(Str)
         Call PetscPrintf(PETSC_COMM_WORLD, Str(1:Len)//char(10),iError)
!
      End If   ! Block I -b Writing files for parallel input
!
!
!     Block II:
!     Reading data in parallel (files already splitted)
!
!
      If (iRPI .gt. 0) Then
         t0lecP = noMPI_Wtime ()
!
!     Mesh
!
         Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &        'Reading from parallel input files...'//char(10), iError)
         Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &        'Mesh_xxxx.txt ...'//char(10), iError)
!
         Write(Str,"('Mesh_',I0.4,'.txt')") iRank
         IO_Mesh = 13
         iniError = 1
         Open(IO_Mesh, FILE=Str, STATUS='OLD', ERR=50)
         iniError = 0
 50      continue
         If (iniError .ne. 0) Then
            Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &           'Can not open file: '//Str//char(10), iError)
         End If
!
!     Continue reading while not error
!
         If (iniError .eq. 0) Then
            Str='*Global Mesh Sizes'
            iError = iFindStringInFile(Str, IO_Mesh)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 2
            Else
               Read(IO_Mesh, *) NodT, NelT
            End If
         End If
         If (iniError .eq. 0) Then
            Str='*Local Mesh Sizes'
            iError = iFindStringInFile(Str, IO_Mesh)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 3
            Else
               Read(IO_Mesh, *) NodL, NnlNodes, NelL
            End If
         End If
         If (iniError .eq. 0) Then
            Allocate(nlNodes(NnlNodes))
            Str='*Ghost nodes'
            iError = iFindStringInFile(Str, IO_Mesh)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 4
            Else
               Read(IO_Mesh, *) (nlNodes(I), I = 1, NnlNodes)
            End If
         End If
         If (iniError .eq. 0) Then
            Allocate(iaux(NodL))
            Str='*Nodal Permutation'
            iError = iFindStringInFile(Str, IO_Mesh)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 5
            Else
               Read(IO_Mesh, *) (iaux(I), I = 1, NodL)
            End If
         End If
         If (iniError .eq. 0) Then
            Allocate(giEls(NelL))
            Str='*Element Permutation'
            iError = iFindStringInFile(Str, IO_Mesh)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 6
            Else
               Read(IO_Mesh, *) (giEls(I), I = 1, NelL)
            End If
         End If
!
!     Collective error test
!
         call MPI_Allreduce (iniError, NError, 1, MPI_INTEGER,
     &        MPI_SUM, PETSC_COMM_WORLD, iError)
         If (NError .ne. 0) Then
            Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &           'Some processes report error reading files:'
     &           //char(10), iError)
            Write (Str, "('Rank: ',I0,', Error: ',I0)") iRank, iniError
            Len = Len_Trim(Str)
            Call PetscSynchronizedPrintf (PETSC_COMM_WORLD,
     &           Str(1:Len)//char(10),iError)
            Call PetscSynchronizedFlush (PETSC_COMM_WORLD, iError)
            Call PetscFinalize(iError)
!
         End If
!
!     Make processes know the ranges assigned to each one
!
         Call MPI_Allgather(NodL, 1, MPI_INTEGER,
     &        locNodes, 1, MPI_INTEGER, PETSC_COMM_WORLD, iError)
!
         iFirstNode(1) = 1
         Do i = 1, nProcs - 1
            iFirstNode(i+1) = iFirstNode(i) + locNodes(i)
         End Do
         nFirst = iFirstNode(iRank+1)
         nLast = nFirst + locNodes(iRank+1) - 1
!
!     Re-build Nodal permutation IS
!
         Call ISCreateGeneral(PETSC_COMM_WORLD, NodL, iaux,
     &        PETSC_COPY_VALUES, isPerm, iError)
         DeAllocate(iaux)
!
!     Re-build Element permutation IS (inverted)
!
         Do i = 1, NelL
            giEls(i) = giEls(i) - 1
         End Do
         Call ISCreateGeneral (PETSC_COMM_WORLD, NelL, giEls,
     &        PETSC_USE_POINTER, isPels, iError)
!
!     Create parallel vector for coordinates (with ghost values)
!
         Allocate (iaux(NnlNodes*nDim))
         Do i = 1, NnlNodes
            Do j = 1, nDim
               iaux((i-1)*nDim+j) = (nlNodes(i)-1)*nDim + j - 1
            End Do
         End Do
!
         Call VecCreateGhost (PETSC_COMM_WORLD,
     &        nDim*NodL, nDim*NodT, nDim*NnlNodes, iaux,
     &        DCoords, iError)
         DeAllocate(iaux)
!
!     Read Coordinates
!
         Str='*COORDINATES'
         iError = iFindStringInFile(Str, IO_Mesh)
         If (iError .ne. 0) then
            Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &           'Can not find '//Str//char(10), iError)
            iniError = 7
         Else
            Read (IO_Mesh, *) I
            If (I .ne. NodL+NnlNodes) Stop 'I .ne. NodL+NnlNodes'
            Call VecGhostGetLocalForm(DCoords, vvPTSC, iError);
            Call VecGetArrayF90(vvPTSC, ivPTSC, iError)
            Do i = 1, NodL + NnlNodes
               Read (IO_Mesh, *) (ivPTSC((i-1)*nDim+j), j = 1, nDim)
            End Do
            call VecRestoreArrayF90(vvPTSC, ivPTSC, iError)
            call VecGhostRestoreLocalForm(DCoords, vvPTSC, iError);
         End If
!
!     Read elements: nodes per element, build lIE
!
         Allocate(lIE(NelL+1))
!
         If (iniError .eq. 0) Then
            Str='*ELEMENT GROUPS'
            iError = iFindStringInFile(Str, IO_Mesh)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 8
            Else
               Read (IO_Mesh, *) i
               Read (IO_Mesh, *) i, j, Str
               If (j .ne. NelL) Stop "Inconsistent NelL"
               Read (IO_Mesh, *) (lIE(i), i = 1, NelL)
               Length_JE = IEarrange(lIE, NelL)
               Allocate (lJE(Length_JE))
            End If
         End If
!
!     Read elements: incidences, build lJE
!
         If (iniError .eq. 0) Then
            Str='*INCIDENCE'
            iError = iFindStringInFile(Str, IO_Mesh)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 9
            Else
               Read (IO_Mesh, *) (lJE(i), i = 1, Length_JE)
            End If
         End If
!
!     Read elements: type
!
         Allocate(lElType(NelL))
         If (iniError .eq. 0) Then
            Str='*ELEMENT TYPE'
            iError = iFindStringInFile(Str, IO_Mesh)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 10
            Else
               Read (IO_Mesh, *) (lElType(I), I = 1, NelL)
            End If
         End If
!
!     Read elements: material
!
         Allocate(lElMat(NelL))
         If (iniError .eq. 0) Then
            Str='*ELEMENT MAT'
            iError = iFindStringInFile(Str, IO_Mesh)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 11
            Else
               Read (IO_Mesh, *) (lElMat(I), I = 1, NelL)
            End If
         End If
!
!     Collective error test
!
         call MPI_Allreduce (iniError, NError, 1, MPI_INTEGER,
     &        MPI_SUM, PETSC_COMM_WORLD, iError)
         If (NError .ne. 0) Then
            Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &           'Some processes report error reading Mesh:'
     &           //char(10), iError)
            Write (Str, "('Rank: ',I0,', Error: ',I0)") iRank, iniError
            Len = Len_Trim(Str)
            Call PetscSynchronizedPrintf (PETSC_COMM_WORLD,
     &           Str(1:Len)//char(10),iError)
            Call PetscSynchronizedFlush (PETSC_COMM_WORLD, iError)
            Call PetscFinalize(iError)
!
         End If
!
!     And Dirichlet conditions...
!
         Allocate (viDir(NSubSteps))
         Allocate (vcDir(NSubSteps))
!
!     Create parallel vectors for Dirichlet indicators/conditions
!     (with ghost values)
!
         Allocate (iaux(NnlNodes*iDoFT))
         Do i = 1, NnlNodes
            Do j = 1, iDoFT
               iaux((i-1)*iDoFT+j) = (nlNodes(i)-1)*iDoFT + j - 1
            End Do
         End Do
!
         Call VecCreateGhost (PETSC_COMM_WORLD,
     &        iDoFT*NodL, iDoFT*NodT, iDoFT*NnlNodes, iaux,
     &        viDir(1), iError)
         DeAllocate(iaux)
!
         Do iSS = 1, NSubSteps
            If (iSS .gt. 1) Then
               Call VecDuplicate(viDir(1), viDir(iSS), iError)
            End If
            Call VecDuplicate(viDir(1), vcDir(iSS), iError)
         End Do
!
!     Read Dirichlet conditions: indicators and values
!
         Str='*DIRICHLET CONDITIONS'
         iError = iFindStringInFile(Str, IO_Mesh)
         If (iError .ne. 0) then
            Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &           'Can not find '//Str//char(10), iError)
            iniError = 12
         Else
            Do iSS = 1, NSubSteps
               Call VecGhostGetLocalForm(viDir(iSS), vvPTSC, iError)
               Call VecGetArrayF90(vvPTSC, ivPTSC, iError)
               Do i = 1, NodL + NnlNodes
                  Read (IO_Mesh, *)
     &                 (ivPTSC((i-1)*iDoFT+j), j = 1, iDoFT)
               End Do
               Call VecRestoreArrayF90(vvPTSC, ivPTSC, iError)
               Call VecGhostRestoreLocalForm(viDir(iSS), vvPTSC, iError)
            End Do
            Do iSS = 1, NSubSteps
               Call VecGhostGetLocalForm(vcDir(iSS), vvPTSC, iError)
               Call VecGetArrayF90(vvPTSC, ivPTSC, iError)
               Do i = 1, NodL + NnlNodes
                  Read (IO_Mesh, *)
     &                 (ivPTSC((i-1)*iDoFT+j), j = 1, iDoFT)
               End Do
               Call VecRestoreArrayF90(vvPTSC, ivPTSC, iError)
               Call VecGhostRestoreLocalForm(vcDir(iSS), vvPTSC, iError)
            End Do
         End If
!
!     Last collective error test
!
         call MPI_Allreduce (iniError, NError, 1, MPI_INTEGER,
     &        MPI_SUM, PETSC_COMM_WORLD, iError)
         If (NError .ne. 0) Then
            Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &           'Some processes report error reading Mesh:'
     &           //char(10), iError)
            Write (Str, "('Rank: ',I0,', Error: ',I0)") iRank, iniError
            Len = Len_Trim(Str)
            Call PetscSynchronizedPrintf (PETSC_COMM_WORLD,
     &           Str(1:Len)//char(10),iError)
            Call PetscSynchronizedFlush (PETSC_COMM_WORLD, iError)
            Call PetscFinalize(iError)
!
         End If
         Close (IO_Mesh)
!
!         Str='Reading mesh in parallel'
!         Call MemReport(iRank, nProcs, Str)
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        '...ready.'//char(10), iError)
!
      End If                    ! (iRPI .gt. 0) (Read in parallel mesh)
!
!     Read Param_xxxx.txt files
!
      If (iRPI .gt. 0) Then
         Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &        'Reading Param_xxxx.txt files...'//char(10), iError)
!
         Write(Str,"('Param_',I0.4,'.txt')") iRank
         IO_Par = 14
         iniError = 1
         Open(IO_Par, FILE=Str, STATUS='OLD', ERR=70)
         iniError = 0
 70      continue
         If (iniError .ne. 0) Then
            Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &           'Can not open file: '//Str//char(10), iError)
         End If
!
!     Continue reading while not error
!
         If (iniError .eq. 0) Then
            Str='*Global Mesh Sizes'
            iError = iFindStringInFile(Str, IO_Par)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 2
            Else
               Read(IO_Par, *) i, j
               If (i .ne. NodT .or. j .ne. NelT) Then
                  Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &                 'Global mesh data in Param_xxxx.txt'//
     &                 ' doesnt match Mesh_xxxx.txt data'//
     &                 char(10), iError)
                  iniError = 3
               End If
            End If
         End If
         If (iniError .eq. 0) Then
            Str='*Local Mesh Sizes'
            iError = iFindStringInFile(Str, IO_Par)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 4
            Else
               Read(IO_Par, *) i, j, k
               If (i.ne.NodL .or. j.ne.NnlNodes .or. k.ne.NelL) Then
                  Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &                 'Local mesh data in Param_xxxx.txt'//
     &                 ' doesnt match Mesh_xxxx.txt data'//
     &                 char(10), iError)
                  iniError = 5
               End If
            End If
         End If
!
         If (iniError .eq. 0) Then
            Str='*Global parameters'
            iError = iFindStringInFile(Str, IO_Par)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 6
            Else
               Read(IO_Par, *) MaxId_Dataset, Length_Param,Length_JParam
            End If
         End If
!
         If (iniError .eq. 0) Then
            Str='*Local parameters'
            iError = iFindStringInFile(Str, IO_Par)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 7
            Else
               Read(IO_Par, *) nGoPs, lenlJE, lenlJJE
            End If
         End If
!
!     Collective error test
!
         call MPI_Allreduce (iniError, NError, 1, MPI_INTEGER,
     &        MPI_SUM, PETSC_COMM_WORLD, iError)
         If (NError .ne. 0) Then
            Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &           'Some processes report error reading files:'
     &           //char(10), iError)
            Write (Str, "('Rank: ',I0,', Error: ',I0)") iRank, iniError
            Len = Len_Trim(Str)
            Call PetscSynchronizedPrintf (PETSC_COMM_WORLD,
     &           Str(1:Len)//char(10),iError)
            Call PetscSynchronizedFlush (PETSC_COMM_WORLD, iError)
            Call PetscFinalize(iError)
         End If
!
         Allocate(giGoP(nGoPs))
         Allocate(giPar(nGoPs))
         Allocate(giJPar(nGoPs))
         lenlIE = nGoPs + 1
         Allocate(lIE_Par(lenlIE))
         Allocate(lIE_JPar(lenlIE))
!
         If (iniError .eq. 0) Then
            Str='*Parameters mapping'
            iError = iFindStringInFile(Str, IO_Par)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 8
            Else
               Read(IO_Par, *) (giGoP(i), i = 1, nGoPs)
            End If
         End If
!
         If (iniError .eq. 0) Then
            Str='*Pointers to Real Parameters'
            iError = iFindStringInFile(Str, IO_Par)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 9
            Else
               Read(IO_Par, *) (giPar(i), i = 1, nGoPs)
            End If
         End If
!
         If (iniError .eq. 0) Then
            Str='*Pointers to Integer Parameters'
            iError = iFindStringInFile(Str, IO_Par)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 10
            Else
               Read(IO_Par, *) (giJPar(i), i = 1, nGoPs)
            End If
         End If
!
         If (iniError .eq. 0) Then
            Str='*Parameter Groups'
            iError = iFindStringInFile(Str, IO_Par)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 11
            Else
               Read(IO_Par, *) nGoPs
            End If
         End If
!
         Allocate(vParam(nOldTimeSteps+1))
         Allocate(vJParam(nOldTimeSteps+1))
!     Pre-alloc memory for vectors, so that they are sequentially stored
         Allocate(auxParam((nOldTimeSteps+1)*lenlJE))
         Allocate(auxJParam((nOldTimeSteps+1)*lenlJJE))
!         Call VecCreateSeq(PETSC_COMM_SELF, lenlJE, vParam(1), iError)
!         Call VecCreateSeq(PETSC_COMM_SELF, lenlJJE, vJParam(1), iError)
!         Do iTS = 1, nOldTimeSteps
!            Call VecDuplicate(vParam(1), vParam(iTS+1), iError)
!            Call VecDuplicate(vJParam(1), vJParam(iTS+1), iError)
!         End Do
         Do iTS = 1, nOldTimeSteps + 1
            Call VecCreateSeqWithArray(PETSC_COMM_SELF, lenlJE,
     &           auxParam(1+lenlJE*(iTS-1)), vParam(iTS), iError)
            Call VecCreateSeqWithArray(PETSC_COMM_SELF, lenlJJE,
     &           auxJParam(1+lenlJJE*(iTS-1)), vJParam(iTS), iError)
         End Do
!
         If (iniError .eq. 0) Then
            Str='*Real Parameters'
            iError = iFindStringInFile(Str, IO_Par)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 12
            Else
               Read(IO_Par, *) (lIE_Par(i), i = 1, nGoPs)
               i = IEarrange(lIE_Par, nGoPs)
               If (lenlJE .gt. 0) Then
                  Do iTS = 1, nOldTimeSteps
                     Call VecGetArrayF90(vParam(iTS), ivPTSC, iError)
                     Read(IO_Par, *) (ivPTSC(i), i = 1, lenlJE)
                     Call VecRestoreArrayF90(vParam(iTS), ivPTSC,iError)
                  End Do
               End If
            End If
         End If
!
!         Call VecCreateSeq(PETSC_COMM_SELF, lenlJE, vParam, iError)
!         Call VecCreateSeq(PETSC_COMM_SELF, lenlJJE, vJParam, iError)
         If (iniError .eq. 0) Then
            Str='*Integer Parameters'
            iError = iFindStringInFile(Str, IO_Par)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 12
            Else
               Read(IO_Par, *) (lIE_JPar(i), i = 1, nGoPs)
               i = IEarrange(lIE_JPar, nGoPs)
               If (lenlJJE .gt. 0) Then
                  Do iTS = 1,nOldTimeSteps
                     Call VecGetArrayF90(vJParam(iTS), ivPTSC, iError)
                     Read(IO_Par, *) (ivPTSC(i), i = 1, lenlJJE)
                     Call VecRestoreArrayF90(vJParam(iTS),ivPTSC,iError)
                  End Do
               End If
            End If
         End If
!
!     Collective error test
!
         call MPI_Allreduce (iniError, NError, 1, MPI_INTEGER,
     &        MPI_SUM, PETSC_COMM_WORLD, iError)
         If (NError .ne. 0) Then
            Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &           'Some processes report error reading files:'
     &           //char(10), iError)
            Write (Str, "('Rank: ',I0,', Error: ',I0)") iRank, iniError
            Len = Len_Trim(Str)
            Call PetscSynchronizedPrintf (PETSC_COMM_WORLD,
     &           Str(1:Len)//char(10),iError)
            Call PetscSynchronizedFlush (PETSC_COMM_WORLD, iError)
            Call PetscFinalize(iError)
         End If
!
         Close(IO_Par)
!
!         Str='Reading parameters in parallel'
!         Call MemReport(iRank, nProcs, Str)
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        '...ready.'//char(10), iError)
!
!     End reading in parallel Param_xxxx.txt files
!
      End If                    ! (iRPI .gt. 0) (Read in parallel Params)
!
!     Read Param_xxxx.txt files
!
!
!     Now IniFile_xxxx.txt
!
      If (iRPI .gt. 0) Then
         Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &        'IniFile_xxxx.txt ...'//char(10), iError)
!
         Write(Str,"('IniFile_',I0.4,'.txt')") iRank
         IO_Ini = 15
         iniError = 1
         Open(IO_Ini, FILE=Str, STATUS='OLD', ERR=90)
         iniError = 0
 90      continue
         If (iniError .ne. 0) Then
            Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &           'Can not open file: '//Str//char(10), iError)
         End If
!
!     Continue reading while not error
!
         If (iniError .eq. 0) Then
            Str='*Global Mesh Sizes'
            iError = iFindStringInFile(Str, IO_Ini)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 2
            Else
               Read(IO_Ini, *) i, j
               If (i .ne. NodT .or. j .ne. NelT) Then
                  Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &                 'Global mesh data in Param_xxxx.txt'//
     &                 ' doesnt match Mesh_xxxx.txt data'//
     &                 char(10), iError)
                  iniError = 3
               End If
            End If
         End If
         If (iniError .eq. 0) Then
            Str='*Local Mesh Sizes'
            iError = iFindStringInFile(Str, IO_Ini)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 4
            Else
               Read(IO_Ini, *) i, j, k
               If (i.ne.NodL .or. j.ne.NnlNodes .or. k.ne.NelL) Then
                  Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &                 'Local mesh data in IniFile_xxxx.txt'//
     &                 ' doesnt match Mesh_xxxx.txt data'//
     &                 char(10), iError)
                  iniError = 5
               End If
            End If
         End If
!
         If (iniError .eq. 0) Then
            Str='*Initial Conditions'
            iError = iFindStringInFile(Str, IO_Ini)
            If (iError .ne. 0) then
               Call PetscPrintf (PETSC_COMM_SELF, char(10)//
     &              'Can not find '//Str//char(10), iError)
               iniError = 6
            End If
         End If
!
!     Collective error test
!
         call MPI_Allreduce (iniError, NError, 1, MPI_INTEGER,
     &        MPI_SUM, PETSC_COMM_WORLD, iError)
         If (NError .ne. 0) Then
            Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &           'Some processes report error reading files:'
     &           //char(10), iError)
            Write (Str, "('Rank: ',I0,', Error: ',I0)") iRank, iniError
            Len = Len_Trim(Str)
            Call PetscSynchronizedPrintf (PETSC_COMM_WORLD,
     &           Str(1:Len)//char(10),iError)
            Call PetscSynchronizedFlush (PETSC_COMM_WORLD, iError)
            Call PetscFinalize(iError)
         End If
!
         Allocate(DSols(nOldTimeSteps))
!
         Do iTS = 1, nOldTimeSteps
            Call VecDuplicate(viDir(1), DSols(iTS), iError)
!
            Call VecGhostGetLocalForm(DSols(iTS), vvPTSC, iError)
            Call VecGetArrayF90(vvPTSC, ivPTSC, iError)
            Do i = 1, NodL + NnlNodes
               Read (IO_Ini, *)
     &              (ivPTSC((i-1)*iDoFT+j), j = 1, iDoFT)
            End Do
            Call VecRestoreArrayF90(vvPTSC, ivPTSC, iError)
            Call VecGhostRestoreLocalForm(DSols(iTS), vvPTSC, iError)
         End Do
!
         If (initialTime .ne. 0) Then
            Str='*Time'
            iError = iFindStringInFile (Str, IO_Ini)
            If (iError .ne. 0) Then
               Call PetscPrintf (PETSC_COMM_SELF,
     &              Str//' not found.'//char(10), iError)
               Call PetscPrintf (PETSC_COMM_SELF,
     &              ' Will use Initial Time from Basparam.txt'//
     &              char(10), iError)
               initialTime = 0
            Else
               Read (IO_Ini, *) Tini
            End If
         End If
         Close (IO_Ini)
!
!         Str='Reading IniFile in parallel'
!         Call MemReport(iRank, nProcs, Str)
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        '...ready.'//char(10), iError)
!
         tlecP = noMPI_Wtime () - t0lecP
         Write(Str,"('Time reading parallel data',A1,"//
     &        "'(Mesh...txt, Params...txt, IniFile...txt):',"//
     &        " F12.3, ' Secs')") char(10), tlecP
         Len = Len_Trim(Str)
         Call PetscPrintf(PETSC_COMM_WORLD, Str(1:Len)//char(10),iError)
!
         Str='After parallel read'
         Call MemReport(iRank, nProcs, Str)
!
      End If   ! Block II (Reading data from parallel files)
!
!
!     If writing sequential output (iWPO == 0)
!     and not reading sequential input (iRPI != 0)
!
!     Build scatter-gather contexts for gathering data 
!     before printing results
!     And create sequential vectors for output
!
      If (iRPI .ne. 0 .and. iWPO .eq. 0) Then
         t0bgds = noMPI_Wtime ()
!
         Call PetscPrintf (PETSC_COMM_WORLD, char(10)//
     &     'Building gathering data structures...'//char(10), iError)
!
!     Coordinates:
c$$$!     A couple of index sets
c$$$!
c$$$         Allocate (iaux(NodL*nDim))
c$$$         Call ISGetIndicesF90 (isPerm, iPTSC, iError)
c$$$         Do i = 1, NodL
c$$$            Do j = 1, nDim
c$$$               iaux((i-1)*nDim+j) = iPTSC(i)*nDim + j - 1
c$$$            End Do
c$$$         End Do
c$$$         Call ISRestoreIndicesF90(isPerm, iPTSC, iError)
c$$$         Call ISCreateGeneral(PETSC_COMM_WORLD, NodL*nDim, iaux,
c$$$     &        PETSC_USE_POINTER, isPar, iError)
c$$$         Call ISCreateStride(PETSC_COMM_WORLD,
c$$$     &        NodL*nDim, (nFirst-1)*nDim, 1, isSeq, iError)
c$$$!
c$$$!     Create sequential vector
c$$$!
c$$$         lenSCoo = 0
c$$$         If (iRank .eq. 0) lenSCoo = nDim*NodT
c$$$         Call VecCreateMPI (PETSC_COMM_WORLD, lenSCoo, nDim*NodT,
c$$$     &        SCoords, iError)
c$$$!
c$$$!     Create scatter-gather context for distributing coordinates
c$$$!
c$$$         Call VecScatterCreate (SCoords, isSeq, DCoords, isPar,
c$$$     &        sgcCoords, iError)
c$$$         Call ISDestroy(isPar, iError)
c$$$         DeAllocate(iaux)
c$$$         Call ISDestroy(isSeq, iError)
!
!     Create data structures for gathering parameters
!
!     First: Rebuild IE_Par, IE_JPar in master
!
!     vvPTSC will hold the number of parameters in each GoP
         Call VecCreateSeq(PETSC_COMM_SELF, nGoPs, vvPTSC, iError)
!     Load with the sizes, take from IE_..., also load iaux for index
         Allocate(iaux(nGoPs))
         Allocate(jaux(nGoPs))
         Call VecGetArrayF90(vvPTSC, ivPTSC, iError)
         nn = 0
         Do i = 1, nGoPs
            If (giGop(i) .gt. 0) Then
               nn = nn + 1
               ivPTSC(nn) = lIE_Par(i+1) - lIE_Par(i) ! Send This number
               jaux(nn) = i - 1 ! From this position in worker
               iaux(nn) = giGop(i) - 1 ! To this position in master
            End If
         End Do
         Call VecRestoreArrayF90(vvPTSC, ivPTSC, iError)
         Call ISCreateGeneral(PETSC_COMM_SELF, nn, iaux,
     &        PETSC_USE_POINTER, isSeq, iError)
         Call ISCreateGeneral(PETSC_COMM_SELF, nn, jaux,
     &        PETSC_USE_POINTER, isPar, iError)
!     vv2PTSC will be the same, but on master
         nn = MaxId_DataSet
         If (iRank .ne. 0) nn = 0
         Call VecCreateMPI (PETSC_COMM_WORLD, nn, PETSC_DETERMINE,
     &        vv2PTSC, iError)
         Call VecScatterCreate(vvPTSC, isPar, vv2PTSC, isSeq,
     &        sgcAux, iError)
         Call ISDestroy(isPar, iError)
         Call ISDestroy(isSeq, iError)
         DeAllocate(iaux)
         DeAllocate(jaux)
         Call VecScatterBegin(sgcAux, vvPTSC, vv2PTSC,
     &        INSERT_VALUES, SCATTER_FORWARD, iError)
         Call VecScatterEnd(sgcAux, vvPTSC, vv2PTSC,
     &        INSERT_VALUES, SCATTER_FORWARD, iError)
!
         If (iRank .eq. 0) Then
            Allocate(IE_Par(MaxId_DataSet+1))
            Call VecGetArrayF90(vv2PTSC, ivPTSC, iError)
            Do i = 1, MaxId_DataSet
               IE_Par(i) = ivPTSC(i)
            End Do
            Call VecRestoreArrayF90(vv2PTSC, ivPTSC, iError)
            i = IEArrange(IE_Par, MaxId_DataSet)
         End If
!
!     Same for IE_JPar, use available structures
!
         Call VecGetArrayF90(vvPTSC, ivPTSC, iError)
         nn = 0
         Do i = 1, nGoPs
            If (giGop(i) .gt. 0) Then
               nn = nn + 1
               ivPTSC(nn) = lIE_JPar(i+1) - lIE_JPar(i) ! Send This number
            End If
         End Do
         Call VecRestoreArrayF90(vvPTSC, ivPTSC, iError)
         Call VecScatterBegin(sgcAux, vvPTSC, vv2PTSC,
     &        INSERT_VALUES, SCATTER_FORWARD, iError)
         Call VecScatterEnd(sgcAux, vvPTSC, vv2PTSC,
     &        INSERT_VALUES, SCATTER_FORWARD, iError)
!
         If (iRank .eq. 0) Then
            Allocate(IE_JPar(MaxId_DataSet+1))
            Call VecGetArrayF90(vv2PTSC, ivPTSC, iError)
            Do i = 1, MaxId_DataSet
               IE_JPar(i) = ivPTSC(i)
            End Do
            Call VecRestoreArrayF90(vv2PTSC, ivPTSC, iError)
            i = IEArrange(IE_JPar, MaxId_DataSet)
         End If
         Call VecScatterDestroy(sgcAux, iError)
         Call VecDestroy(vvPTSC, iError)
         Call VecDestroy(vv2PTSC, iError)
!
!     Build index sets to distribute REAL parameters
!     (nGoPs: Number of Groups of Parameters stored locally)
!
         Allocate(iaux(lenlJE))
         Allocate(jaux(lenlJE))
         isizP = 0              ! Index size for distributing Param
         Do i = 1, nGoPs
            If (giGoP(i) .gt. 0) Then
               Do k = lIE_Par(i), lIE_Par(i+1) - 1
                  isizP = isizP + 1
                  iaux(isizP) = k - 1 ! Index over local Param
                  jaux(isizP) = ! Index over global Param
     &                 giPar(i) + k - lIE_Par(i) - 1
               End Do
            End If
         End Do
!
         Call ISCreateGeneral(PETSC_COMM_SELF, isizP, jaux,
     &        PETSC_USE_POINTER, isSeq, iError)
         Call ISCreateGeneral(PETSC_COMM_SELF, isizP, iaux,
     &        PETSC_USE_POINTER, isPar, iError)
!
         nn = 0
         If (iRank .eq. 0) nn = IE_Par(MaxId_DataSet + 1) - 1
!
         Call VecCreateMPI(PETSC_COMM_WORLD, nn, PETSC_DETERMINE,
     &        vSParam, iError)
!
!     Create S-G context
!
         Call VecScatterCreate (vSParam, isSeq, vParam(1), isPar,
     &        sgcParam, iError)
         call ISDestroy(isSeq, iError)
         call ISDestroy(isPar, iError)
         DeAllocate(iaux)
         DeAllocate(jaux)
!
!     Build index sets to distribute INTEGER parameters
!     (nGoPs: Number of Groups of Parameters stored locally)
!
         Allocate(iaux(lenlJJE))
         Allocate(jaux(lenlJJE))
         isizP = 0              ! Index size for distributing Param
         Do i = 1, nGoPs
            If (giGoP(i) .gt. 0) Then
               Do k = lIE_JPar(i), lIE_JPar(i+1) - 1
                  isizP = isizP + 1
                  iaux(isizP) = k - 1 ! Index over local Param
                  jaux(isizP) = ! Index over global Param
     &                 giJPar(i) + k - lIE_JPar(i) - 1
               End Do
            End If
         End Do
!
         Call ISCreateGeneral(PETSC_COMM_SELF, isizP, jaux,
     &        PETSC_USE_POINTER, isSeq, iError)
         Call ISCreateGeneral(PETSC_COMM_SELF, isizP, iaux,
     &        PETSC_USE_POINTER, isPar, iError)
!
         nn = 0
         If (iRank .eq. 0) nn = IE_JPar(MaxId_DataSet + 1) - 1
!
         Call VecCreateMPI(PETSC_COMM_WORLD, nn, PETSC_DETERMINE,
     &        vSJParam, iError)
!
!     Create S-G context
!
         Call VecScatterCreate (vSJParam, isSeq, vJParam(1), isPar,
     &        sgcJParam, iError)
         call ISDestroy(isSeq, iError)
         call ISDestroy(isPar, iError)
         DeAllocate(iaux)
         DeAllocate(jaux)
!
!     Rebuild S-G context for Unknowns
!
!     A couple of index sets
!
         Allocate (iaux(NodL*iDoFT))
         Call ISGetIndicesF90 (isPerm, iPTSC, iError)
         Do i = 1, NodL
            Do j = 1, iDoFT
               iaux((i-1)*iDoFT+j) = iPTSC(i)*iDoFT + j - 1
            End Do
         End Do
         Call ISRestoreIndicesF90(isPerm, iPTSC, iError)
         Call ISCreateGeneral(PETSC_COMM_WORLD, NodL*iDoFT, iaux,
     &        PETSC_USE_POINTER, isPar, iError)
!
         Call ISCreateStride(PETSC_COMM_WORLD,
     &        NodL*iDoFT, (nFirst-1)*iDoFT, 1, isSeq, iError)
!
!     Create sequential vector
!
         nn = 0
         If (iRank .eq. 0) nn = iDoFT*NodT
         Call VecCreateMPI (PETSC_COMM_WORLD, nn, iDoFT*NodT,
     &        SSol, iError)
!
!     Create scatter-gather context for distributing unknowns
!
         Call VecScatterCreate (SSol, isSeq, DSols(1), isPar,
     &        sgcVUnk, iError)
         Call ISDestroy(isPar, iError)
         DeAllocate(iaux)
         Call ISDestroy(isSeq, iError)
!
         Str='Sequential vectors built'
         Call MemReport(iRank, nProcs, Str)
         Call PetscPrintf (PETSC_COMM_WORLD,
     &        '...ready.'//char(10), iError)
!
         tbgds = noMPI_Wtime () - t0bgds
         Write(Str,"(A1,'Time building gathering data structures:',"//
     &        " F12.3, ' Secs')") char(10), tbgds
         Len = Len_Trim(Str)
         Call PetscPrintf(PETSC_COMM_WORLD, Str(1:Len)//char(10), i)
!
      End If                    ! (iRPI .ne. 0 .and. iWPO .eq. 0)
!     Build scatter-gather contexts for gathering data 
!     before printing results when reading parallel and writing sequential
!
!     At this stage we got all needed data in memory
!
!     Deal here with the particular case in which we want output only
!     in a few nodes (we need the distributed vector of solutions DSols(I))
!
      If (NodOUT .gt. 0) Then
!     Create vector for writing in file (only components in root)
         nn = 0
         If (iRank .eq. 0) nn = NodOUT*iDoFT
         Call VecCreateMPI (PETSC_COMM_WORLD, nn, PETSC_DETERMINE,
     &        SOUT, iError)
         Call ISCreateStride(PETSC_COMM_WORLD, nn,
     &        0, 1, isSeq, iError)
         If (iRank .eq. 0) Then
            Allocate(iaux(nn))
            Do i = 1, NodOUT
               Do j = 1, iDoFT
                  iaux((i-1)*iDoFT+j) = (NodesOut(i)-1)*iDoFT + j - 1
               End Do
            End Do
         End If
         Call ISCreateGeneral(PETSC_COMM_WORLD, nn, iaux,
     &        PETSC_USE_POINTER, isPar, iError)
         Call VecScatterCreate(DSols(1), isPar, SOUT, isSeq, sgcNOUT, i)
         Call ISDestroy(isSeq, iError)
         Call ISDestroy(isPar, iError)
         If (iRank .eq. 0) DeAllocate(iaux)
      End If
!
!     Proceed with computations
!
      If (iCom .ne. 0) Then
         t0comp = noMPI_Wtime ()
!
         Call GenericFP (NodT, NelT, NDim, iDofT, nOldTimeSteps,
     &        NonLinearProblem, MaxIterG, SRPG, nkt, NStepsRenum,
     &        Sym, Iterative, MaxIter, SRP, iSolverType,
     &        ITMAXSOL, EPS, Krylov, LFIL, TolPre,
     &        iElementLib, iCoupling,
     &        iDofNamesG, rNormG, TolG, iXupdateG,
     &        iDofNames, rNorm, Tol, CommonPar, iXupdate,
     &        DelT, Tini, Tmax, nStepOut, NodOUT, SOUT, sgcNOUT,
     &        MaxNodEl, MaxElemLib, MaxLCommonPar,
     &        iWPO, NodL, NnlNodes, NelL, nlNodes, isPerm,
     &        DCoords, SCoords, sgcCoords, lIE, lJE, lElType, lElMat,
     &        lIE_Par, lIE_JPar, vParam, vJParam,
     &        IE_Par, IE_JPar, vSParam, vSJParam, sgcParam, sgcJParam,
     &        giGop, giPar, giJPar,
     &        viDir, vcDir, DSols, SSol, sgcVUnk, Lch, NSubSteps,
     &        nGoPs, iWDataSet, MaxId_DataSet)
!
         tcomp = noMPI_Wtime () - t0comp
         Write(Str,"('Time solving:', F12.3, ' Secs')") tcomp
         Len = Len_Trim(Str)
         Call PetscPrintf(PETSC_COMM_WORLD, Str(1:Len)//char(10),iError)
!
      End If                    ! iCom .ne 0 (Actually solve)
!
!?      DeAllocate (locNodes)
!?      DeAllocate (iFirstNode)
!
!     Free Mesh data: Coordinates:
!
      call VecDestroy(DCoords, iError)
!
c$$$      If (iWPO .eq. 0) Then     ! Writing sequential, these were used
c$$$         Call VecDestroy (SCoords, iError)
c$$$         Call VecScatterDestroy (sgcCoords, iError)
c$$$      End If
!     Local elemental information:
      DeAllocate (lIE)          ! Allocated inside "BuildlIEJEdef"
      DeAllocate (lJE)          ! Allocated inside "BuildlIEJEdef"
      DeAllocate (lElType)
      DeAllocate (lElMat)
!     Dirichlet conditions:
      Do iSS = 1, NSubSteps
         Call VecDestroy(viDir(iSS), iError)
         Call VecDestroy(vcDir(iSS), iError)
      End Do
!
!     Free parameters data: 
!
      DeAllocate (lIE_Par)
      DeAllocate (lIE_JPar)
      Do iTS = 1, nOldTimeSteps+1
         Call VecDestroy(vParam(iTS), iError)
         Call VecDestroy(vJParam(iTS), iError)
      End Do
      DeAllocate(auxParam)
      DeAllocate(auxJParam)
!
!     These probably weren't used for a long time...
      DeAllocate (giGoP)
      DeAllocate (giPar)
      DeAllocate (giJPar)
!
      If (iWPO .eq. 0) Then     ! Writing sequential, these were used
         Call VecDestroy(vSParam, iError)
         Call VecDestroy(vSJParam, iError)
         Call VecScatterDestroy (sgcParam, iError)
         Call VecScatterDestroy (sgcJParam, iError)
      End If
!
!     Free solution data:
!
      Do iTS = 1, nOldTimeSteps
         Call VecDestroy(DSols(iTS), iError)
      End Do
      If (iWPO .eq. 0) Then     ! Writing sequential, these were used
         Call VecDestroy(SSol, iError)
         Call VecScatterDestroy (sgcVUnk, iError)
      End If
!
!     This probably wasn't used for a long time...
      call ISDestroy(isPerm, iError)
!
      Str='Exiting'
      Call MemReport(iRank, nProcs, Str)
!
      ttime = noMPI_Wtime () - time00
      Write(Str,"('Total program time:',"//
     &     " F12.3, ' Secs')") ttime
      Len = Len_Trim(Str)
      Call PetscPrintf(PETSC_COMM_WORLD, Str(1:Len)//char(10), iError)
!
      Call PetscFinalize(iError)
!
      End Program
