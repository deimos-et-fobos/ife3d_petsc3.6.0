!     ------------------------------------------------------------------
!
!     This routine builds the initial local mesh
!     (according to the first naive partition), the elements in each
!     process are those with *any* node in the process
!
!     Parameters:
!     gIE, gJE: (input)  ! Global conectivity (only master)
!     NodT: (input)      ! Total nodes in the mesh (all)
!     NelT: (input)      ! Total elments in the mesh (only master)
!     lIE, lJE: (output) ! Local conectivity (all)
!     NelL: (output)     ! Local elements in the mesh (all)
!     iRank, nProcs: (inp) process rank, number of processes
!     iFirstNode: (input)! First node in each process (all)
!     locNodes: (input)  ! # of nodes in each process (all)
!
!     ------------------------------------------------------------------
      Subroutine BuildlIEJEpre (gIE, gJE, NodT, NelT, lIE, lJE, NelL,
     &     iRank, nProcs, iFirstNode, locNodes)
!     ------------------------------------------------------------------
!
#include "finclude/petscsys.h"
!     #include "finclude/petscis.h"
!     #include "finclude/petscis.h90"
!
      INTEGER gIE(*), gJE(*)    ! Global conectivity (only master)
      Dimension iFirstNode(nProcs) ! First node in each process (only master)
      Dimension locNodes(*)     ! # of nodes in each process (only master)
      INTEGER, Pointer :: lIE(:) ! Parte local de IE (puntero, NelL+1)
      INTEGER, Pointer :: lJE(:) ! Parte local de JE (nodos de c/elemento)
      integer status(MPI_STATUS_SIZE)
!
!     Distribute conectivity
!
      If (iRank .eq. 0) then
!
!     Build "local" conectivity for each process
!     (starting with last, ending with '0' or master)
!
         Do iother = nProcs-1, 0, -1
            nFirst = iFirstNode(iother+1)
            nLast = nFirst + locNodes(iother+1) - 1
!
!     First sweep: count length of "local IE" and "local JE"
!
            lenlIE = 1
            lenlJE = 0
!     Sweeps all elements
            Do iel = 1, NelT
               iea = gIE(iel)
               ieb = gIE(iel + 1) - 1 
               isw = 0
!
!     Any node belonging to this process causes the element to be included
!     Sweeps nodes of this element
!
               Do j = iea, ieb
                  node = gJE(j)
!     If any node is "local" to this process, mark
                  If (node .ge. nFirst .and. node .le. nLast) Then
                     isw = 1
                     Exit
                  End If
               End Do
!     If marked, count
               If (isw .ne. 0) Then
                  lenlIE = lenlIE + 1
                  lenlJE = lenlJE + ieb - iea + 1
               End If
            End Do
!
!     Sized computed, alloc memory and store results
!
            Allocate (lIE(lenlIE))
            Allocate (lJE(lenlJE))
!
!     Now, repeat, but storing computed values
!
            lenlIE = 1
            lenlJE = 0
            lIE(lenlIE) = lenlJE + 1
!     Sweeps all elements
            Do iel = 1, NelT
               iea = gIE(iel)
               ieb = gIE(iel + 1) -1
               isw = 0
!
!     Any node belonging to this process causes the element to be included
!     Sweeps nodes of this element
!
               Do j = iea, ieb
                  node = gJE(j)
                  If (node .ge. nFirst .and. node .le. nLast) Then
                     isw = 1
                     Exit
                  End if
               End Do
!
!     If marked, store
!
               If (isw .ne. 0) Then
                  Do j = iea, ieb
                     node = gJE(j)
                     lenlJE = lenlJE + 1
                     lJE(lenlJE) = node
                  End Do
                  lenlIE = lenlIE + 1
                  lIE(lenlIE) = lenlJE + 1
               End If
            End Do
!
!     "local" mesh conectivity built, send (if it's not the master)
!
            If (iother .gt. 0) Then
               Call MPI_Send (lenlIE, 1, MPI_INTEGER,
     &              iother, 1, MPI_COMM_WORLD, iError)
               Call MPI_Send (lenlJE, 1, MPI_INTEGER,
     &              iother, 2, MPI_COMM_WORLD, iError)
               Call MPI_Send (lIE, lenlIE, MPI_INTEGER,
     &              iother, 3, MPI_COMM_WORLD, iError)
               Call MPI_Send (lJE, lenlJE, MPI_INTEGER,
     &              iother, 4, MPI_COMM_WORLD, iError)
               DeAllocate(lIE)
               DeAllocate(lJE)
            End If
         End Do
!
      Else   ! Not master process
!
!     Receive from master "local" conectivity
!
         Call MPI_Recv (lenlIE, 1, MPI_INTEGER,
     &        0, 1, MPI_COMM_WORLD, status, iError)
         Call MPI_Recv (lenlJE, 1, MPI_INTEGER,
     &        0, 2, MPI_COMM_WORLD, status, iError)
         Allocate(lIE(lenlIE))
         Allocate(lJE(lenlJE))
         Call MPI_Recv (lIE, lenlIE, MPI_INTEGER,
     &        0, 3, MPI_COMM_WORLD, status, iError)
         Call MPI_Recv (lJE, lenlJE, MPI_INTEGER,
     &        0, 4, MPI_COMM_WORLD, status, iError)
!
      End If                    ! Process 0
!
      NelL = lenlIE - 1
!
      End
!     ------------------------------------------------------------------
!
!     This routine builds the final local mesh
!     (according to the optimized partition),
!     the elements are assigned to each process according to 
!     *the first node* in its connectivity !!!
!
!     Parameters:
!     gIE, gJE: (input)  ! Global conectivity (only master)
!     NodT: (input)      ! Total nodes in the mesh (all)
!     NelT: (input)      ! Total elments in the mesh (only master)
!     lIE, lJE: (output) ! Local conectivity (all)
!     NelL: (output)     ! Local elements in the mesh (all)
!     giEls: (output)    ! global index for local elements (all)
!     nGhosts: (output)  ! Number of "Ghost nodes": non-local nodes
!     nlNodes: (output)  ! List of non-local nodes
!     iRank, nProcs: (inp) process rank, number of processes
!     isPerm: (input) Petsc Index Set with the permutation
!     iFirstNode: (input)! First node in each process (all)
!     locNodes: (input)  ! # of nodes in each process (all)
!
!     ------------------------------------------------------------------
      Subroutine BuildlIEJEdef (gIE, gJE, NodT, NelT,
     &     lIE, lJE, NelL, giEls, nGhosts, nlNodes,
     &     iRank, nProcs, isPerm, isPart, iFirstNode, locNodes,
     &     NodOUT, NodesOUT)
!     ------------------------------------------------------------------
!
!      Use MemAndTimes
!
#include "finclude/petscsys.h"
#include "finclude/petscis.h"
#include "finclude/petscis.h90"
!
      INTEGER gIE(*), gJE(*) ! Global conectivity (only master)
      Dimension iFirstNode(nProcs) ! First node in each process (only master)
      Dimension locNodes(*)  ! # of nodes in each process (only master)
      Dimension NodesOUT(*)  ! Nodes in which solution is wanted
      Allocatable iGPerm(:)  ! Global permutation (only master)
      Allocatable msw(:)     ! Multiple switch (for non-local nodes, master)
!     El fortran de INTEL se queja si no se declaran enteros los
!     "dummy arguments allocatables"
      INTEGER, Pointer :: lIE(:)     ! Local IE (pointer, NelL+1)
      INTEGER, Pointer :: lJE(:)     ! Local JE (nodes for each element)
      INTEGER, Pointer :: giEls(:)   ! global index for local elements
      INTEGER, Pointer :: nlNodes(:) ! List of non-local nodes foreach proc
      INTEGER, Allocatable :: auxPerm(:) ! For re-creating permutation
      IS isPerm, isPart         ! Permutation index set
      PetscInt, pointer :: iPerm(:) ! Permutation array
      INTEGER status(MPI_STATUS_SIZE)
!
!     Gather permutation vector in master
!     use original values for locNodes, iFirstNode
!
      If (iRank .eq. 0) Then
         Allocate (iGPerm(NodT))
         Allocate (msw(NodT))
         iFirstNode = iFirstNode - 1 ! Convert to "C" style indexing
      End if
!
      call ISGetIndicesF90 (isPerm, iPerm, iError)
      NodL = locNodes(iRank+1)
!      write (*,*) iperm(1), nodl, igperm(1), locnodes(1), ifirstnode(1)
      call MPI_Gatherv(iPerm, NodL, MPI_INTEGER,
     &     iGPerm, locNodes, iFirstNode, MPI_INTEGER,
     &     0 , MPI_COMM_WORLD, iError)
      call ISRestoreIndicesF90 (isPerm, iPerm, iError)
      If (iRank .eq. 0) then
         iFirstNode = iFirstNode + 1 ! Re-convert to FORTRAN style indexing
      End if
!
!     Here we can renumber NodesOUT (iGPerm is in "C" style indexing)
!
      If (iRank .eq. 0) Then
         Do i = 1, NodOUT
            NodesOUT(i) = iGPerm(NodesOUT(i)) + 1
         End Do
      End If
!
!     Now we can recompute locNodes, iFirstNode, for the NEW partition
!
      Call ISPartitioningCount(isPart, nProcs, locNodes, iError)
      NodL = locNodes(iRank + 1)
      iFirstNode(1) = 1
      Do i = 1, nProcs - 1
         iFirstNode(i+1) = iFirstNode(i) + locNodes(i)
      End Do
!
!
!     Re-distributes perm according to new nodes assignment
!
      Call ISDestroy(isPerm, iError)
      Allocate(auxPerm(NodL))
!
      If (iRank .eq. 0) Then
         iFirstNode = iFirstNode - 1 ! Convert to "C" style indexing
      End if
      call MPI_Scatterv(iGPerm, locNodes, iFirstNode, MPI_INTEGER,
     &     auxPerm, NodL, MPI_INTEGER,
     &     0 , MPI_COMM_WORLD, iError)
      If (iRank .eq. 0) then
         iFirstNode = iFirstNode + 1 ! Re-convert to FORTRAN style indexing
      End if
!
      Call ISCreateGeneral(PETSC_COMM_WORLD, NodL, auxPerm,
     &     PETSC_COPY_VALUES, isPerm, iError)
      DeAllocate(auxPerm)
!
!     Master distributes data
!
      If (iRank .eq. 0) then
!
!     Build "local" conectivity for each process
!     (starting with last, ending with '0' or master)
!
!     Vector msw contains the numeration of the "external nodes"
!     (FORTRAN style indexing)
!
         msw = 0
         Do iother = nProcs-1, 0, -1
            nFirst = iFirstNode(iother+1)
            nLocN = locNodes(iother+1)
            nLast = nFirst + nLocN - 1
            nGhosts = 0
!
!     First sweep: count length of "local IE" and "local JE"
!
            lenlIE = 1
            lenlJE = 0
!     Sweeps all elements
            Do iel = 1, NelT
               iea = gIE(iel)
               ieb = gIE(iel + 1) - 1 
!
!     Only check for the first node,
!     In this way, the assignment element -> process is unique
!
               node = gJE(iea)
               node = iGPerm(node) + 1
               If (node .ge. nFirst .and. node .le. nLast) Then
                  lenlIE = lenlIE + 1
                  lenlJE = lenlJE + ieb - iea + 1
!
!     Check for non-local nodes in this (iother) process,
!     start with second node, (first is always local)
!
                  Do j = iea + 1, ieb
                     node = gJE(j)
                     node = iGPerm(node) + 1
                     If (node .lt. nFirst .or. node .gt. nLast) Then
!     non-local node
                        If (msw(node) .eq. 0) Then
!     Not counted, count and store the numeration
                           nGhosts = nGhosts + 1
                           msw(node) = nGhosts
                        End If   ! New non-local node
                     End If   ! Non-local node
                  End Do   ! nodes of the element (starting with 2nd)
               End If   ! Element assigned to this process
            End Do   ! Over full element list
!
!     Sized computed, alloc memory and store results
!
            Allocate (lIE(lenlIE))
            Allocate (lJE(lenlJE))
            Allocate (giEls(lenlIE-1))
            Allocate (nlNodes(nGhosts))
!
!     Use nGhosts as index for filling nlNodes vector
!
            nGhosts = 0
!
!     Now, repeat, but storing computed values
!
            lenlIE = 1
            lenlJE = 0
            lIE(lenlIE) = lenlJE + 1
!     Sweeps all elementos
            do iel = 1, NelT
               iea = gIE(iel)
               ieb = gIE(iel + 1) -1
!
!     Only check for the first node,
!     In this way, the assignment element -> process is unique
!
               node = gJE(iea)
               node = iGPerm(node) + 1
               If (node .ge. nFirst .and. node .le. nLast) Then
!
!     This element is assigned to this process
!
                  giEls(lenlIE) = iel
!
!     Renumber (and store also non-local nodes)
!
                  Do j = iea, ieb
                     node = gJE(j)
                     node = iGPerm(node) + 1
                     If (node .ge. nFirst .and. node .le. nLast) Then
!     Local nodes: convert to local numeration, FORTRAN style
                        node = node - nFirst + 1
                     Else
!     Non local nodes, get the new number
                        nnode = msw(node)
                        If (nnode .gt. nGhosts) Then
!     Not stored, count, store, mark
                           nGhosts = nGhosts + 1
                           nlNodes(nGhosts) = node
                        End If   ! New non-local node
!     FORTRAN style index, after the local nodes
                        node = nnode + nLocN
                     End If   ! Non-local node
                     lenlJE = lenlJE + 1
                     lJE(lenlJE) = node
                  End do   ! nodes of the element (all)
                  lenlIE = lenlIE + 1
                  lIE(lenlIE) = lenlJE + 1
               End If   ! Element assigned to this process
            End Do   ! Over full element list
!
!     Vector msw must be zeroed, this only sweeps the non-zero elements
!     ( msw = 0 ) may be shorter, but sweeps the entire vector !
!
            Do i = 1, nGhosts
               msw(nlNodes(i)) = 0
            End Do
!
!     "local" mesh conectivity built, send (if it's not this proccess)
!
            NelL = lenlIE - 1
            If (iother .gt. 0) Then
               Call MPI_Send (NelL, 1, MPI_INTEGER,
     &              iother, 1, MPI_COMM_WORLD, iError)
               Call MPI_Send (lenlJE, 1, MPI_INTEGER,
     &              iother, 2, MPI_COMM_WORLD, iError)
               Call MPI_Send (lIE, lenlIE, MPI_INTEGER,
     &              iother, 3, MPI_COMM_WORLD, iError)
               Call MPI_Send (lJE, lenlJE, MPI_INTEGER,
     &              iother, 4, MPI_COMM_WORLD, iError)
               Call MPI_Send (giEls, NelL, MPI_INTEGER,
     &              iother, 5, MPI_COMM_WORLD, iError)
               DeAllocate(lIE)
               DeAllocate(lJE)
               DeAllocate(giEls)
!
!     Send also non-local nodes
!
               Call MPI_Send (nGhosts, 1, MPI_INTEGER,
     &              iother, 6, MPI_COMM_WORLD, iError)
               Call MPI_Send (nlNodes, nGhosts, MPI_INTEGER,
     &              iother, 7, MPI_COMM_WORLD, iError)
               DeAllocate(nlNodes)
            End If
         End Do
!
         DeAllocate (iGPerm)
         DeAllocate (msw)
      Else
!
!     Receive from master "local" conectivity
!
         Call MPI_Recv (NelL, 1, MPI_INTEGER,
     &        0, 1, MPI_COMM_WORLD, status, iError)
         lenlIE = NelL + 1
         Call MPI_Recv (lenlJE, 1, MPI_INTEGER,
     &        0, 2, MPI_COMM_WORLD, status, iError)
         Allocate(lIE(lenlIE))
         Allocate(lJE(lenlJE))
         Allocate(giEls(NelL))
         Call MPI_Recv (lIE, lenlIE, MPI_INTEGER,
     &        0, 3, MPI_COMM_WORLD, status, iError)
         Call MPI_Recv (lJE, lenlJE, MPI_INTEGER,
     &        0, 4, MPI_COMM_WORLD, status, iError)
         Call MPI_Recv (giEls, NelL, MPI_INTEGER,
     &        0, 5, MPI_COMM_WORLD, status, iError)
!
!     Receive also non-local nodes
!
         Call MPI_Recv (nGhosts, 1, MPI_INTEGER,
     &        0, 6, MPI_COMM_WORLD, status, iError)
         Allocate(nlNodes(nGhosts))
         Call MPI_Recv (nlNodes, nGhosts, MPI_INTEGER,
     &        0, 7, MPI_COMM_WORLD, status, iError)
!
      End If                    ! Master Process / Others
!
      End
