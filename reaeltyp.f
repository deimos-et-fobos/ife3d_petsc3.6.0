      SUBROUTINE REAELTYP (LU, NGR, NEL, ELTYPE)
      CHARACTER *80 ELTYPE, REC
!     write (*,*) 'entro en reaeltype'
      READ (LU, '(A80)') REC
!        write (*,*) 'record:'
!        write (*,*) 'record:'
!     write (*,'(a70)') rec(1:70)
      OPEN  (3, FILE='SMORED.AUX',STATUS='UNKNOWN')
      WRITE (3,'(A80)') REC
      CLOSE (3)
      OPEN (3, FILE='SMORED.AUX',STATUS='OLD')
      READ (3, *) IGR, NEL
      CLOSE(3, STATUS='DELETE')
C
!     write (*,*) 'antes de backspace. lu = ', lu
!     write (*,*) 'antes de backspace. lu = ', lu
!      BACKSPACE(LU)
!     write (*,*) 'despues de backspace'
!     write (*,*) 'despues de backspace'
!      READ (LU,*) IGR, NEL
!     write (*,*) 'grupo:', igr, '# elementos:', nel
      IF (IGR.NE.NGR) THEN
         STOP 'Unexpected Group number'
      END IF
      I = 80
      DO WHILE (REC(I:I) .EQ. ' ')
         I = I - 1
      END DO
      ILAST = I
      DO WHILE (REC(I:I) .NE. ' ')
         I = I - 1
      END DO
      IFIRST = I+1
!     write (*,*) 'IFirst, ILast:', ifirst, ilast
      LENELTYPE = ILAST - IFIRST + 1
      ELTYPE(1:LENELTYPE) = REC(IFIRST:ILAST)
      RETURN
      END
