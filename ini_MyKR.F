!***********************************************************************
!*	INICIALIZACION DE LA MATRIZ KR Y M
!***********************************************************************

      subroutine INICIALIZACION(KR,M,NNP,CNUMP,NGP,NELP,MMS,NELS,NGS,
     &            NNF,NGF,NELF,NCACOF,ICACO,NUMITAS,ILCIFS,IELS,DIRS,
     &            NUMITAP,ILCIFP,IELP)



      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#include "petsc/finclude/petscsys.h"
#include "petsc/finclude/petscvec.h"
#include "petsc/finclude/petscmat.h"

      DIMENSION NNP(6,*),MMS(4,*),NNF(4,*),ICACO(*),ILCIFS(*),IELS(*),
     &            DIRS(*),ILCIFP(*),IELP(*)
      INTEGER CNUMP(*)

      Mat         KR,M
      PetscInt   FILA,COL,D4(4),ID12(12)
      PetscScalar V,E4(4,4),E12(12,12)

!*     
!***********************************************************************
!*	INICIALIZACION DE MATRICES AUXILIARES A 0
!***********************************************************************	
!*
      DO 101 i=1,12
         DO 102 j=1,12
            E12(i,j)=0
 102     CONTINUE
 101  CONTINUE
      DO 103 i=1,4
         DO 104 j=1,4
            E4(i,j)=0
 104     CONTINUE
 103  CONTINUE
      V=0

!*      
!***********************************************************************
!*	PARTE DE LA PLACA
!***********************************************************************
!*    
      DO 110 i=1,NELP
         DO 111 j=1,3
            ID12(j)=CNUM(NNP(1,i))+j
            ID12(j+3)=CNUM(NNP(2,i))+j
            ID12(j+6)=CNUM(NNP(3,i))+j
            ID12(j+9)=CNUM(NNP(j+3,i))+1
 111     CONTINUE 
         CALL MATSETVALUES( M,12,ID12,12,ID12,E12,INSERT_VALUES,IERR)
         CALL MATSETVALUES(KR,12,ID12,12,ID12,E12,INSERT_VALUES,IERR)
 110  CONTINUE
 
!*      
!***********************************************************************
!*	PARTE DEL SOLIDO
!***********************************************************************
!*    
      DO 120 i=1,NELS
         DO 121 j=1,4
            DO 122 k=1,3
               ID12(3*(j-1)+k)= 3*(MMS(j,i)-1) + k + NGP
 122        CONTINUE
 121     CONTINUE
         CALL MATSETVALUES( M,12,ID12,12,ID12,E12,INSERT_VALUES,IERR)
         CALL MATSETVALUES(KR,12,ID12,12,ID12,E12,INSERT_VALUES,IERR)
 120  CONTINUE
 
!*      
!***********************************************************************
!*	PARTE DEL FLUIDO
!***********************************************************************
!*    
      DO 130 i=1,NELF
         DO 131 j=1,4
            ID4(j)= NNF(j) + NGP + NGS
 131     CONTINUE
         CALL MATSETVALUES( M,4,ID4,4,ID4,E4,INSERT_VALUES,IERR)
         CALL MATSETVALUES(KR,4,ID4,4,ID4,E4,INSERT_VALUES,IERR)
 130  CONTINUE
 
!*      
!***********************************************************************
!*	PARTE INTERACCION PLACA
!***********************************************************************
!*    
      DO 140 i=1,NUMITAP
         COL  = ILCIFP(i) + NGF + NGP + NGS
         DO 141 j=1,3
            FILA = CNUM(NNP(j,IELP(i)))+1 
            CALL MATSETVALUE(KR,FILA,COL,V,INSERT_VALUES,IERR)
 141     CONTINUE  	  
 140  CONTINUE
 
!*      
!***********************************************************************
!*	PARTE INTERACCION SOLIDO
!***********************************************************************
!*    
      DO 150 i=1,NUMITAS
         COL  = ILCIFS(i) + NGF + NGP + NGS
         DO 151 j=1,4
            FILA = 3*(MMS(j,IELS(i))-1) + DIRS(i) + NGP     
            CALL MATSETVALUE(KR,FILA,COL,V,INSERT_VALUES,IERR)
 151     CONTINUE    
 150  CONTINUE

!*      
!***********************************************************************
!*	PARTE INTERACCION FLUIDO
!***********************************************************************
!*    
      DO 160 i=1,NCACOF
         COL  = i + NGF + NGP + NGS
         FILA = ICACO(i) + NGP + NGS
         CALL MATSETVALUE(KR,FILA,COL,V,INSERT_VALUES,IERR)
 160  CONTINUE
          
      return
      end
