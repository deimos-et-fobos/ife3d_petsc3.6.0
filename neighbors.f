!     ------------------------------------------------------------------
!     Encuentra los vecinos de cada nodo para crear el grafo para 
!     la permutacion de columnas.
!     ------------------------------------------------------------------
      subroutine neighbors(ia,ja,n_ia,n_ja,nelp,nels,nelf,nvercos,
     &           numitap,numitas,nverp,narp,nvers,nnodf,ncacofp,
     &           ncacofs,nnp,mms,nnf,ielps,ivercos,ielp,ilcifp,inodfp,
     &           iels,ilcifs,inodfs,iat,jat,iat_v,nedge_grafo)
!     ------------------------------------------------------------------
!
      implicit double precision (a-h,o-z)
      parameter (max_vec=1000)
      dimension ia(*),ja(*),iat(*),jat(*),iat_v(*),
     &          nnp(6,*),mms(4,*),nnf(4,*),ielps(*),ivercos(*),
     &          ielp(*),ilcifp(*),inodfp(*),iels(*),ilcifs(*),
     &          inodfs(*),nod_v(max_vec)
      integer aux_i,aux_f
!
!     INICIALIZACION IA(*) y JA(*) 
!
      ia(1)=1
      inod=0
!*    PLACA
      n_aux=0;
      do 10 i=1,nelp
        inod=inod+1
        do 20 j=1,6
          ja(ia(inod)+j-1)=nnp(j,i)
 20     continue
        ia(inod+1)=ia(inod)+6
 10   continue
!*    SOLIDO
      n_aux=n_aux+nverp+narp;
      do 11 i=1,nels
        inod=inod+1
        do 21 j=1,4
          ja(ia(inod)+j-1)=mms(j,i)+n_aux
 21     continue
        ia(inod+1)=ia(inod)+4
 11   continue
!*    FLUIDO
      n_aux=n_aux+nvers;
      do 12 i=1,nelf
        inod=inod+1
        do 22 j=1,4
          ja(ia(inod)+j-1)=nnf(j,i)+n_aux
 22     continue
        ia(inod+1)=ia(inod)+4
 12   continue
!*    INT. SOLIDO-PLACA
      n_aux=n_aux+nnodf;
      do 13 i=1,nvercos
        inod=inod+1
        k=ielps(i)
        do 23 j=1,6
          ja(ia(inod)+j-1)=nnp(j,k)
 23     continue
        ja(ia(inod)+7-1)=i+n_aux
        ja(ia(inod)+8-1)=ivercos(i)+nverp+narp
        ia(inod+1)=ia(inod)+8
 13   continue
!*    INT. FLUIDO-PLACA
      n_aux=n_aux+nvercos;
      do 14 i=1,numitap
        inod=inod+1
        k=ielp(i)
        do 24 j=1,6
          ja(ia(inod)+j-1)=nnp(j,k)
 24     continue
        ja(ia(inod)+7-1)=ilcifp(i)+n_aux
        ja(ia(inod)+8-1)=inodfp(i)+nverp+narp+nvers
        ia(inod+1)=ia(inod)+8
 14   continue
!*    INT. FLUIDO-SOLIDO
      do 15 i=1,numitas
        inod=inod+1
        k=iels(i)
        do 25 j=1,4
          ja(ia(inod)+j-1)=mms(j,k)+nverp+narp
 25     continue
        ja(ia(inod)+5-1)=ilcifs(i)+n_aux
        ja(ia(inod)+6-1)=inodfs(i)+nverp+narp+nvers
        ia(inod+1)=ia(inod)+6
 15   continue
      n_aux=n_aux+ncacofp+ncacofs    
!
!     CONECTIVIDAD TRANSPUESTA
!
      write(*,*) '---> Conectividad transpuesta'
      call trasim(ia,ja,iat,jat,n_aux,n_ia)
!
!     Cuenta nodos vecinos de cada nodo y numero de lados del grafo
!
      write(*,*) '---> Nodos vecinos'
      iat_v(1)=1
      nedge_grafo=0
      do 100 i=1,n_aux
        iv=0
!        nod_v(1)=i
        do 101 j=iat(i),iat(i+1)-1
          iel=jat(j)
          do 102 k=ia(iel),ia(iel+1)-1
            nv=ja(k)
            iflag=0
            do 103 l=1,iv
              if(nv.eq.nod_v(l)) iflag=1
 103        continue
            if(nv.eq.i) iflag=1
            if(iflag.eq.0)then
              iv=iv+1
              if(iv.gt.max_vec)then
                write(*,*)'---> ERROR: max_vec in subroutine neighbors'
                stop
              endif
              nod_v(iv)=nv
              if(nv.gt.i) nedge_grafo=nedge_grafo+1
            endif 
 102      continue
 101    continue
        iat_v(i+1)=iat_v(i)+iv
 100  continue
!
      if((iat_v(n_aux+1)-1).ne.(2*nedge_grafo)) goto 666
      return
!
 666  write(*,*)'---> ERROR in subroutine neighbors.'
      write(*,*)'---> 2*nedge_grafo must be equal to iat_v(n_aux+1)-1'
      write(*,*)'---> 2*nedge_grafo = ',2*nedge_grafo
      write(*,*)'---> iat_v(n_aux+1)-1 = ',iat_v(n_aux+1)-1
      stop
      end

!     ------------------------------------------------------------------
!     Encuentra los vecinos de cada nodo para crear el grafo global. 
!     ------------------------------------------------------------------
      subroutine neighbors_g(ia,ja,n_ia,n_ja,nelp,nels,nelf,nvercos,
     &           numitap,numitas,nverp,narp,cnump,nvers,nnodf,ncacofp,
     &           ncacofs,nnp,mms,nnf,ielps,ivercos,ielp,ilcifp,inodfp,
     &           iels,ilcifs,inodfs,iat,jat,iat_v,nedge_grafo)
!     ------------------------------------------------------------------
!
      implicit double precision (a-h,o-z)
      parameter (max_vec=10000)
      dimension ia(*),ja(*),iat(*),jat(*),iat_v(*),
     &          nnp(6,*),mms(4,*),nnf(4,*),ielps(*),ivercos(*),
     &          ielp(*),ilcifp(*),inodfp(*),iels(*),ilcifs(*),
     &          inodfs(*),nod_v(max_vec)
      integer aux_i,aux_f,cnump(*)
!
!     INICIALIZACION IA(*) y JA(*) 
!
      ia(1)=1
      inod=0
!*    PLACA
      n_aux=0;
      do 10 i=1,nelp
        inod=inod+1
        do 20 j=1,3
          do 30 k=1,5
            ja(ia(inod)+5*(j-1)+k-1)=cnump(nnp(j,i))+k
 30       continue
 20     continue
        do 40 j=1,3
          ja(ia(inod)+15+j-1)=cnump(nnp(j+3,i))+1
 40     continue
        ia(inod+1)=ia(inod)+18
 10   continue
!*    SOLIDO
      n_aux=n_aux+5*nverp+narp;
      do 11 i=1,nels
        inod=inod+1
        do 21 j=1,4
          do 31 k=1,3
            ja(ia(inod)+3*(j-1)+k-1)=3*(mms(j,i)-1)+n_aux+k
 31       continue
 21     continue
        ia(inod+1)=ia(inod)+12
 11   continue
!*    FLUIDO
      n_aux=n_aux+3*nvers;
      do 12 i=1,nelf
        inod=inod+1
        do 22 j=1,4
          ja(ia(inod)+j-1)=nnf(j,i)+n_aux
 22     continue
        ia(inod+1)=ia(inod)+4
 12   continue
!*    INT. SOLIDO-PLACA
      n_aux=n_aux+nnodf;
      do 13 i=1,nvercos
        inod=inod+1
        k=ielps(i)
        do 23 j=1,3
          do 33 l=1,5
            ja(ia(inod)+5*(j-1)+l-1)=cnump(nnp(j,k))+l
 33       continue
 23     continue
        do 43 j=1,3
          ja(ia(inod)+15+j-1)=cnump(nnp(j+3,k))+1
          ja(ia(inod)+18+j-1)=3*(i-1)+n_aux+j
          ja(ia(inod)+21+j-1)=3*(ivercos(i)-1)+5*nverp+narp+j
 43     continue
        ia(inod+1)=ia(inod)+24
 13   continue
!*    INT. FLUIDO-PLACA
      n_aux=n_aux+3*nvercos;
      do 14 i=1,numitap
        inod=inod+1
        k=ielp(i)
        do 24 j=1,3
          do 34 l=1,5
            ja(ia(inod)+5*(j-1)+l-1)=cnump(nnp(j,k))+l
 34       continue
 24     continue
        do 44 j=1,3
          ja(ia(inod)+15+j-1)=cnump(nnp(j+3,k))+1
 44     continue
        ja(ia(inod)+19-1)=ilcifp(i)+n_aux
        ja(ia(inod)+20-1)=inodfp(i)+5*nverp+narp+3*nvers
        ia(inod+1)=ia(inod)+20
 14   continue
!*    INT. FLUIDO-SOLIDO
      do 15 i=1,numitas
        inod=inod+1
        k=iels(i)
        do 25 j=1,4
          do 35 l=1,3
            ja(ia(inod)+3*(j-1)+l-1)=3*(mms(j,k)-1)+5*nverp+narp+l
 35       continue
 25     continue
        ja(ia(inod)+13-1)=ilcifs(i)+n_aux
        ja(ia(inod)+14-1)=inodfs(i)+5*nverp+narp+3*nvers
        ia(inod+1)=ia(inod)+14
 15   continue
      n_aux=n_aux+ncacofp+ncacofs
!
!     CONECTIVIDAD TRANSPUESTA
!
      write(*,*) '---> Conectividad transpuesta'
      call trasim(ia,ja,iat,jat,n_aux,n_ia)
!
!     Cuenta nodos vecinos de cada nodo y numero de lados del grafo
!
      write(*,*) '---> Nodos vecinos'
      iat_v(1)=1
      nedge_grafo=0
      do 100 i=1,n_aux
        iv=0
!        nod_v(1)=i
        do 101 j=iat(i),iat(i+1)-1
          iel=jat(j)
          do 102 k=ia(iel),ia(iel+1)-1
            nv=ja(k)
            iflag=0
            do 103 l=1,iv
              if(nv.eq.nod_v(l)) iflag=1
 103        continue
            if(nv.eq.i) iflag=1
            if(iflag.eq.0)then
              iv=iv+1
              if(iv.gt.max_vec)then
                write(*,*)'---> ERROR: max_vec in subroutine'//
     &                    ' neighbors_g'
                stop
              endif
              nod_v(iv)=nv
              if(nv.gt.i) nedge_grafo=nedge_grafo+1
            endif 
 102      continue
 101    continue
        iat_v(i+1)=iat_v(i)+iv
 100  continue
!
      if((iat_v(n_aux+1)-1).ne.(2*nedge_grafo)) goto 666
      return
!
 666  write(*,*)'---> ERROR in subroutine neighbors_g.'
      write(*,*)'---> 2*nedge_grafo must be equal to iat_v(n_aux+1)-1'
      write(*,*)'---> 2*nedge_grafo = ',2*nedge_grafo
      write(*,*)'---> iat_v(n_aux+1)-1 = ',iat_v(n_aux+1)-1
      stop
      end

!     ------------------------------------------------------------------
!     Encuentra los vecinos de cada nodo para crear el grafo para la 
!     permutacion de columnas utilizando la presión del fluido como 
!     variable. 
!     ------------------------------------------------------------------
      subroutine neighbors_presion(ia,ja,n_ia,n_ja,nelp,nels,nelf,
     &           nvercos,numitap,numitas,nverp,narp,nvers,nverf,
     &           nnp,mms,mmf,ielps,ivercos,iverco,ielp,ilcifp,
     &           iels,ilcifs,iat,jat,iat_v,nedge_grafo)
!     ------------------------------------------------------------------
!
      implicit double precision (a-h,o-z)
      parameter (max_vec=1000)
      dimension ia(n_ia+1),ja(n_ja),
     &          iat(nverp+narp+nvers+nverf+nvercos+1),
     &          iat_v(nverp+narp+nvers+nverf+nvercos+1),
     &          jat(n_ja),iverco(3,*),
     &          nnp(6,*),mms(4,*),mmf(4,*),ielps(*),ivercos(*),
     &          ielp(*),ilcifp(*),iels(*),ilcifs(*),nod_v(max_vec)
      integer aux_i,aux_f
!
!     INICIALIZACION IA(*) y JA(*) 
!
      ia(1)=1
      inod=0
!*    PLACA
      n_aux=0;
      do 10 i=1,nelp
        inod=inod+1
        do 20 j=1,6
          ja(ia(inod)+j-1)=nnp(j,i)
 20     continue
        ia(inod+1)=ia(inod)+6
 10   continue
!*    SOLIDO
      n_aux=n_aux+nverp+narp;
      do 11 i=1,nels
        inod=inod+1
        do 21 j=1,4
          ja(ia(inod)+j-1)=mms(j,i)+n_aux
 21     continue
        ia(inod+1)=ia(inod)+4
 11   continue
!*    FLUIDO
      n_aux=n_aux+nvers;
      do 12 i=1,nelf
        inod=inod+1
        do 22 j=1,4
          ja(ia(inod)+j-1)=mmf(j,i)+n_aux
 22     continue
        ia(inod+1)=ia(inod)+4
 12   continue
!*    INT. SOLIDO-PLACA
      n_aux=n_aux+nverf;
      do 13 i=1,nvercos
        inod=inod+1
        k=ielps(i)
        do 23 j=1,6
          ja(ia(inod)+j-1)=nnp(j,k)
 23     continue
        ja(ia(inod)+7-1)=i+n_aux
        ja(ia(inod)+8-1)=ivercos(i)+nverp+narp
        ia(inod+1)=ia(inod)+8
 13   continue
!*    INT. FLUIDO-PLACA
      n_aux=n_aux+nvercos;
      do 14 i=1,numitap
        inod=inod+1
        k=ielp(i)
        do 24 j=1,6
          ja(ia(inod)+j-1)=nnp(j,k)
 24     continue
        l=ilcifp(i)
        do 34 j=1,3
          ja(ia(inod)+6+j-1)=iverco(j,l)+nverp+narp+nvers
 34     continue
        ia(inod+1)=ia(inod)+9
 14   continue
!*    INT. FLUIDO-SOLIDO
      do 15 i=1,numitas
        inod=inod+1
        k=iels(i)
        do 25 j=1,4
          ja(ia(inod)+j-1)=mms(j,k)+nverp+narp
 25     continue
        l=ilcifs(i)
        do 35 j=1,3
          ja(ia(inod)+4+j-1)=iverco(j,l)+nverp+narp+nvers
 35     continue
        ia(inod+1)=ia(inod)+7
 15   continue
!
!     CONECTIVIDAD TRANSPUESTA
!
      write(*,*) '---> Conectividad transpuesta'
      call trasim(ia,ja,iat,jat,n_aux,n_ia)
!
!     Cuenta nodos vecinos de cada nodo y numero de lados del grafo
!
      write(*,*) '---> Nodos vecinos'
      iat_v(1)=1
      nedge_grafo=0
      do 100 i=1,n_aux
        iv=0
!        nod_v(1)=i
        do 101 j=iat(i),iat(i+1)-1
          iel=jat(j)
          do 102 k=ia(iel),ia(iel+1)-1
            nv=ja(k)
            iflag=0
            do 103 l=1,iv
              if(nv.eq.nod_v(l)) iflag=1
 103        continue
            if(nv.eq.i) iflag=1
            if(iflag.eq.0)then
              iv=iv+1
              if(iv.gt.max_vec)then
                write(*,*)'---> ERROR: max_vec in subroutine neighbors'
                stop
              endif
              nod_v(iv)=nv
              if(nv.gt.i) nedge_grafo=nedge_grafo+1
            endif 
 102      continue
 101    continue
        iat_v(i+1)=iat_v(i)+iv
 100  continue
!
      if((iat_v(n_aux+1)-1).ne.(2*nedge_grafo)) goto 666
      return
!
 666  write(*,*)'---> ERROR in subroutine neighbors_presion.'
      write(*,*)'---> 2*nedge_grafo must be equal to iat_v(n_aux+1)-1'
      write(*,*)'---> 2*nedge_grafo = ',2*nedge_grafo
      write(*,*)'---> iat_v(n_aux+1)-1 = ',iat_v(n_aux+1)-1
      stop
      end

!     ------------------------------------------------------------------
!     Encuentra los vecinos de cada nodo para crear el grafo global 
!     utilizando la presión del fluido como variable. 
!     ------------------------------------------------------------------
      subroutine neighbors_g_presion(ia,ja,n_ia,n_ja,nelp,nels,nelf,
     &           nvercos,numitap,numitas,nverp,narp,cnump,nvers,nverf,
     &           ncacofp,ncacofs,nnp,mms,mmf,ielps,ivercos,iverco,ielp,
     &           ilcifp,iels,ilcifs,iat,jat,iat_v,nedge_grafo)
!     ------------------------------------------------------------------
!
      implicit double precision (a-h,o-z)
      parameter (max_vec=10000)
      dimension ia(*),ja(*),iat(*),jat(*),iat_v(*),iverco(3,*),
     &          nnp(6,*),mms(4,*),mmf(4,*),ielps(*),ivercos(*),
     &          ielp(*),ilcifp(*),iels(*),ilcifs(*),nod_v(max_vec)
      integer cnump(*)
!
!     INICIALIZACION IA(*) y JA(*) 
!
      ia(1)=1
      inod=0
!*    PLACA
      n_aux=0;
      do 10 i=1,nelp
        inod=inod+1
        do 20 j=1,3
          do 30 k=1,5
            ja(ia(inod)+5*(j-1)+k-1)=cnump(nnp(j,i))+k
 30       continue
 20     continue
        do 40 j=1,3
          ja(ia(inod)+15+j-1)=cnump(nnp(j+3,i))+1
 40     continue
        ia(inod+1)=ia(inod)+18
 10   continue
!*    SOLIDO
      n_aux=n_aux+5*nverp+narp;
      do 11 i=1,nels
        inod=inod+1
        do 21 j=1,4
          do 31 k=1,3
            ja(ia(inod)+3*(j-1)+k-1)=3*(mms(j,i)-1)+n_aux+k
 31       continue
 21     continue
        ia(inod+1)=ia(inod)+12
 11   continue
!*    FLUIDO
      n_aux=n_aux+3*nvers;
      do 12 i=1,nelf
        inod=inod+1
        do 22 j=1,4
          ja(ia(inod)+j-1)=mmf(j,i)+n_aux
 22     continue
        ia(inod+1)=ia(inod)+4
 12   continue
!*    INT. SOLIDO-PLACA
      n_aux=n_aux+nverf;
      do 13 i=1,nvercos
        inod=inod+1
        k=ielps(i)
        do 23 j=1,3
          do 33 l=1,5
            ja(ia(inod)+5*(j-1)+l-1)=cnump(nnp(j,k))+l
 33       continue
 23     continue
        do 43 j=1,3
          ja(ia(inod)+15+j-1)=cnump(nnp(j+3,k))+1
          ja(ia(inod)+18+j-1)=3*(i-1)+n_aux+j
          ja(ia(inod)+21+j-1)=3*(ivercos(i)-1)+5*nverp+narp+j
 43     continue
        ia(inod+1)=ia(inod)+24
 13   continue
!*    INT. FLUIDO-PLACA
      n_aux=n_aux+3*nvercos;
      do 14 i=1,numitap
        inod=inod+1
        k=ielp(i)
        do 24 j=1,3
          do 34 l=1,5
            ja(ia(inod)+5*(j-1)+l-1)=cnump(nnp(j,k))+l
 34       continue
 24     continue
        l=ilcifp(i)
        do 44 j=1,3
          ja(ia(inod)+15+j-1)=cnump(nnp(j+3,k))+1
          ja(ia(inod)+18+j-1)=iverco(j,l)+5*nverp+narp+3*nvers
 44     continue
        ia(inod+1)=ia(inod)+21
 14   continue
!*    INT. FLUIDO-SOLIDO
      do 15 i=1,numitas
        inod=inod+1
        k=iels(i)
        do 25 j=1,4
          do 35 l=1,3
            ja(ia(inod)+3*(j-1)+l-1)=3*(mms(j,k)-1)+5*nverp+narp+l
 35       continue
 25     continue
        l=ilcifs(i)
        do 45 j=1,3
          ja(ia(inod)+12+j-1)=iverco(j,l)+5*nverp+narp+3*nvers
 45     continue
        ia(inod+1)=ia(inod)+15
 15   continue
!
!     CONECTIVIDAD TRANSPUESTA
!
      write(*,*) '---> Conectividad transpuesta'
      call trasim(ia,ja,iat,jat,n_aux,n_ia)
!
!     Cuenta nodos vecinos de cada nodo y numero de lados del grafo
!
      write(*,*) '---> Nodos vecinos'
      iat_v(1)=1
      nedge_grafo=0
      do 100 i=1,n_aux
        iv=0
!        nod_v(1)=i
        do 101 j=iat(i),iat(i+1)-1
          iel=jat(j)
          do 102 k=ia(iel),ia(iel+1)-1
            nv=ja(k)
            iflag=0
            do 103 l=1,iv
              if(nv.eq.nod_v(l)) iflag=1
 103        continue
            if(nv.eq.i) iflag=1
            if(iflag.eq.0)then
              iv=iv+1
              if(iv.gt.max_vec)then
                write(*,*)'---> ERROR: max_vec in subroutine'//
     &                    ' neighbors_g'
                stop
              endif
              nod_v(iv)=nv
              if(nv.gt.i) nedge_grafo=nedge_grafo+1
            endif 
 102      continue
 101    continue
        iat_v(i+1)=iat_v(i)+iv
 100  continue
!
      if((iat_v(n_aux+1)-1).ne.(2*nedge_grafo)) goto 666
      return
!
 666  write(*,*)'---> ERROR in subroutine neighbors_g_presion.'
      write(*,*)'---> 2*nedge_grafo must be equal to iat_v(n_aux+1)-1'
      write(*,*)'---> 2*nedge_grafo = ',2*nedge_grafo
      write(*,*)'---> iat_v(n_aux+1)-1 = ',iat_v(n_aux+1)-1
      stop
      end
