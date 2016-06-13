!     ------------------------------------------------------------------
      subroutine read_grafo(n_iat,nedge_grafo,iat_v,jat_v,graph_name,
     &                      r_grafo,iflag)
!     ------------------------------------------------------------------
!
      implicit double precision (a-h,o-z)
      dimension iat_v(*),jat_v(*)
      character*80 graph_name
      character*30000 rec
      integer r_grafo
!
!     Primer lectura del grafo en "graph_name".
!     Lee iat_v y determina el tamaño de jat_v.
!     Si el grafo no existe o tiene error --> r_grafo=0 y crea un nuevo grafo.
!
      open(21,file=graph_name,status='OLD',err=666)
      if(iflag.eq.1)then
        read(21,*)n_iat,nedge_grafo
        iat_v(1)=1
        do 100 i=1,n_iat
          read(21,'(A)',end=666) rec
          rec=adjustl(rec)
          len_rec=len_trim(rec)
          nv = 0
          iblank = 1
          do 101 j=1,len_rec
            if(iblank.eq.0)then
              if(rec(j:j).eq.' ') then
                iblank = 1
              endif
            else
              if(rec(j:j).ne.' ') then
                iblank = 0
                nv = nv + 1
              endif
            endif
 101      continue
          iat_v(i+1) = iat_v(i) + nv
 100    continue
        close(21)
!
        if((iat_v(n_iat+1)-1).ne.(2*nedge_grafo)) goto 665
      endif

!
!     Segunda lectura del grafo en "graph_name".
!     Lee jat_v.
!     Si el grafo no existe o tiene error --> r_grafo=0 y crea un nuevo grafo.
!
      if(iflag.eq.2 .and. r_grafo.eq.1)then
        read(21,'(A)') rec
        do 200 i=1,n_iat
          read(21,*) (jat_v(j),j=iat_v(i),iat_v(i+1)-1)
 200    continue
        close(21)
!    
        do 300 i=1,n_iat+1          ! start with 0
          iat_v(i)=iat_v(i)-1
 300    continue
        do 400 i=1,iat_v(n_iat+1)   ! start with 0
          jat_v(i)=jat_v(i)-1
 400    continue
      endif
      return
!
 666  write(*,*)'---> ERROR reading '//trim(graph_name)//
     &          ' in read_grafo.f'
      goto 661
 665  write(*,*)'---> ERROR reading '//trim(graph_name)//
     &          ' in read_grafo.f'
      write(*,*)'---> 2*nedge_grafo must be equal to iat_v(n_iat+1)-1'
      write(*,*)'---> 2*nedge_grafo = ',2*nedge_grafo
      write(*,*)'---> iat_v(n_iat+1)-1 = ',iat_v(n_iat+1)-1
 661  r_grafo=0
      iat_v(n_iat+1)=1
      close(21)
      return
      end
