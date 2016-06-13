      subroutine create_grafo(ia,ja,iat,jat,iat_v,jat_v,n_iat)

      implicit double precision (a-h,o-z)
      dimension ia(*),ja(*),iat(*),jat(*),iat_v(*),jat_v(*)

!***********************************************************************  
!*    GENERA EL GRAFO PARA SER USADO POR METIS
!***********************************************************************  
      do 10 i=1,iat_v(n_iat+1)-1
        jat_v(i)=-1
 10   continue

      write(*,*) '---> Creating graph'
      icont=0
      do 100 i=1,n_iat
        do 101 j=iat(i),iat(i+1)-1
          iel=jat(j)
          do 102 k=ia(iel),ia(iel+1)-1
            nv=ja(k)
            iflag=0
            do 103 l=iat_v(i),iat_v(i+1)-1
              if(nv.eq.jat_v(l)) iflag=1
 103        continue
            if(nv.eq.i) iflag=1     ! graph without itself
            if(iflag.eq.0)then
              icont=icont+1
              jat_v(icont)=nv
            endif
 102      continue
 101    continue
 100  continue

      do 166 i=1,iat_v(n_iat+1)-1
        if(jat_v(i).eq.-1) then
          write(6,*) 'Error: creating the graph in create_grafo.f'
          stop
        endif
 166  continue

      do 110 i=1,n_iat+1
        iat_v(i)=iat_v(i)-1     ! start with 0  
 110  continue
      do 111 i=1,iat_v(n_iat+1)
        jat_v(i)=jat_v(i)-1     ! start with 0  
 111  continue

      return
      end
