!     ------------------------------------------------------------------
      subroutine write_grafo(n_iat,nedge_grafo,iat_v,jat_v,graph_name)
!     ------------------------------------------------------------------
!
      implicit double precision (a-h,o-z)
      dimension iat_v(*),jat_v(*)
      character*80 graph_name
!
!     ESCRIBE EL GRAFO EN FORMATO DE METIS
!
      open(20,file=graph_name)
      write(20,*) n_iat,nedge_grafo
      do 100 i=1,n_iat
        write(20,*) (jat_v(j)+1,j=iat_v(i)+1,iat_v(i+1))
 100  continue
      close(20)
!
      return
      end
