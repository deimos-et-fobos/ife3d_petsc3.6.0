cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                      c
      subroutine caljee(nef,mm,iet,jet,jee,nod,nel)
c                                                                      c
c     OBJETIVO:         Calcula la conectividad de elementos           c
c                                                                      c
c     coded by Ronia                                                   c
c                                                                      c
c     INPUT:                                                           c
c           nef:        nodos por elementos finitos                    c
c           nel:        # de elementos                                 c
c           mm:         matriz de lados                                c
c           iet,jet     la tranpuesta de je                            c
c           jee:        matriz de conectividad de elementos            c
c     OUTPUT:                                                          c
c           jee:        matriz de conectividad de los elementos        c
c                                                                      c
c                                                                      c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit double precision (a-h,o-z)

      dimension mm(4,*),iet(*),jet(*),jee(*)

c
c     calcula la matriz de conectividad de los elementos
c
      call trasim(nef,mm,iet,jet,nod,nel)
!     write(1202,*) (iet(i),i=1,nod+1)
!     write(1203,*) (jet(i),i=1,iet(nod+1)-1)
      call conel4(mm,iet,jet,jee,nel)
!      write(1204,*) (jee(i),i=1,4*nel)

      return
      end
