      subroutine domain_div(xmax,ndiv,nglp,nelp,zp,mmp,nnp,cnump,
     &           ngls,nvers,zs,mms,nglf,nelf,zf,mmf,nnf,ncacof,ncacofp,
     &           icaco,iverco,nvercos,ivercos,vec_perm,vec_iperm,nverf,
     &           color_p,color_s,color_f)

                 
      implicit double precision (a-h,o-z)
      common/p_fluido/varf
      integer cnump(*),vec_perm(*),vec_iperm(*),dof
      dimension zp(2,*),mmp(3,*),nnp(6,*),zs(3,*),mms(4,*),
     &          zf(3,*),mmf(4,*),nnf(4,*),icaco(*),iverco(3,*),
     &          ivercos(*)
      integer color_p(*),color_s(*),color_f(*)
      character*20 varf


      if(varf(1:7).eq.'presion')then
        dof=nglp+ngls+nglf+3*nvercos
      else
        dof=nglp+ngls+nglf+ncacof+3*nvercos
      endif

      xsize=xmax/ndiv
      iperm=0
      tol=1.0d-6

      do 10 i=1,dof
        vec_iperm(i)=-1
 10   continue

      do 20 k=1,ndiv
        x_min=xsize*(k-1)
        x_max=xsize*k
        if(i.eq.ndiv) x_max=xmax

!       Placa
        do 100 i=1,nelp
          do 101 j=1,3
            ngl=cnump(nnp(j,i))+1
            if(vec_iperm(ngl).eq.-1) then
              xbar=zp(1,mmp(j,i))
              if((x_min-xbar).lt.tol) then
                if((xbar-x_max).lt.tol) then
                  vec_iperm(ngl)=iperm
                  vec_iperm(ngl+1)=iperm+1
                  vec_iperm(ngl+2)=iperm+2
                  vec_iperm(ngl+3)=iperm+3
                  vec_iperm(ngl+4)=iperm+4
                  iperm=iperm+5
                  color_p(mmp(j,i))=k
                endif
              endif
            endif
            ngl=cnump(nnp(j+3,i))+1
            if(vec_iperm(ngl).eq.-1) then
              xbar=(zp(1,mmp(1+mod(j,3),i))+zp(1,mmp(j,i)))/2
              if((x_min-xbar).lt.tol) then
                if((xbar-x_max).lt.tol) then
                  vec_iperm(ngl)=iperm
                  iperm=iperm+1
                endif
              endif
            endif
 101      continue
 100    continue

!       Solido
        do 200 i=1,nvers
          ngl=nglp+3*(i-1)+1
          if(vec_iperm(ngl).eq.-1) then
            xbar=zs(1,i)
            if((x_min-xbar).lt.tol) then
              if((xbar-x_max).lt.tol) then
                vec_iperm(ngl)=iperm
                vec_iperm(ngl+1)=iperm+1
                vec_iperm(ngl+2)=iperm+2
                iperm=iperm+3
                color_s(i)=k
              endif
            endif
          endif
 200    continue

!       Fluido
        ioffset=nglp+ngls
      if(varf(1:7).eq.'presion')then
        do 310 i=1,nverf
          ngl=ioffset+i
          if(vec_iperm(ngl).eq.-1) then
            xbar=zf(1,i)
            if((x_min-xbar).lt.tol) then
              if((xbar-x_max).lt.tol) then
                vec_iperm(ngl)=iperm
                iperm=iperm+1
                color_f(i)=k
              endif
            endif
          endif
 310    continue
      else
        do 300 i=1,nelf
          do 301 j=1,4
            ngl=ioffset+nnf(j,i)
            if(vec_iperm(ngl).eq.-1) then
              xbar = ( zf(1,mmf(1+mod( j ,4),i)) + 
     &                 zf(1,mmf(1+mod(j+1,4),i)) +
     &                 zf(1,mmf(1+mod(j+2,4),i)) )/3
              if((x_min-xbar).lt.tol) then
                if((xbar-x_max).lt.tol) then
                  vec_iperm(ngl)=iperm
                  iperm=iperm+1
                  color_f(mmf(1+mod( j ,4),i))=k
                  color_f(mmf(1+mod(j+1,4),i))=k
                  color_f(mmf(1+mod(j+2,4),i))=k
                endif
              endif
            endif
 301      continue
 300    continue
      endif

!       Contacto Solido
        ioffset=nglp+ngls+nglf
        do 400 i=1,nvercos
          ngl=ioffset+3*(i-1)+1
          if(vec_iperm(ngl).eq.-1) then
            xbar = zs(1,ivercos(i))
            if((x_min-xbar).lt.tol) then
              if((xbar-x_max).lt.tol) then
                vec_iperm(ngl)=iperm
                vec_iperm(ngl+1)=iperm+1
                vec_iperm(ngl+2)=iperm+2
                iperm=iperm+3
              endif
            endif
          endif
 400    continue

!       Contacto Fluido
        if(varf(1:7).eq.'presion')then
        else
          ioffset=nglp+ngls+nglf+3*nvercos
          do 500 i=1,ncacof
            ngl=ioffset+i
            if(vec_iperm(ngl).eq.-1) then
              xbar = ( zf(1,iverco(1,i)) +
     &                 zf(1,iverco(2,i)) +
     &                 zf(1,iverco(3,i)) )/3
              if((x_min-xbar).lt.tol) then
                if((xbar-x_max).lt.tol) then
                  vec_iperm(ngl)=iperm
                  iperm=iperm+1
                endif
              endif
            endif
 500      continue
        endif

  20  continue
      
      do 1000 i=1,dof
        if(vec_iperm(i).eq.-1)then
          write(*,*) 'ERROR in permutation.f --> i=',i
          STOP
        endif
 1000 continue

      do 60 i=1,dof
        vec_perm(vec_iperm(i)+1)= i-1     ! start with 0
 60   continue

      return
      end
