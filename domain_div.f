      subroutine domain_div(cmin,cmax,ndiv,nglp,nelp,zp,mmp,nnp,cnump,
     &           ngls,nvers,zs,mms,nglf,nelf,zf,mmf,nnf,ncacof,ncacofp,
     &           icaco,iverco,nvercos,ivercos,vec_perm,vec_iperm,nverf,
     &           color_p,color_s,color_f,dirp,smp)

                 
      implicit double precision (a-h,o-z)
      common/p_fluido/varf
      integer cnump(*),vec_perm(*),vec_iperm(*),dof
      dimension zp(2,*),mmp(3,*),nnp(6,*),zs(3,*),mms(4,*),
     &          zf(3,*),mmf(4,*),nnf(4,*),icaco(*),iverco(3,*),
     &          ivercos(*),ndiv(3),cmin(3),cmax(3),smp(*)
      integer color_p(*),color_s(*),color_f(*),dirp(*)
      character*20 varf

      WRITE(*,*) "---> Number of divisions = ", ndiv(1)*ndiv(2)*ndiv(3)
      WRITE(*,*) "---> Divisions in x,y & z directions:"
      WRITE(*,'(6x,3(I4,1x))') (ndiv(j),j=1,3)
      WRITE(*,*) '---> Domain x, y & z coordinates:'
      WRITE(*,1234) 'x = ',cmin(1),cmax(1)
      WRITE(*,1234) 'y = ',cmin(2),cmax(2)
      WRITE(*,1234) 'z = ',cmin(3),cmax(3)
 1234 format(6x,A4,'[',f9.3,';',f9.3,']',2x)

      xmin=cmin(1)
      xmax=cmax(1)
      ymin=cmin(2)
      ymax=cmax(2)
      zmin=cmin(3)
      zmax=cmax(3)

      if(varf(1:7).eq.'presion')then
        dof=nglp+ngls+nglf+3*nvercos
      else
        dof=nglp+ngls+nglf+ncacof+3*nvercos
      endif

      xsize=abs(xmax-xmin)/ndiv(1)
      ysize=abs(ymax-ymin)/ndiv(2)
      zsize=abs(zmax-zmin)/ndiv(3)
      iperm=0
      tol=1.0d-6

      do 10 i=1,dof
        vec_perm(i)=-1
        vec_iperm(i)=-1
 10   continue

      do 20 kx=1,ndiv(1)
        x_min=xmin+xsize*(kx-1)
        x_max=xmin+xsize*kx
        if(kx.eq.ndiv(1)) x_max=xmax
      do 21 ky=1,ndiv(2)
        y_min=ymin+ysize*(ky-1)
        y_max=ymin+ysize*ky
        if(ky.eq.ndiv(2)) y_max=ymax
      do 22 kz=1,ndiv(3)
        z_min=zmin+zsize*(kz-1)
        z_max=zmin+zsize*kz
        if(kz.eq.ndiv(3)) z_max=zmax

!       Placa
        do 100 i=1,nelp
          do 101 j=1,3
            ngl=cnump(nnp(j,i))+1
            if(vec_iperm(ngl).eq.-1) then
              if(dirp(i).eq.1)then
                xbar=smp(i)
                ybar=zp(1,mmp(j,i))
                zbar=zp(2,mmp(j,i))
              elseif(dirp(i).eq.2)then
                xbar=zp(2,mmp(j,i))
                ybar=smp(i)
                zbar=zp(1,mmp(j,i))
              else
                xbar=zp(1,mmp(j,i))
                ybar=zp(2,mmp(j,i))
                zbar=smp(i)
              endif
              if((x_min-xbar).lt.tol) then
              if((y_min-ybar).lt.tol) then
              if((z_min-zbar).lt.tol) then
                if((xbar-x_max).lt.tol) then
                if((ybar-y_max).lt.tol) then
                if((zbar-z_max).lt.tol) then
                  vec_iperm(ngl)=iperm
                  vec_iperm(ngl+1)=iperm+1
                  vec_iperm(ngl+2)=iperm+2
                  vec_iperm(ngl+3)=iperm+3
                  vec_iperm(ngl+4)=iperm+4
                  iperm=iperm+5
                  color_p(mmp(j,i))= (kz-1)*ndiv(1)*ndiv(2) + 
     &                               (ky-1)*ndiv(1) + kx
                endif 
                endif 
                endif
              endif 
              endif 
              endif
            endif
            ngl=cnump(nnp(j+3,i))+1
            if(vec_iperm(ngl).eq.-1) then
              if(dirp(i).eq.1)then
                xbar=smp(i)
                ybar=(zp(1,mmp(1+mod(j,3),i))+zp(1,mmp(j,i)))/2
                zbar=(zp(2,mmp(1+mod(j,3),i))+zp(2,mmp(j,i)))/2
              elseif(dirp(i).eq.2)then
                xbar=(zp(2,mmp(1+mod(j,3),i))+zp(2,mmp(j,i)))/2
                ybar=smp(i)
                zbar=(zp(1,mmp(1+mod(j,3),i))+zp(1,mmp(j,i)))/2
              else
                xbar=(zp(1,mmp(1+mod(j,3),i))+zp(1,mmp(j,i)))/2
                ybar=(zp(2,mmp(1+mod(j,3),i))+zp(2,mmp(j,i)))/2
                zbar=smp(i)
              endif
              if((x_min-xbar).lt.tol) then
              if((y_min-ybar).lt.tol) then
              if((z_min-zbar).lt.tol) then
                if((xbar-x_max).lt.tol) then
                if((ybar-y_max).lt.tol) then
                if((zbar-z_max).lt.tol) then
                  vec_iperm(ngl)=iperm
                  iperm=iperm+1
                endif 
                endif 
                endif 
              endif 
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
            ybar=zs(2,i)
            zbar=zs(3,i)
            if((x_min-xbar).lt.tol) then
            if((y_min-ybar).lt.tol) then
            if((z_min-zbar).lt.tol) then
              if((xbar-x_max).lt.tol) then
              if((ybar-y_max).lt.tol) then
              if((zbar-z_max).lt.tol) then
                vec_iperm(ngl)=iperm
                vec_iperm(ngl+1)=iperm+1
                vec_iperm(ngl+2)=iperm+2
                iperm=iperm+3
                color_s(i)= (kz-1)*ndiv(1)*ndiv(2) + (ky-1)*ndiv(1) + kx
              endif 
              endif 
              endif 
            endif
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
            ybar=zf(2,i)
            zbar=zf(3,i)
            if((x_min-xbar).lt.tol) then
            if((y_min-ybar).lt.tol) then
            if((z_min-zbar).lt.tol) then
              if((xbar-x_max).lt.tol) then
              if((ybar-y_max).lt.tol) then
              if((zbar-z_max).lt.tol) then
                vec_iperm(ngl)=iperm
                iperm=iperm+1
                color_f(i)= (kz-1)*ndiv(1)*ndiv(2) + (ky-1)*ndiv(1) + kx
              endif 
              endif 
              endif 
            endif
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
              ybar = ( zf(2,mmf(1+mod( j ,4),i)) + 
     &                 zf(2,mmf(1+mod(j+1,4),i)) +
     &                 zf(2,mmf(1+mod(j+2,4),i)) )/3
              zbar = ( zf(3,mmf(1+mod( j ,4),i)) + 
     &                 zf(3,mmf(1+mod(j+1,4),i)) +
     &                 zf(3,mmf(1+mod(j+2,4),i)) )/3
              if((x_min-xbar).lt.tol) then
              if((y_min-ybar).lt.tol) then
              if((z_min-zbar).lt.tol) then
                if((xbar-x_max).lt.tol) then
                if((ybar-y_max).lt.tol) then
                if((zbar-z_max).lt.tol) then
                  vec_iperm(ngl)=iperm
                  iperm=iperm+1
                  color_f(mmf(1+mod( j ,4),i))= (kz-1)*ndiv(1)*ndiv(2) +
     &                                          (ky-1)*ndiv(1) + kx
                  color_f(mmf(1+mod(j+1,4),i))= (kz-1)*ndiv(1)*ndiv(2) +
     &                                          (ky-1)*ndiv(1) + kx
                  color_f(mmf(1+mod(j+2,4),i))= (kz-1)*ndiv(1)*ndiv(2) +
     &                                          (ky-1)*ndiv(1) + kx
                endif 
                endif 
                endif 
              endif 
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
            ybar = zs(2,ivercos(i))
            zbar = zs(3,ivercos(i))
            if((x_min-xbar).lt.tol) then
            if((y_min-ybar).lt.tol) then
            if((z_min-zbar).lt.tol) then
              if((xbar-x_max).lt.tol) then
              if((ybar-y_max).lt.tol) then
              if((zbar-z_max).lt.tol) then
                vec_iperm(ngl)=iperm
                vec_iperm(ngl+1)=iperm+1
                vec_iperm(ngl+2)=iperm+2
                iperm=iperm+3
              endif 
              endif 
              endif 
            endif 
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
              ybar = ( zf(2,iverco(1,i)) +
     &                 zf(2,iverco(2,i)) +
     &                 zf(2,iverco(3,i)) )/3
              zbar = ( zf(3,iverco(1,i)) +
     &                 zf(3,iverco(2,i)) +
     &                 zf(3,iverco(3,i)) )/3
              if((x_min-xbar).lt.tol) then
              if((y_min-ybar).lt.tol) then
              if((z_min-zbar).lt.tol) then
                if((xbar-x_max).lt.tol) then
                if((ybar-y_max).lt.tol) then
                if((zbar-z_max).lt.tol) then
                  vec_iperm(ngl)=iperm
                  iperm=iperm+1
                endif 
                endif 
                endif 
              endif 
              endif 
              endif 
            endif
 500      continue
        endif

  22  continue
  21  continue
  20  continue
      
      do 1000 i=1,dof
        if(vec_iperm(i).eq.-1)then
          write(*,*) 'ERROR in vec_iperm in domain_div.f --> i=',i
          STOP
        endif
 1000 continue

      do 60 i=1,dof
        vec_perm(vec_iperm(i)+1)= i-1     ! start with 0
 60   continue
      do 61 i=1,dof
        if(vec_perm(i).eq.-1)then
          write(*,*) 'ERROR in vec_perm in domain_div.f --> i=',i
          STOP
        endif
 61   continue

      return
      end
