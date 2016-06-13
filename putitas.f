c	RETURN
c
c	numitas		--> n de caritas de contacto
c	inodf(numitas) 	--> nodo del fluido de contacto
c	ilcif(numitas)	--> n de cara de contacto del fluido
c	iels(numitas)	--> n de elemento del solido
c	x1(numitas)=ax	--> vertices de la carita.
c	y1(numitas)=ay
c	x2(numitas)=bx
c	y2(numitas)=by
c	x3(numitas)=cx
c	y3(numitas)=cy
      subroutine putitas(nels,nrcos,nrcs,irefcs,ncaco,mms,zs,zf,
     &               iverco,numitas,inodf,icaco,ilcif,icref,iels,
     &               indcs,x1,x2,x3,y1,y2,y3,z1,z2,z3,proj)

      implicit double precision (a-h,o-z)

      integer nels,nrcos,irefcs(*),ncaco,mms(4,*),iverco(3,*),
     &        numitas,inodf(*),ilcif(*),icaco(*),iels(*),icref(*),
     &        inter,nrcs(4,*),indcs(*),cn(4,4),h,proj
      dimension x1(*),x2(*),x3(*),y1(*),y2(*),y3(*),z1(*),z2(*),z3(*),
     &          zf(3,*),zs(3,*),xint(6),yint(6),zint(6)

      tol=0.000001
      numitas=0
      cn(1,1)=2; cn(1,2)=3; cn(1,3)=4; cn(1,4)=1
      cn(2,1)=1; cn(2,2)=3; cn(2,3)=4; cn(2,4)=2
      cn(3,1)=1; cn(3,2)=2; cn(3,3)=4; cn(3,4)=3
      cn(4,1)=1; cn(4,2)=2; cn(4,3)=3; cn(4,4)=4

      do 1000 k=1,nels                 ! loop sobre elem 
         do 1100 j=1,nrcos             ! loop sobre referencias
            do 1200 l=1,4              ! loop sobre las caras del elemento
               if(nrcs(l,k).eq.irefcs(j)) then
                  do 1300 i=1,ncaco    ! loop sobre las caras de cont del fl
                     if(nrcs(l,k).eq.icref(i)) then     ! ver si el elemento k tiene el mismo nr que la cara i de cont del fl

                        ns1=mms(cn(l,1),k)      ! vertices del solido
                        ns2=mms(cn(l,2),k)
                        ns3=mms(cn(l,3),k) 
                        nf1=iverco(1,i)         ! vertices del fluido 
                        nf2=iverco(2,i)
                        nf3=iverco(3,i)

                        ! coordenadas de los vertices del solido
                        x1s=zs(1,ns1)
                        y1s=zs(2,ns1)
                        z1s=zs(3,ns1)
                        x2s=zs(1,ns2)
                        y2s=zs(2,ns2)
                        z2s=zs(3,ns2)
                        x3s=zs(1,ns3)
                        y3s=zs(2,ns3)
                        z3s=zs(3,ns3)

                        ! coordenadas de los vertices del fluido 
                        x1f=zf(1,nf1)
                        y1f=zf(2,nf1)
                        z1f=zf(3,nf1)
                        x2f=zf(1,nf2)
                        y2f=zf(2,nf2)
                        z2f=zf(3,nf2)
                        x3f=zf(1,nf3)
                        y3f=zf(2,nf3)
                        z3f=zf(3,nf3)

                        ! primer filtrado para intersec
                        xs_min = x1s
                        xs_max = x1s
                        ys_min = y1s
                        ys_max = y1s
                        zs_min = z1s
                        zs_max = z1s
                        xf_min = x1f
                        xf_max = x1f
                        yf_min = y1f
                        yf_max = y1f
                        zf_min = z1f
                        zf_max = z1f

                        if (x2s.lt.xs_min) xs_min=x2s
                        if (x3s.lt.xs_min) xs_min=x3s
                        if (x2s.gt.xs_max) xs_max=x2s
                        if (x3s.gt.xs_max) xs_max=x3s
                        if (y2s.lt.ys_min) ys_min=y2s
                        if (y3s.lt.ys_min) ys_min=y3s
                        if (y2s.gt.ys_max) ys_max=y2s
                        if (y3s.gt.ys_max) ys_max=y3s
                        if (z2s.lt.zs_min) zs_min=z2s
                        if (z3s.lt.zs_min) zs_min=z3s
                        if (z2s.gt.zs_max) zs_max=z2s
                        if (z3s.gt.zs_max) zs_max=z3s
                        if (x2f.lt.xf_min) xf_min=x2f
                        if (x3f.lt.xf_min) xf_min=x3f
                        if (x2f.gt.xf_max) xf_max=x2f
                        if (x3f.gt.xf_max) xf_max=x3f
                        if (y2f.lt.yf_min) yf_min=y2f
                        if (y3f.lt.yf_min) yf_min=y3f
                        if (y2f.gt.yf_max) yf_max=y2f
                        if (y3f.gt.yf_max) yf_max=y3f
                        if (z2f.lt.zf_min) zf_min=z2f
                        if (z3f.lt.zf_min) zf_min=z3f
                        if (z2f.gt.zf_max) zf_max=z2f
                        if (z3f.gt.zf_max) zf_max=z3f

                        flag_int=0
                        
                        if((x1f.ge.xs_min .and. x1f.le.xs_max) .or.
     &                     (x2f.ge.xs_min .and. x2f.le.xs_max) .or.
     &                     (x3f.ge.xs_min .and. x3f.le.xs_max))then
                          if((y1f.ge.ys_min .and. y1f.le.ys_max) .or.
     &                       (y2f.ge.ys_min .and. y2f.le.ys_max) .or.
     &                       (y3f.ge.ys_min .and. y3f.le.ys_max))then
                            if((z1f.ge.zs_min .and. z1f.le.zs_max) .or.
     &                         (z2f.ge.zs_min .and. z2f.le.zs_max) .or.
     &                         (z3f.ge.zs_min .and. z3f.le.zs_max))then
                              flag_int=1
                              goto 666
                            endif
                          endif
                        endif
                        if((x1s.ge.xf_min .and. x1s.le.xf_max) .or.
     &                     (x2s.ge.xf_min .and. x2s.le.xf_max) .or.
     &                     (x3s.ge.xf_min .and. x3s.le.xf_max))then
                          if((y1s.ge.yf_min .and. y1s.le.yf_max) .or.
     &                       (y2s.ge.yf_min .and. y2s.le.yf_max) .or.
     &                       (y3s.ge.yf_min .and. y3s.le.yf_max))then
                            if((z1s.ge.zf_min .and. z1s.le.zf_max) .or.
     &                         (z2s.ge.zf_min .and. z2s.le.zf_max) .or.
     &                         (z3s.ge.zf_min .and. z3s.le.zf_max))then
                               flag_int=1
                            endif
                          endif
                        endif
                        
 666                    continue

                        inter=0
                        
                        if(flag_int.eq.1) then
                          call intersecs(x1s,y1s,z1s,x2s,y2s,z2s,
     &                         x3s,y3s,z3s,x1f,y1f,z1f,x2f,y2f,z2f,
     &                         x3f,y3f,z3f,xint,yint,zint,inter,proj)
                        endif

!              write(87,*) "inter = ",inter
!              if ((inter.gt.0) .and. (inter.le.6)) then
!                 do 4000 h=1,inter
!                     write(87,*) xint(h),yint(h)
! 4000                   continue
!              endif
  
                        if(inter.ge.3 ) then
                           call ordnods(xint,yint,zint,inter)
                           do 2000 h=2,inter-1
                              numitas = numitas+1
                              inodf(numitas)=icaco(i)
                              ilcif(numitas)=i
                              iels(numitas)=k
                              indcs(numitas)=l
                              x1(numitas)=xint(1)
                              y1(numitas)=yint(1)
                              z1(numitas)=zint(1)
                              x2(numitas)=xint(h)
                              y2(numitas)=yint(h)
                              z2(numitas)=zint(h)
                              x3(numitas)=xint(h+1)
                              y3(numitas)=yint(h+1)
                              z3(numitas)=zint(h+1)
!      write(2881,*) icref(i),xint(1),yint(1),zint(1),xint(h),yint(h),
!     &              zint(h),xint(h+1),yint(h+1),zint(h+1)
 2000                      continue
                        endif
                     endif
 1300             continue
               endif   
 1200       continue    ! \l
 1100    continue       ! \j
 1000 continue          ! \k
 
!      close(2881)
      return
      end
