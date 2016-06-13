c	RETURN
c
c	numitap		--> n de caritas de contacto
c	inodf(numitap) 	--> nodo del fluido de contacto
c	ilcif(numitap)	--> n de cara de contacto del fluido
c	ielp(numitap)	--> n de elemento del solido
c	x1(numitap)=ax	--> vertices de la carita.
c	y1(numitap)=ay
c	x2(numitap)=bx
c	y2(numitap)=by
c	x3(numitap)=cx
c	y3(numitap)=cy
      subroutine putitap(nelp,nrcop,nrap,nrep,irefcp,ncaco,mmp,zp,zf,
     &             iverco,numitap,inodf,icaco,ilcif,icref,ielp,indcl,
     &             x1,x2,x3,y1,y2,y3,zint,thin,smp,dirp,proj_pf)

      implicit double precision (a-h,o-z)

      integer nelp,nrcop,nrep(*),irefcp(*),ncaco,mmp(3,*),iverco(3,*),
     &        numitap,inodf(*),ilcif(*),icaco(*),ielp(*),icref(*),
     &        inter,k,l,j,i,h,i0,dirp(*),nrap(3,*),indcl(*),flag_int,
     &        proj_pf
      dimension x1(*),x2(*),x3(*),y1(*),y2(*),y3(*),zint(*),
     &         zf(3,*),zp(2,*),xint(6),yint(6),thin(*),smp(*)
      
      tol=0.000001
      numitap=0

      do 1000 l=1,2                 ! loop sobre las caras de la placa 1=up, 2=down
         do 1100 k=1,nelp           ! loop sobre elem 
            do 1200 j=1,nrcop       ! loop sobre referencias
               if(nrep(k).eq.irefcp(j)) then
                  do 1300 i=1,ncaco                   ! loop sobre las caras de cont del fl
                     if(nrep(k).eq.icref(i)) then     ! ver si el elemento k tiene el mismo nr que la cara i de cont del fl

                        np1=mmp(1,k)   ! vertices del solido
                        np2=mmp(2,k)
                        np3=mmp(3,k) 
                        nf1=iverco(1,i)      ! vertices del fluido 
                        nf2=iverco(2,i)
                        nf3=iverco(3,i)

                        ! coordenadas de los vertices del solido 
                        x1p=zp(1,np1)
                        y1p=zp(2,np1)
                        x2p=zp(1,np2)
                        y2p=zp(2,np2)
                        x3p=zp(1,np3)
                        y3p=zp(2,np3)

                        if (l.eq.1) then
                           zi=smp(k)+thin(k)/2
                        else
                           zi=smp(k)-thin(k)/2 
                        endif

                        ! coordenadas de los vertices del fluido 
                        x1f=zf(1+mod(dirp(k),3),nf1)
                        y1f=zf(1+mod(dirp(k)+1,3),nf1)
                        z1f=zf(dirp(k),nf1)
                        x2f=zf(1+mod(dirp(k),3),nf2)
                        y2f=zf(1+mod(dirp(k)+1,3),nf2)
                        z2f=zf(dirp(k),nf2)
                        x3f=zf(1+mod(dirp(k),3),nf3)
                        y3f=zf(1+mod(dirp(k)+1,3),nf3)
                        z3f=zf(dirp(k),nf3)
                        
                        ! primer filtrado para intersec
                        xp_min = x1p
                        xp_max = x1p
                        yp_min = y1p
                        yp_max = y1p
                        xf_min = x1f
                        xf_max = x1f
                        yf_min = y1f
                        yf_max = y1f

                        if (x2p.lt.xp_min) xp_min=x2p 
                        if (x3p.lt.xp_min) xp_min=x3p 
                        if (x2p.gt.xp_max) xp_max=x2p 
                        if (x3p.gt.xp_max) xp_max=x3p 
                        if (y2p.lt.yp_min) yp_min=y2p 
                        if (y3p.lt.yp_min) yp_min=y3p 
                        if (y2p.gt.yp_max) yp_max=y2p 
                        if (y3p.gt.yp_max) yp_max=y3p 
                        if (x2f.lt.xf_min) xf_min=x2f 
                        if (x3f.lt.xf_min) xf_min=x3f 
                        if (x2f.gt.xf_max) xf_max=x2f 
                        if (x3f.gt.xf_max) xf_max=x3f 
                        if (y2f.lt.yf_min) yf_min=y2f 
                        if (y3f.lt.yf_min) yf_min=y3f 
                        if (y2f.gt.yf_max) yf_max=y2f 
                        if (y3f.gt.yf_max) yf_max=y3f 
                        
                        flag_int = 0

                        if(dabs(zi-z1f).le.tol) then
                          if(dabs(zi-z2f).le.tol) then
                            if(dabs(zi-z3f).le.tol) then

                        if((x1f.ge.xp_min .and. x1f.le.xp_max) .or. 
     &                     (x2f.ge.xp_min .and. x2f.le.xp_max) .or. 
     &                     (x3f.ge.xp_min .and. x3f.le.xp_max))then  
                          if((y1f.ge.yp_min .and. y1f.le.yp_max) .or. 
     &                       (y2f.ge.yp_min .and. y2f.le.yp_max) .or. 
     &                       (y3f.ge.yp_min .and. y3f.le.yp_max))then  
                            flag_int=1
                            goto 666 
                          endif
                        endif
                        if((x1p.ge.xf_min .and. x1p.le.xf_max) .or. 
     &                     (x2p.ge.xf_min .and. x2p.le.xf_max) .or. 
     &                     (x3p.ge.xf_min .and. x3p.le.xf_max))then  
                          if((y1p.ge.yf_min .and. y1p.le.yf_max) .or. 
     &                       (y2p.ge.yf_min .and. y2p.le.yf_max) .or. 
     &                       (y3p.ge.yf_min .and. y3p.le.yf_max))then  
                            flag_int=1
                          endif
                        endif

                            endif
                          endif
                        endif

 666                    continue

                        inter=0
                              if(flag_int.eq.1) then
                                call intersec(x1p,y1p,x2p,y2p,x3p,y3p,
     &                                        x1f,y1f,x2f,y2f,x3f,y3f,
     &                                        xint,yint,inter)
                              endif
!              write(87,*) "inter = ",inter
!              if ((inter.gt.0) .and. (inter.le.6)) then
!                 do 4000 h=1,inter
!                     write(87,*) xint(h),yint(h)
! 4000                   continue
!              endif
  
                        if(inter.ge.3 ) then
                           call ordnod(xint,yint,inter)
                           do 2000 h=2,inter-1
                              numitap = numitap+1
                              inodf(numitap)=icaco(i)
                              ilcif(numitap)=i
                              ielp(numitap)=k
                              indcl(numitap)=0
                              x1(numitap)=xint(1)
                              y1(numitap)=yint(1)
                              x2(numitap)=xint(h)
                              y2(numitap)=yint(h)
                              x3(numitap)=xint(h+1)
                              y3(numitap)=yint(h+1)
                              zint(numitap)=zi
!      write(2880,*) icref(i),xint(1),yint(1),xint(h),yint(h),
!     &              xint(h+1),yint(h+1),zi
 2000                      continue
                        endif
                     endif
 1300             continue
               endif   
 1200       continue    ! \j
 1100    continue       ! \k
 1000 continue          ! \l
 
      do 3000 k=1,nelp              ! loop sobre elem 
         do 3100 j=1,nrcop          ! loop sobre referencias
            do 3200 l=1,3           ! loop sobre las aristas de la placa
               if(nrap(l,k).eq.irefcp(j)) then
                  do 3300 i=1,ncaco             ! loop sobre las caras de cont del fl
                     if(nrap(l,k).eq.icref(i)) then     ! ver si la arista (l,k) tiene el mismo nr que la cara i de cont del fl

                        np1=mmp(l,k)         ! vertices de la placa
                        np2=mmp(1+mod(l,3),k)
                        nf1=iverco(1,i)         ! vertices del fluido 
                        nf2=iverco(2,i)
                        nf3=iverco(3,i)

                        ! coordenadas de los vertices del solido
                        x1p=zp(1,np1)
                        y1p=zp(2,np1)
                        x2p=zp(1,np2)
                        y2p=zp(2,np2)

                        ! coordenadas de los vertices del fluido 
                        x1f=zf(1+mod(dirp(k),3),nf1)
                        y1f=zf(1+mod(dirp(k)+1,3),nf1)
                        z1f=zf(dirp(k),nf1)
                        x2f=zf(1+mod(dirp(k),3),nf2)
                        y2f=zf(1+mod(dirp(k)+1,3),nf2)
                        z2f=zf(dirp(k),nf2)
                        x3f=zf(1+mod(dirp(k),3),nf3)
                        y3f=zf(1+mod(dirp(k)+1,3),nf3)
                        z3f=zf(dirp(k),nf3)

                        inter=0
                        if(dabs(x1p-x2p).le.tol) then
                           if(dabs(x1p-x1f).le.tol) then
                              if(dabs(x1p-x2f).le.tol) then
                                 if(dabs(x1p-x3f).le.tol) then
                                 call intersec2(y1p,y2p,y1f,z1f,y2f,z2f,
     &                                       y3f,z3f,xint,yint,smp(k),
     &                                                thin(k),inter)
                                 zi=x1p
                                 endif
                              endif
                           endif
                        else
                           if(dabs(y1p-y1f).le.tol) then
                              if(dabs(y1p-y2f).le.tol) then
                                 if(dabs(y1p-y3f).le.tol) then
                                 call intersec2(x1p,x2p,x1f,z1f,x2f,z2f,
     &                                       x3f,z3f,xint,yint,smp(k),
     &                                                thin(k),inter)
                                 zi=y1p
                                 endif
                              endif
                           endif
                        endif

!              write(87,*) "inter = ",inter
!              if ((inter.gt.0) .and. (inter.le.6)) then
!               do 4000 h=1,inter
!                   write(87,*) xint(h),yint(h)
! 4000                   continue
!              endif
  
                        if(inter.ge.3 ) then
                           call ordnod(xint,yint,inter)
                           do 4000 h=2,inter-1
                              numitap = numitap+1
                              inodf(numitap)=icaco(i)
                              ilcif(numitap)=i
                              ielp(numitap)=k
                              indcl(numitap)=l
                              x1(numitap)=xint(1)
                              y1(numitap)=yint(1)
                              x2(numitap)=xint(h)
                              y2(numitap)=yint(h)
                              x3(numitap)=xint(h+1)
                              y3(numitap)=yint(h+1)
                              zint(numitap)=zi
 4000                      continue
                        endif
                     endif
 3300             continue
               endif   
 3200       continue    ! \j
 3100    continue       ! \k
 3000 continue
 
!      close(2880)

      return
      end
