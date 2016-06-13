      subroutine puntar_presion(zf,mmf,nverf,nelf,nrcf,nrcof,irefcf,
     &                   nvercof,iflag_verf,zp,mmp,nelp,nrap,nrep,nrcop,
     &                   irefcp,thin,smp,dirp,ivercof,ielpf,indlpf)

      implicit double precision (a-h,o-z)
      dimension mmf(4,*),irefcf(*),irefcp(*),nrep(*),
     &          iflag_pla(1000),iflag_flu(1000),iflag_verf(*),ielpf(*),
     &          ivercof(*),indlpf(*),thin(*),smp(*),mmp(3,*),zp(2,*),
     $          nrap(3,*),zf(3,*),nrcf(4,*)
      integer dirp(*)


      tol=0.000001

      do 1 i=1,1000
        iflag_flu(i)=0 
        iflag_pla(i)=0
 1    continue


      do 2 ir=1,nrcof
        do 3 irp=1,nrcop
          if(irefcf(ir).eq.irefcp(irp))then
            iflag_flu(ir)=1
            iflag_pla(irp)=1
          endif
 3      continue
 2    continue

      do 4 i=1,nverf
        iflag_verf(i)=0
 4    continue
  
      do 11 k=1,nelf
       do 12 j=1,4
        if(nrcf(j,k).ne.0) then
         if (nrcof.gt.0) then
           do 13 ir=1,nrcof
             if(iflag_flu(ir).ne.0)then
               if(nrcf(j,k).eq.irefcf(ir)) then
                 do 14 i=1,4
                  if(j.ne.i)then
!                    if(nrvs(i,k).lt.1 .or. nrvs(i,k).gt.7)then
                     iflag_verf(mmf(i,k))=nrcf(j,k)
!                    endif
                  endif
 14              continue
               endif
             endif
 13        continue
         end if
        end if
 12    continue
 11   continue

      nvercof=0
      do 1111 i=1,nverf
        if(iflag_verf(i).ne.0) nvercof=nvercof+1
 1111 continue

      nvercof=0
      do 20 k=1,nelp
        do 21 irp=1,nrcop
          if(iflag_pla(irp).ne.0)then
            if(nrep(k).eq.irefcp(irp))then
              do 22 i=1,nverf
                if(iflag_verf(i).eq.nrep(k))then
                  do 23 l=1,2 

                  np1=mmp(1,k)        ! vertices de la placa            
                  np2=mmp(2,k)
                  np3=mmp(3,k)

                  ! coordenadas del lado de la placa
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

                  ! coordenada del vertice del fluido 
                  x1f=zf(1+mod(dirp(k),3),i)
                  y1f=zf(1+mod(dirp(k)+1,3),i)
                  z1f=zf(dirp(k),i)

                  ! primer filtrado para intersec
                  xp_min = x1p
                  xp_max = x1p
                  yp_min = y1p
                  yp_max = y1p

                  if (x2p.lt.xp_min) xp_min=x2p
                  if (x3p.lt.xp_min) xp_min=x3p
                  if (x2p.gt.xp_max) xp_max=x2p
                  if (x3p.gt.xp_max) xp_max=x3p
                  if (y2p.lt.yp_min) yp_min=y2p
                  if (y3p.lt.yp_min) yp_min=y3p
                  if (y2p.gt.yp_max) yp_max=y2p
                  if (y3p.gt.yp_max) yp_max=y3p
 
                  flag_int = 0

                  if(dabs(zi-z1f).le.tol) then
                    if(x1f.ge.xp_min .and. x1f.le.xp_max)then
                      if(y1f.ge.yp_min .and. y1f.le.yp_max)then
                        flag_int=1
                      endif
                    endif
                  endif

                  inter=0
                  if(flag_int.eq.1) then
                    call intersec_presion(x1p,y1p,x2p,y2p,x3p,y3p,
     &                            x1f,y1f,inter)
                  endif

                  if(inter.eq.1)then
                    nvercof=nvercof+1
                    ivercof(nvercof)=i       ! numero de vertice del solido
                    ielpf(nvercof)=k        ! elemento de la placa
                    indlpf(nvercof)=0        ! lado del elemento
                    iflag_verf(i)=0         ! ilimina el vertice de la lista para que no incluya 2 veces      
                  endif

 23               continue
                endif
 22           continue
            endif
          endif
 21     continue     
 20   continue

      do 30 k=1,nelp
        do 31 l=1,3
          if(nrap(l,k).ge.7)then
            do 32 irp=1,nrcop
              if(iflag_pla(irp).ne.0)then
                if(nrap(l,k).eq.irefcp(irp))then
                  do 33 i=1,nverf
                    if(iflag_verf(i).eq.nrap(l,k))then
                                      
                      ! vertices de la placa 
                      np1=mmp(l,k)            
                      np2=mmp(1+mod(l,3),k)

                      ! coordenadas del lado de la placa
                      x1p=zp(1,np1)
                      y1p=zp(2,np1)
                      x2p=zp(1,np2)
                      y2p=zp(2,np2)  
                      zp_s=smp(k)+thin(k)/2                        
                      zp_i=smp(k)-thin(k)/2                        

                      ! coordenada del vertice del fluido 
                      x1f=zf(1+mod(dirp(k),3),i)
                      y1f=zf(1+mod(dirp(k)+1,3),i)
                      z1f=zf(dirp(k),i)

                      inter=0
                      if(z1f+tol.ge.zp_i .and. z1f-tol.le.zp_s) then
                        if(dabs(x1p-x2p).le.tol) then
                          if(dabs(x1p-x1f).le.tol) then
                            if(y1p.gt.y2p)then
                              if(y1f+tol.ge.y2p .and. y1f-tol.le.y1p) 
     &                          inter=1
                            else
                              if(y1f+tol.ge.y1p .and. y1f-tol.le.y2p) 
     &                          inter=1
                            endif 
                          endif
                        else
                          if(dabs(y1p-y2p).le.tol) then
                            if(dabs(y1p-y1f).le.tol) then
                              if(x1p.gt.x2p)then
                                if(x1f+tol.ge.x2p .and. x1f-tol.le.x1p)
     &                            inter=1
                              else
                                if(x1f+tol.ge.x1p .and. x1f-tol.le.x2p)
     &                            inter=1
                              endif 
                            endif
                          endif
                        endif
                      endif

                      if(inter.eq.1)then
                        nvercof=nvercof+1
                        ivercof(nvercof)=i       ! numero de vertice del fluido
                        ielpf(nvercof)=k        ! elemento de la placa
                        indlpf(nvercof)=l        ! lado del elemento
                        iflag_verf(i)=0         ! ilimina el vertice de la lista para que no incluya 2 veces      
                      endif

                    endif
 33               continue
                endif
              endif
 32         continue
          endif
 31     continue     
 30   continue

      return
      end
