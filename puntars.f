      subroutine puntars(zs,mms,nvers,nels,nrvs,nrcs,nrcos,irefcs,
     &                   nvercos,iflag_vers,zp,mmp,nelp,nrap,nrcop,
     &                   irefcp,thin,smp,dirp,ivercos,ielps,indlp)

      implicit double precision (a-h,o-z)
      dimension mms(4,*),nrvs(4,*),irefcs(*),irefcp(*),
     &          iflag_pla(1000),iflag_sol(1000),iflag_vers(*),ielps(*),
     &          ivercos(*),indlp(*),thin(*),smp(*),mmp(3,*),zp(2,*),
     $          nrap(3,*),zs(3,*),nrcs(4,*)
      integer dirp(*)


      tol=0.000001

      do 1 i=1,1000
        iflag_sol(i)=0
        iflag_pla(i)=0
 1    continue


      do 2 ir=1,nrcos
        do 3 irp=1,nrcop
          if(irefcs(ir).eq.irefcp(irp))then
            iflag_sol(ir)=1
            iflag_pla(irp)=1
          endif
 3      continue
 2    continue

      do 4 i=1,nvers
        iflag_vers(i)=0
 4    continue
  
      do 11 k=1,nels
       do 12 j=1,4
        if(nrcs(j,k).ne.0) then
         if (nrcos.gt.0) then
           do 13 ir=1,nrcos
             if(iflag_sol(ir).ne.0)then
               if(nrcs(j,k).eq.irefcs(ir)) then
                 do 14 i=1,4
                  if(j.ne.i)then
!                    if(nrvs(i,k).lt.1 .or. nrvs(i,k).gt.7)then
                     iflag_vers(mms(i,k))=nrcs(j,k)
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

      nvercos=0
      do 20 k=1,nelp
        do 21 l=1,3
          if(nrap(l,k).ge.12)then
            do 22 irp=1,nrcop
              if(iflag_pla(irp).ne.0)then
                if(nrap(l,k).eq.irefcp(irp))then
                  do 23 i=1,nvers
                    if(iflag_vers(i).eq.nrap(l,k))then
                                      
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

                      ! coordenada del vertice del solido 
                      x1s=zs(1+mod(dirp(k),3),i)
                      y1s=zs(1+mod(dirp(k)+1,3),i)
                      z1s=zs(dirp(k),i)

                      inter=0
                      if(z1s+tol.ge.zp_i .and. z1s-tol.le.zp_s) then
                        if(dabs(x1p-x2p).le.tol) then
                          if(dabs(x1p-x1s).le.tol) then
                            if(y1p.gt.y2p)then
                              if(y1s+tol.ge.y2p .and. y1s-tol.le.y1p) 
     &                          inter=1
                            else
                              if(y1s+tol.ge.y1p .and. y1s-tol.le.y2p) 
     &                          inter=1
                            endif 
                          endif
                        else
                          if(dabs(y1p-y2p).le.tol) then
                            if(dabs(y1p-y1s).le.tol) then
                              if(x1p.gt.x2p)then
                                if(x1s+tol.ge.x2p .and. x1s-tol.le.x1p)
     &                            inter=1
                              else
                                if(x1s+tol.ge.x1p .and. x1s-tol.le.x2p)
     &                            inter=1
                              endif 
                            endif
                          endif
                        endif
                      endif

                      if(inter.eq.1)then
                        nvercos=nvercos+1
                        ivercos(nvercos)=i       ! numero de vertice del solido
                        ielps(nvercos)=k        ! elemento de la placa
                        indlp(nvercos)=l        ! lado del elemento
                        iflag_vers(i)=0         ! ilimina el vertice de la lista para que no incluya 2 veces      
                      endif

                    endif
 23               continue
                endif
              endif
 22         continue
          endif
 21     continue     
 20   continue

      return
      end
