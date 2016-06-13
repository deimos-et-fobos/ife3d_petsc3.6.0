      subroutine putitasp(nelp,nrcop,nrap,irefcp,nvercos,mmp,zp,zs,
     &              numitasp,ilcisp,icrefs,ielsp,
     &              indcsp,x1,x2,x3,y1,y2,y3,zint,thin,smp,dirp,proj_sp)

      implicit double precision (a-h,o-z)

      integer nelp,nrcop,irefcp(*),nvercos,mmp(3,*),
     &        numitasp,ilcisp(*),ielsp(*),icrefs(*),
     &        inter,k,l,j,i,h,i0,dirp(*),nrap(3,*),indcsp(*),flag_int,
     &        proj_sp
      dimension x1(*),x2(*),x3(*),y1(*),y2(*),y3(*),zint(*),
     &         zs(3,*),zp(2,*),xint(6),yint(6),thin(*),smp(*)
      
      tol=0.000001
      numitasp=0
      ns1=1;ns2=1;ns3=1;

      do 3000 k=1,nelp              ! loop sobre elem 
         do 3100 j=1,nrcop          ! loop sobre referencias
            do 3200 l=1,3           ! loop sobre las aristas de la placa
               if(nrap(l,k).eq.irefcp(j)) then
                  do 3300 i=1,nvercos             ! loop sobre los vertices de cont del solido
                     if(nrap(l,k).eq.icrefs(i)) then     ! ver si la arista (l,k) tiene el mismo nr que la cara i de cont del solido

                        np1=mmp(l,k)         ! vertices de la placa
                        np2=mmp(1+mod(l,3),k)

                        ! coordenadas de los vertices de la placa
                        x1p=zp(1,np1)
                        y1p=zp(2,np1)
                        x2p=zp(1,np2)
                        y2p=zp(2,np2)

                        ! coordenadas de los vertices del solido 
                        x1s=zs(1+mod(dirp(k),3),ns1)
                        y1s=zs(1+mod(dirp(k)+1,3),ns1)
                        z1s=zs(dirp(k),ns1)
                        x2s=zs(1+mod(dirp(k),3),ns2)
                        y2s=zs(1+mod(dirp(k)+1,3),ns2)
                        z2s=zs(dirp(k),ns2)
                        x3s=zs(1+mod(dirp(k),3),ns3)
                        y3s=zs(1+mod(dirp(k)+1,3),ns3)
                        z3s=zs(dirp(k),ns3)

                        inter=0
                        if(dabs(x1p-x2p).le.tol) then
                           if(dabs(x1p-x1s).le.tol) then
                              if(dabs(x1p-x2s).le.tol) then
                                 if(dabs(x1p-x3s).le.tol) then
                                 call intersec2(y1p,y2p,y1s,z1s,y2s,z2s,
     &                                       y3s,z3s,xint,yint,smp(k),
     &                                                thin(k),inter)
                                 zi=x1p
                                 endif
                              endif
                           endif
                        else
                           if(dabs(y1p-y1s).le.tol) then
                              if(dabs(y1p-y2s).le.tol) then
                                 if(dabs(y1p-y3s).le.tol) then
                                 call intersec2(x1p,x2p,x1s,z1s,x2s,z2s,
     &                                       x3s,z3s,xint,yint,smp(k),
     &                                                thin(k),inter)
                                 zi=y1p
                                 endif
                              endif
                           endif
                        endif
  
                        if(inter.ge.3 ) then
                           call ordnod(xint,yint,inter)
                           do 4000 h=2,inter-1
                              numitasp = numitasp+1
                              ilcisp(numitasp)=i
                              ielsp(numitasp)=k
                              indcsp(numitasp)=l
                              x1(numitasp)=xint(1)
                              y1(numitasp)=yint(1)
                              x2(numitasp)=xint(h)
                              y2(numitasp)=yint(h)
                              x3(numitasp)=xint(h+1)
                              y3(numitasp)=yint(h+1)
                              zint(numitasp)=zi
 4000                      continue
                        endif
                     endif
 3300             continue
               endif   
 3200       continue    ! \j
 3100    continue       ! \k
 3000 continue
 
      return
      end
