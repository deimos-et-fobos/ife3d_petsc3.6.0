ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c									c
      function cal_jlf(elemento,mmf,nnf,zf,zp,mmp,componentes,
     &      jeef,nrcof,irefcf,nrcf,ncacof,inodfp,numitap,x1p,y1p,x2p,
     &      y2p,x3p,y3p,zintp,indcp,uf,ielp,dirp,smp,componentes_pz,
     &      componentes_prot,ndirf,nuf)
     
c									c
c	INPUT:								c
c		ndirf --> nro de lados con cond de dir
c									c
c	RETURN:								c
c		el valor de J_l 					c
c									c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit double precision (a-h,o-z)

      dimension mmf(4,*),zf(3,*),nnf(4,*),uf(*),zp(2,*),irefcf(*),
     &            componentes(4,*),jeef(*),nrcf(4,*),inodfp(*),ielp(*),
     &            x1p(*),y1p(*),x2p(*),y2p(*),x3p(*),y3p(*),zintp(*),
     &            indcp(*),componentes_pz(3,*),componentes_prot(2,3,*),
     &            u(3),v(3),t(3),cv1(3),cv2(3),cv3(3),cv12(3),cv23(3),
     &            cv31(3),w(3),rot(3),smp(*)
      double precision nuf(3,*),cal_jlf,nx,ny,nz,l1,l2,l3,lmax,nu
      integer elemento,vecino,vecinop,referencia,dirich,contacto,
     &            cn(4,4),dirp(*),mmp(3,*)

      cn(1,1)=2; cn(1,2)=3; cn(1,3)=4; cn(1,4)=1
      cn(2,1)=1; cn(2,2)=3; cn(2,3)=4; cn(2,4)=2
      cn(3,1)=1; cn(3,2)=2; cn(3,3)=4; cn(3,4)=3
      cn(4,1)=1; cn(4,2)=2; cn(4,3)=3; cn(4,4)=4

      aux=0.0     
      tol=1.d-9
      normalborde = 2   ! solo es para incializar, si se usa, cambia de valor antes de ser usado
      do 10 i=1,4
         aux2=0.0
         f12=0.0
         f23=0.0
         f31=0.0

         ! vertices de la cara i
         n1=mmf(cn(i,1),elemento)
         n2=mmf(cn(i,2),elemento)
         n3=mmf(cn(i,3),elemento)

         ! coordenadas de los vertices de la cara i
         do 20 j=1,3
            cv1(j)=zf(j,n1)
            cv2(j)=zf(j,n2)
            cv3(j)=zf(j,n3)
            cv12(j)=(cv1(j)+cv2(j))/2
            cv23(j)=(cv2(j)+cv3(j))/2
            cv31(j)=(cv3(j)+cv1(j))/2
 20      continue
         do 21 j=1,3
            u(j)=cv2(j)-cv1(j)
            v(j)=cv3(j)-cv1(j)
            t(j)=cv3(j)-cv2(j)
 21      continue
 
         l1=dsqrt(t(1)**2+t(2)**2+t(3)**2)
         l2=dsqrt(u(1)**2+u(2)**2+u(3)**2)
         l3=dsqrt(v(1)**2+v(2)**2+v(3)**2)
 
         lmax=l1
         if(l2.gt.lmax) lmax = l2
         if(l3.gt.lmax) lmax = l3
         
         area=dsqrt((u(2)*v(3)-u(3)*v(2))**2+(u(3)*v(1)-
     &                  u(1)*v(3))**2+(u(1)*v(2)-u(2)*v(1))**2)/2.d0
         nx=nuf(1,nnf(i,elemento))
         ny=nuf(2,nnf(i,elemento))
         nz=nuf(3,nnf(i,elemento))

         a_i=componentes(1,elemento)
         b_i=componentes(2,elemento)
         c_i=componentes(3,elemento)
         d_i=componentes(4,elemento)

         vecino=jeef(i+4*(elemento-1))    ! se fija cual es el vecino
         dirich=0
         contacto=0
         
         if(vecino.eq.0)then              ! si no existe vecino
            referencia=nrcf(i,elemento)

            if(referencia.eq.1)then
               dirich=1
               ndirf=ndirf+1
            endif

            do 30 j=1,nrcof               ! busca si es de contacto
               if(referencia.eq.irefcf(j)) contacto=1
 30         continue

            if(dirich.eq.1)then           ! si es de dirichlet
!               a=2*a_i
!               b=2*b_i
!               c=2*c_i
!               d=2*d_i

!               f12=(a+d*cv12(1))**2+(b+d*cv12(2))**2+(c+d*cv12(3))**2
!               f23=(a+d*cv23(1))**2+(b+d*cv23(2))**2+(c+d*cv23(3))**2
!               f31=(a+d*cv31(1))**2+(b+d*cv31(2))**2+(c+d*cv31(3))**2
               a=0
               b=0
               c=0
               d=0

               f12=0
               f23=0
               f31=0
!               goto 666
               
            else
               if(contacto.eq.1)then      ! si es de contacto
                  do 40 j=1,numitap
                     nfl=inodfp(j)
                     if(nfl.eq.nnf(i,elemento)) then
                        clf=uf(nfl)
                        x1=x1p(j)
                        y2=y1p(j)
                        x2=x2p(j)
                        y2=y2p(j)
                        x3=x3p(j)
                        y3=y3p(j)
                        zi= zintp(j)
                        indp= indcp(j)
                        vecinop=ielp(j)
                        idirp= dirp(vecinop)
                        nu = nuf(idirp,nnf(i,elemento))
                        clf = nu * clf
                        if (indp.ne.0) then
                           xi = zp(1,mmp(indp,vecinop))
                           xf = zp(1,mmp(1+mod(indp,3),vecinop))
                           normalborde = 2 
                           if (dabs(xi-xf).gt.tol) then
                              xi = zp(2,mmp(indp,vecinop))
                              xf = zp(2,mmp(1+mod(indp,3),vecinop))
                              normalborde = 1
                           endif
                        endif
                        zmp=smp(vecinop)
                        do 41 k=1,3
                           w(k)=componentes_pz(k,vecinop)
                         rot(k)=componentes_prot(normalborde,k,vecinop)
 41                     continue
!                        write(175,*) x1,y1,x2,y2,x3,y3,zi,indp,vecinop,
!     &                               idirp,clf,zmp,w,rot
                        aux2=aux2+contfl(x1,y1,x2,y2,x3,y3,zi,
     &                                    indp,clf,w,rot,zmp)
                        
                     endif
 40               continue
               else                       ! si es Neumann
               
                  a=2*a_i
                  b=2*b_i
                  c=2*c_i
                  d=2*d_i
                  
                  f12 = 0
                  f23 = 0
                  f31 = 0
                  
                  if(1-dabs(nx).gt.tol) then
                     f12 = f12 + (a+d*cv12(1))**2
                     f23 = f23 + (a+d*cv23(1))**2
                     f31 = f31 + (a+d*cv31(1))**2
                  endif
                  if(1-dabs(ny).gt.tol) then
                     f12 = f12 + (b+d*cv12(2))**2
                     f23 = f23 + (b+d*cv23(2))**2
                     f31 = f31 + (b+d*cv31(2))**2
                  endif
                  if(1-dabs(nz).gt.tol) then
                     f12 = f12 + (c+d*cv12(3))**2
                     f23 = f23 + (c+d*cv23(3))**2
                     f31 = f31 + (c+d*cv31(3))**2
                  endif
               endif
            endif
         endif

! 666     continue

         ! INTERIOR
         if(vecino.gt.0)then
            a_o=componentes(1,vecino)
            b_o=componentes(2,vecino)
            c_o=componentes(3,vecino)
            d_o=componentes(4,vecino)

            a=a_i-a_o
            b=b_i-b_o
            c=c_i-c_o
            d=d_i-d_o

            f12=(a+d*cv12(1))**2+(b+d*cv12(2))**2+(c+d*cv12(3))**2
            f23=(a+d*cv23(1))**2+(b+d*cv23(2))**2+(c+d*cv23(3))**2
            f31=(a+d*cv31(1))**2+(b+d*cv31(2))**2+(c+d*cv31(3))**2

         endif

         if (contacto.eq.1) then
            aux = aux + area * aux2 * lmax
         else
            aux = aux + area * (f12+f23+f31) / 3 * lmax
         endif
         
 10   continue

      cal_jlf = aux / 2

      return
      end
