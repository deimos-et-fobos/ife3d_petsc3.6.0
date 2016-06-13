***********************************************************************
****     subroutina para calcular despl. y pres. por vertices     *****
***********************************************************************
      subroutine depre(uf,nel,nnod,nver,z,ng,mm,nn,nsd,u,p,iver,div,
     &                 ioprom,ndsd,isd,dens,velsq,vol)
      implicit double precision (a-h,o-z)
      dimension uf(*),z(3,*),ng(4,*),mm(4,*),nn(4,*),nsd(*),u(3,*),p(*),
     &          div(*),isd(*),dens(*),velsq(*),vol(*),area(4),
     &          bar(4),q(3),xn(3),uv(3),vv(3),z1(3),z2(3),z3(3),z4(3)
      integer cn(4,4)
      double precision norma,iver(*)
      
      cn(1,1)=2; cn(1,2)=3; cn(1,3)=4; cn(1,4)=1
      cn(2,1)=1; cn(2,2)=4; cn(2,3)=3; cn(2,4)=2
      cn(3,1)=1; cn(3,2)=4; cn(3,3)=2; cn(3,4)=3
      cn(4,1)=1; cn(4,2)=2; cn(4,3)=3; cn(4,4)=4

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     Inicializacion del iver, p, u 
     
      do 10 i=1,nver
         iver(i)=0
         p(i)=0.d0
         do 11 j=1,3
            u(j,i)=0.d0
 11      continue
 10   continue

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     Loop en los elementos del fluido 

      do 20 k=1,nel
                      
         coefp=1
         do 21 i=1,ndsd
            if(isd(i).eq.nsd(k)) coefp=-dens(i)*velsq(i)*velsq(i)
 21      continue
                  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc        
         ! Calcula el area de las 4 caras del tetraedro k
         ! La cara i es la opuesta a mm(i,k)
             
         do 22 i=1,4
            do 23 l=1,3
               uv(l)=z(l,mm(cn(i,2),k))-z(l,mm(cn(i,1),k))
               vv(l)=z(l,mm(cn(i,3),k))-z(l,mm(cn(i,1),k))
 23         continue   
         area(i)=dabs(dsqrt((uv(2)*vv(3)-uv(3)*vv(2))**2+(uv(3)*vv(1)-
     &             uv(1)*vv(3))**2+(uv(1)*vv(2)-uv(2)*vv(1))**2))/2.d0
 22      continue
         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc         
!     Calcula la divergencia del elemento         
         call diver(vol(k),area,ng(1,k),uf(nn(1,k)),
     &              uf(nn(2,k)),uf(nn(3,k)),uf(nn(4,k)),div(k))

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
         do 50 i=1,3
            z1(i)=z(i,mm(1,k))
            z2(i)=z(i,mm(2,k))
            z3(i)=z(i,mm(3,k))
            z4(i)=z(i,mm(4,k))
 50      continue
 
         do 24 j=1,4
         
            p(mm(j,k))=p(mm(j,k))+coefp*div(k)
            iver(mm(j,k))=iver(mm(j,k))+1

!            if(ioprom.eq.1) then         

               call valor(vol(k),area,j,z1,z2,z3,z4,ng(1,k),
     &            uf(nn(1,k)),uf(nn(2,k)),uf(nn(3,k)),uf(nn(4,k)),q)

               do 25 i=1,3
                  u(i,mm(j,k))= u(i,mm(j,k))+q(i)
 25            continue

!            else

!               do 26 l=1,3
!                  uv(l)=z(l,mm(cn(j,2),k))-z(l,mm(cn(j,1),k))
!                  vv(l)=z(l,mm(cn(j,3),k))-z(l,mm(cn(j,1),k))
! 26            continue  
 
!               xn(1)=uv(2)*vv(3)-uv(3)*vv(2)
!               xn(2)=uv(3)*vv(1)-uv(1)*vv(3)
!               xn(3)=uv(1)*vv(2)-uv(2)*vv(1)
!               norma=sqrt(xn(1)**2+xn(2)**2+xn(3)**2)

!               do 30 i=1,3
!                  q(i)=uf(nn(j,k))*xn(i)*ng(j,k)/norma
! 30            continue

!               do 31 i=1,3
!                  do 32 l=1,3
!                     u(i,mm(cn(j,l),k))=u(i,mm(cn(j,l),k))+q(i)
! 32               continue                     
! 31            continue
 
!            end if
 24      continue
 20   continue
 
      do 40 i=1,nver
         p(i)=p(i)/iver(i)
         do 41 j=1,3
!            if(ioprom.eq.1) then
               u(j,i)=u(j,i)/iver(i)
!            else
!               u(j,i)=u(j,i)/iver(i)*0.5d0
!            end if
 41      continue
 40   continue
  
      return
      end
