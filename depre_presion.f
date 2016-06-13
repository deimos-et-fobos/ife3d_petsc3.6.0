***********************************************************************
****     subroutina para calcular despl. por vertices             *****
***********************************************************************
      subroutine depre_presion(pf,nel,nver,z,mm,nsd,u,iver,
     &              ioprom,ndsd,isd,dens,velsq,vol,eig_r)
      implicit double precision (a-h,o-z)
      dimension pf(*),z(3,*),mm(4,*),nsd(*),u(3,*),isd(*),dens(*),
     &          velsq(*),vol(*),Dk(3,3),grad(3),adj(3,3)
      double precision iver(*),pe(4),InvDk(3,3)
      
      coefp=1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     Inicializacion del iver, u 
     
      do 10 i=1,nver
        iver(i)=0.d0
        do 11 j=1,3
          u(j,i)=0.d0
 11     continue
 10   continue

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     Loop en los elementos del fluido 

      do 20 k=1,nel
        
        pe(1)=pf(mm(1,k))    
        pe(2)=pf(mm(2,k))    
        pe(3)=pf(mm(3,k))    
        pe(4)=pf(mm(4,k))    
                      
        do 21 i=1,ndsd
          if(isd(i).eq.nsd(k)) coefp=dens(i)*eig_r
 21     continue
                  
        
        do 22 i=1,3
          do 23 j=1,3
            Dk(i,j)=z(j,mm(i+1,k))-z(j,mm(1,k))
 23       continue
 22     continue
     
        det=6*vol(k)
        do 24 i=1,3
          do 25 j=1,3
            adj(i,j)=Dk(1+mod(i,3),1+mod(j,3))*Dk(1+mod(i+1,3),
     &               1+mod(j+1,3))-Dk(1+mod(i,3),1+mod(j+1,3))*
     &               Dk(1+mod(i+1,3),1+mod(j,3))
            InvDk(j,i)=adj(i,j)/det
 25       continue
 24     continue
  
        call grad_presion(pe,InvDk,grad)
        do 26 i=1,3
          grad(i)=grad(i)*vol(k)/coefp
 26     continue
        
        do 30 j=1,4
          do 31 i=1,3
            u(i,mm(j,k))=u(i,mm(j,k))+grad(i)
 31       continue
          iver(mm(j,k))=iver(mm(j,k))+vol(k)
 30     continue
 20   continue
 
      do 40 i=1,nver
        do 41 j=1,3
          u(j,i)=u(j,i)/iver(i)
 41     continue
 40   continue
  
      return
      end
