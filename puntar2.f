c	RETURN:
c
c	ncaco 		--> n de cara de contacto en el fluido
c	icaco(ncaco) 	--> n de nodo de contacto
c	icref(ncaco) 	--> referencia de contacto de esa cara
c	iverco(3,ncaco)   --> vertices iniciales y finales de la cara
c	xa(ncaco) 	      --> area de la cara
c
      subroutine puntar(z,mm,nn,nel,nrc,nrco,irefco,ncaco,icaco,iverco,
     &                  icref,xa)
      implicit double precision (a-h,o-z)
      dimension nn(4,*),mm(4,*),nrc(4,*),z(3,*),irefco(*),icaco(*),
     &          iverco(3,*),icref(*),xa(*),A(4,4),U(3),V(3),W(3)

      ncaco=0
      do 1 k=1,nel
      do 2 j=1,4
      if(nrc(j,k).ne.0) then
         if (nrco.gt.0) then
            do 3 ir=1,nrco
               if(nrc(j,k).eq.irefco(ir)) then
                  ncaco=ncaco+1
                  icaco(ncaco)=nn(j,k)
                  icref(ncaco)=nrc(j,k)
                  if (j.eq.1) then
                     n1=mm(2,k); n2=mm(3,k); n3=mm(4,k)
                  end if
                  if (j.eq.2) then
                     n1=mm(1,k); n2=mm(3,k); n3=mm(4,k)
                  end if
                  if (j.eq.3) then
                     n1=mm(1,k); n2=mm(2,k); n3=mm(4,k)
                  end if
                  if (j.eq.4) then
                     n1=mm(1,k); n2=mm(2,k); n3=mm(3,k)
                  end if
                  iverco(1,ncaco)=n1
                  iverco(2,ncaco)=n2
                  iverco(3,ncaco)=n3
                  do 4 i=1,3
                     U(i)=z(i,n2)-z(i,n1)
                     V(i)=z(i,n3)-z(i,n1)
  4               continue
                  do 5 i=1,3
                     W(i)=U(i)*V(1+mod(i,3))-U(1+mod(i,3))*V(i)
  5               continue
                  xa(ncaco)=dsqrt(W(1)**2+W(2)**2+W(3)**2)/2
!                  write(911,*)ncaco,icaco(ncaco),icref(ncaco),n1,n2,n3,
!     &                        xa(ncaco),nrc(j,k),irefco(ir)
               endif
  3         continue
         end if
      end if
  2   continue
  1   continue
      return
      end
