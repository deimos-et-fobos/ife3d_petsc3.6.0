c	RETURN:
c
c	ncacof 		--> n de cara de contacto en el fluido
c	icaco(ncacof) 	--> n de nodo de contacto
c	icref(ncacof) 	--> referencia de contacto de esa cara
c	iverco(3,ncacof)   --> vertices iniciales y finales de la cara
c	xa(ncacof) 	      --> area de la cara
c
      subroutine puntar(z,mm,nn,nel,nrc,nrcof,irefcf,ncacof,ncacofp,
     &           icaco,iverco,icref,xa,nrcop,nrcos,irefcp,irefcs)
      implicit double precision (a-h,o-z)
      dimension nn(4,*),mm(4,*),nrc(4,*),z(3,*),irefcf(*),icaco(*),
     &          iverco(3,*),icref(*),xa(*),A(4,4),U(3),V(3),W(3),
     &          irefcp(*),irefcs(*),iflag_pla(1000),iflag_sol(1000)
     &           , area(4)

      ncacof=0
      ncacofp=0
      do 1 i=1,1000
        iflag_pla(i)=0
        iflag_sol(i)=0
 1    continue

      n1=0;n2=0;n3=0;

      area(1)=0
      area(2)=0
      area(3)=0
      area(4)=0

      do 2 ir=1,nrcof
        do 3 irp=1,nrcop
          if(irefcf(ir).eq.irefcp(irp)) iflag_pla(ir)=1
 3      continue
        do 4 irs=1,nrcos
          if(irefcf(ir).eq.irefcs(irs)) then
            if(iflag_pla(ir).eq.0) iflag_sol(ir)=1
          endif
 4      continue 
 2    continue

      do 11 k=1,nel
       do 12 j=1,4
        if(nrc(j,k).ne.0) then
         if (nrcof.gt.0) then
           do 13 ir=1,nrcof
             if(iflag_pla(ir).eq.1)then
               if(nrc(j,k).eq.irefcf(ir)) then
                  ncacof=ncacof+1
                  icaco(ncacof)=nn(j,k)
                  icref(ncacof)=nrc(j,k)
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
                  iverco(1,ncacof)=n1
                  iverco(2,ncacof)=n2
                  iverco(3,ncacof)=n3
                  do 14 i=1,3
                     U(i)=z(i,n2)-z(i,n1)
                     V(i)=z(i,n3)-z(i,n1)
 14               continue
                  do 15 i=1,3
                     W(i)=U(i)*V(1+mod(i,3))-U(1+mod(i,3))*V(i)
 15               continue
                  xa(ncacof)=dsqrt(W(1)**2+W(2)**2+W(3)**2)/2
!                  write(911,*)ncacof,icaco(ncacof),icref(ncacof),n1,n2,n3,
!     &                        xa(ncacof),nrc(j,k),irefcf(ir)
               endif
             endif
 13        continue
         end if
        end if
 12    continue
 11   continue
      ncacofp=ncacof

      do 21 k=1,nel
       do 22 j=1,4
        if(nrc(j,k).ne.0) then
         if (nrcof.gt.0) then
           do 23 ir=1,nrcof
             if(iflag_sol(ir).eq.1)then
               if(nrc(j,k).eq.irefcf(ir)) then
                  ncacof=ncacof+1
                  icaco(ncacof)=nn(j,k)
                  icref(ncacof)=nrc(j,k)
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
                  iverco(1,ncacof)=n1
                  iverco(2,ncacof)=n2
                  iverco(3,ncacof)=n3
                  do 24 i=1,3
                     U(i)=z(i,n2)-z(i,n1)
                     V(i)=z(i,n3)-z(i,n1)
 24               continue
                  do 25 i=1,3
                     W(i)=U(i)*V(1+mod(i,3))-U(1+mod(i,3))*V(i)
 25               continue
                  xa(ncacof)=dsqrt(W(1)**2+W(2)**2+W(3)**2)/2
                  area(irefcf(ir)-150)=area(irefcf(ir)-150)+xa(ncacof)
!                  write(911,*)ncacof,icaco(ncacof),icref(ncacof),n1,n2,n3,
!     &                        xa(ncacof),nrc(j,k),irefcf(ir)
               endif
             endif
 23        continue
         end if
        end if
 22    continue
 21   continue

!      write(2882,*) (area(i),i=1,4)
!      close(2882)
      return
      end
