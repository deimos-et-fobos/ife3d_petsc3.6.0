*
*     Identificacion de superficies
*
      program surface_id

      IMPLICIT double precision (a-h,o-z)

      integer AllocateStatus, DeallocateStatus, all_surfaces, 
     &        plane_surfaces, rest_surfaces, sur_gr_rest, el_ngrs
      integer,dimension(:),allocatable :: sur_gr, nel_gr
      integer,dimension(:,:),allocatable :: mmsur_n
      double precision,dimension(:,:),allocatable :: z_n, bar_n, pl_ 
      character*80 SUR_NEW,SAVESUR,dummy,fopen,cfg_file
      character*80,dimension(:),allocatable :: sur_type
      tol=1.d-12

*********************************************************************
*     Lee el archivo de configuracion
*********************************************************************
      write(6,'(a)') 'Reading surface_id.cfg...'
      cfg_file = 'surface_id.cfg'
      include 'cfgreading.f'
      el_ngrs = maxval(sur_gr)
      allocate(nel_gr(el_ngrs), stat=AllocateStatus)
      if(AllocateStatus /=0) Stop '---> Allocating error: nel_gr'
      do 1000 i=1,el_ngrs
        nel_gr(i)=0
 1000   continue

*********************************************************************
*     Abrir *.sur creado por SAVESUR (solo 1 grupo de elementos)
*********************************************************************
      fopen=SAVESUR
      open(1,file=fopen,status='old',err=10)
      write(6,'(a)') 'Reading '//trim(fopen)//'...'

*********************************************************************
*     Lee coordenadas y conectividad de la malla nueva
*********************************************************************
      call searstr(1,'COORDINATE')
      read(1,*) nver_n
      allocate(z_n(3,nver_n), stat=AllocateStatus)
      if(AllocateStatus /=0) Stop '---> Allocating error: z_n'
      do 1010 i=1,nver_n
        read(1,*) inutil,z_n(1,i),z_n(2,i),z_n(3,i)
 1010 continue

      call searstr(1,'ELEMENT_GROUP')
      read(1,*) dummy
      read(1,*) inutil,nel_n
      allocate(mmsur_n(4,nel_n), stat=AllocateStatus)
      if(AllocateStatus /=0) Stop '---> Allocating error: mmsur_n'
      call searstr(1,'INCIDENCE')
      read(1,*) dummy
      do 1011 i=1,nel_n
        read(1,*) mmsur_n(1,i),mmsur_n(2,i),mmsur_n(3,i)
        mmsur_n(4,i)=0
 1011 continue
      close(1)
 
*********************************************************************
*     Calculo de los baricentros de las caras
*********************************************************************
      write(6,'(a)') 'Surfaces identification...'
      allocate(bar_n(3,nel_n), stat=AllocateStatus)
      if(AllocateStatus /=0) Stop '---> Allocating error: bar_n'
      do 2000 i0=1,nel_n
         do 2001 j0=1,3
            bar_n(j0,i0)=0
            do 2002 k0=1,3
               bar_n(j0,i0)=bar_n(j0,i0)+z_n(j0,mmsur_n(k0,i0))
 2002       continue
            bar_n(j0,i0)=bar_n(j0,i0)/3.0
 2001    continue
 2000 continue

*********************************************************************
*     ALL_SURFACES
*********************************************************************
      if(all_surfaces.eq.1)then
        write(6,'(a)') ' ALL_SURFACES'
        nel_gr(1)=nel_n
        do 2100 i0=1,nel_n
          mmsur_n(4,i0)=1
 2100   continue
      endif

*********************************************************************
*     PLANE_SURFACES
*********************************************************************
      if(plane_surfaces.gt.0)then
        write(6,'(a)') ' PLANE_SURFACES'
        do 2200 j0=1,plane_surfaces
          do 2201 i0=1,nel_n
            if(mmsur_n(4,i0).eq.0)then
              aux=bar_n(1,i0)*pl_(1,j0)+bar_n(2,i0)*pl_(2,j0)+
     &            bar_n(3,i0)*pl_(3,j0)-pl_(4,j0)
              if(abs(aux).lt.tol)then
                mmsur_n(4,i0)=sur_gr(j0)
                nel_gr(sur_gr(j0))=nel_gr(sur_gr(j0))+1
              endif
            endif
 2201     continue
 2200   continue
      endif

*********************************************************************
*     REST_SURFACES
*********************************************************************
      if(rest_surfaces.eq.1)then
        write(6,'(a)') ' REST_SURFACES'
        do 2300 i0=1,nel_n
          if(mmsur_n(4,i0).eq.0)then
            mmsur_n(4,i0)=sur_gr(nsurfaces)
            nel_gr(sur_gr(nsurfaces))=nel_gr(sur_gr(nsurfaces))+1 
          endif
 2300   continue
      endif

*********************************************************************
*     Guarda el archivo *.sur nuevo
*********************************************************************
      fopen=SUR_NEW
      open(3,file=fopen,status='unknown',err=10)
      write(6,'(a)') 'Writing '//trim(fopen)//'...'
      
      write(3,'(a)') '*COORDINATES' 
      write(3,*) nver_n
      do 3000 i=1,nver_n
        write(3,*) i,(z_n(j,i),j=1,3)
 3000 continue
      write(3,*) 

      write(3,'(a)') '*ELEMENT_GROUPS'
      write(3,*) el_ngrs
      do 3001 i=1,el_ngrs      
        write(3,123) i,nel_gr(i),'Tri3'
 3001 continue
      write(3,*) 
 123  format(1x,i3,1x,i9,1x,a4)

      write(3,'(a)') '*INCIDENCES' 
      write(3,'(a)') '<NONE>'
      do 3002 i=1,el_ngrs
        do 3003 j=1,nel_n
          if(mmsur_n(4,j).eq.i) write(3,*) (mmsur_n(k,j),k=1,3)
 3003   continue
 3002 continue
      write(3,'(a)') '*END'
      close(3)

*********************************************************************
*     Deallocate
*********************************************************************
      deallocate(sur_gr,stat=DeallocateStatus)
      deallocate(nel_gr,stat=DeallocateStatus)
      deallocate(mmsur_n,stat=DeallocateStatus)
      deallocate(z_n,stat=DeallocateStatus)
      deallocate(bar_n,stat=DeallocateStatus)
      deallocate(pl_,stat=DeallocateStatus)
      deallocate(sur_type,stat=DeallocateStatus)

      return

 10   write(6,*) '---> Error opening ' // trim(fopen)
      STOP
      end
