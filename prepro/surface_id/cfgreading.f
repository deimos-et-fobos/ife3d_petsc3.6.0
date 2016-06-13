      open(1,file=cfg_file,status='old')
      open(16,file='cfgread.msg',status='unknown')

      if(ifindkey(1,'SUR_MESH_SAVESUR') .ne. 0) then
        read(1,'(A80)') SAVESUR
      else
         write(6,'(A)') '---> Error: '//trim(cfg_file)//
     &                  ': SUR_MESH_SAVESUR not defined.'
         write(16,'(A)') '---> Error: '//trim(cfg_file)//
     &                   ': SUR_MESH_SAVESUR not defined.'
         stop
      endif
      write(16,'(A)') '*SUR_MESH_SAVESUR'
      write(16,'(A80)') SAVESUR
      write(16,*)
      
      if(ifindkey(1,'SUR_MESH_NEW') .ne. 0) then
        read(1,'(A80)') SUR_NEW
      else
        SUR_NEW=trim(SAVESUR)//'_NEW'
      endif
      write(16,'(A)') '*SUR_MESH_NEW'
      write(16,'(A80)') SUR_NEW
      write(16,*)
      
      nsurfaces=0
      all_surfaces=0
      rest_surfaces=0
      plane_surfaces=0
      if(ifindkey(1,'ALL_SURFACES').ne.0) all_surfaces=1
      if(all_surfaces.eq.1)then
        nsurfaces=all_surfaces
      else
        if(ifindkey(1,'NUMBER_OF_PLANE_SURFACES').ne.0) then
          read(1,*) plane_surfaces
        endif
        if(ifindkey(1,'REST_SURFACES').ne.0) then
          read(1,*) sur_gr_rest
          rest_surfaces=1
        endif
        nsurfaces=plane_surfaces+rest_surfaces
      endif

      if(nsurfaces.ne.0)then
        allocate(sur_gr(nsurfaces), stat=AllocateStatus)
        if(AllocateStatus /=0) Stop '---> Allocating error: sur_gr'
        allocate(sur_type(nsurfaces), stat=AllocateStatus)
        if(AllocateStatus /=0) Stop '---> Allocating error: sur_type'
      endif

      if(all_surfaces.eq.1) then
        sur_gr(1)=1
        sur_type(1)='ALL' 
        write(16,'(A)') '*ALL_SURFACES'
        write(16,*)
      endif
      if(plane_surfaces.ne.0)then
        allocate(pl_(4,plane_surfaces), stat=AllocateStatus)
        if(AllocateStatus /=0) Stop '---> Allocating error: pl_'
        write(16,'(A)') '*NUMBER_OF_PLANE_SURFACES'
        write(16,'(I3)') plane_surfaces
        write(16,*)
        if(ifindkey(1,'PLANE_SURFACES').ne.0)then
          write(16,'(A)') '*PLANE_SURFACES'
          do 100 i=1,plane_surfaces
            read(1,*) sur_gr(i),pl_(1,i),pl_(2,i),pl_(3,i),pl_(4,i)
            sur_type(i)='PLANE'
            write(16,*) sur_gr(i),(pl_(j,i),j=1,4)
 100      continue
          write(16,*)
        else
          write(6,'(A)') '---> Error: '//trim(cfg_file)//
     &                  ': PLANE_SURFACES not defined.'
          write(16,'(A)') '---> Error: '//trim(cfg_file)//
     &                  ': PLANE_SURFACES not defined.'
          stop
        endif
      endif
      if(rest_surfaces.eq.1) then
        sur_gr(nsurfaces)=sur_gr_rest
        sur_type(nsurfaces)='REST' 
        write(16,'(A)') '*REST_SURFACES'
        write(16,'(I3)') sur_gr_rest
        write(16,*)
      endif

      close(1)
      close(16)
      write(6,'(A)') 'Writing cfgread.msg...' 
