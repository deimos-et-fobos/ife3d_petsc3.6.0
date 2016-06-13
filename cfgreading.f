C
      OPEN  (1, FILE=FILEGD, STATUS='OLD')
      OPEN (16,FILE='cfgread.msg',STATUS='UNKNOWN')

      IF (IFINDKEY (1, 'TITLE') .NE. 0) THEN
         READ  (1,'(A80)') TITLE
      ELSE
         TITLE = 'Untitled ife3d problem'
      endif
      WRITE (16,'(A)') '*TITLE'
      WRITE (16,'(A80)') TITLE
C
      write_matrix=0
      IF(IFINDKEY(1,'WRITE MATRICES').NE. 0) THEN
         write_matrix=1
         WRITE (16,'(A)') '---> WARNING: Writing in matM & matK'
         WRITE ( 6,'(A)') '---> WARNING: Writing in matM & matK'
      ENDIF
C
      read_matrix=0
      IF(IFINDKEY(1,'READ MATRICES').NE. 0) THEN
        if(write_matrix.eq.1)then
          WRITE (16,'(2A)') '---> WARNING: WRITE and READ ',
     &                 'MATRICES command given'
          WRITE ( 6,'(2A)') '---> WARNING: WRITE and READ ',
     &                 'MATRICES command given'
          WRITE (16,'(A)') '---> WARNING: Computing new matrices'
          WRITE (6,'(A)')  '---> WARNING: Computing new matrices'
        else
          read_matrix=1
          WRITE (16,'(A)')'---> WARNING: Reading from matK & matM'
          WRITE ( 6,'(A)')'---> WARNING: Reading from matK & matM'
        endif
      ENDIF
C
C
C
      IF (IFINDKEY (1, 'PLATE') .NE. 0) THEN
         READ (1, *) indpla
         WRITE (16,'(/,A)') '*PLATE'
         write (16,'(I1)') indpla
      ELSE
         indpla = 0
      endif
C
      IF (IFINDKEY (1, 'SOLID') .NE. 0) THEN
         READ (1, *) indsol
         WRITE (16,'(A)') '*SOLID'
         write (16,'(I1)') indsol
      ELSE
         indsol = 0
      endif
C
      IF (IFINDKEY (1, 'FLUID') .NE. 0) THEN
         READ (1, *) indflu
         WRITE (16,'(A)') '*FLUID'
         write (16,'(I1)') indpla
      ELSE
         indflu = 0
      endif
C
C
C
      IF (indpla.ne.0) then
      IF (IFINDKEY (1, 'PLATE_MESH') .NE. 0) THEN
         READ (1, '(A80)') mallap
         WRITE (16,'(/,A)') '*PLATE_MESH'
         write (16,'(A80)') mallap
      ELSE
         if(indpla.ne.0) mallap = 'placa.rt'
      endif
C
      IF(IFINDKEY(1,'NUMBER_OF_PLATE_CONTACT_REFERENCES').NE. 0) THEN
         READ (1, *) nrcop
         WRITE (16,'(A)') '*NUMBER_OF_PLATE_CONTACT_REFERENCES'
         write (16,'(I2)') nrcop
      ELSE
         nrcop = 0
      endif
C
      if(nrcop.gt.0)then
        IF(IFINDKEY(1,'PLATE_CONTACT_REFERENCES').NE. 0) THEN
          READ (1,*) (irefcp(i),i=1,nrcop)
          WRITE (16,'(A)') '*PLATE_CONTACT_REFERENCES'
          write (16,'(10(I3,1x))') (irefcp(i),i=1,nrcop)
        ELSE
          nrcop = 0
          WRITE (16,'(A)') '*PLATE_CONTACT_REFERENCES'
          WRITE (16,'(2A)') '---> WARNING: No plate contact ',
     &                      'references given'
          WRITE (6,'(2A)')  '---> WARNING: No plate contact ',
     &                      'references given'
        ENDIF
      endif
C
      IF(IFINDKEY(1,'NUMBER_OF_PLATE_DOMAINS').NE. 0) THEN
         READ (1, *) ndsdp
         WRITE (16,'(A)') '*NUMBER_OF_PLATE_DOMAINS'
         write (16,'(I2)') ndsdp
      ELSE
         if(indpla.ne.0) then
            ndsdp = 1
            WRITE (16,'(A)') '*NUMBER_OF_PLATE_DOMAINS'
            write (16,'(I2)') ndsdp
         else
            ndsdp = 0
         endif
      ENDIF
C
      IF(IFINDKEY(1,'PLATE_DOMAINS_PROPERTIES').NE. 0) THEN
        WRITE (16,'(2A)') '*PLATE_DOMAINS_PROPERTIES ',
     &                    '(Domain/Young/Poisson/Kcorr/Density)'
        do 310 i=1,ndsdp
          read(1,*) isdp(i),youngp(i),poisp(i),corrkp(i),densp(i)
          write(16,'(I2,2x,E9.3,2x,F4.2,2x,F5.3,2x,E9.3)') isdp(i),
     &      youngp(i),poisp(i),corrkp(i),densp(i)
 310    continue
      ELSE
        if(indpla.ne.0) then
          WRITE (16,'(2A)') '*PLATE_DOMAINS_PROPERTIES ',
     &                      '(Domain/Young/Poisson/Kcorr/Density)'
          WRITE (16,'(2A)') '---> WARNING: No plate domains ',
     &                      'properties given'
          WRITE (6,'(2A)')  '---> WARNING: No plate domains ',
     &                      'properties given'
          WRITE (16,'(2A)') '---> WARNING: No taking in account ',
     &                      'plate domains'
          WRITE (6,'(2A)')  '---> WARNING: No taking in account ',
     &                      'plate domains'
          indpla = 0
        endif
        ndsdp = 0
      ENDIF
      endif
C
C
C
      IF (indsol.ne.0) then
      IF (IFINDKEY (1, 'SOLID_MESH') .NE. 0) THEN
         READ (1, '(A80)') mallas
         WRITE (16,'(/,A)') '*SOLID_MESH'
         write (16,'(A80)') mallas
      ELSE
         if(indsol.ne.0) mallas = 'solido.rt'
      endif
C
      IF(IFINDKEY(1,'NUMBER_OF_SOLID_CONTACT_REFERENCES').NE. 0) THEN
         READ (1, *) nrcos
         WRITE (16,'(A)') '*NUMBER_OF_SOLID_CONTACT_REFERENCES'
         write (16,'(I2)') nrcos
      ELSE
         nrcos = 0
      endif
C
      if(nrcos.gt.0)then
        IF(IFINDKEY(1,'SOLID_CONTACT_REFERENCES').NE. 0) THEN
          READ (1, *) (irefcs(i),i=1,nrcos)
          WRITE (16,'(A)') '*SOLID_CONTACT_REFERENCES'
          write (16,'(10(I3,1x))') (irefcs(i),i=1,nrcos)
        ELSE
          nrcos = 0
          WRITE (16,'(A)') '*SOLID_CONTACT_REFERENCES'
          WRITE (16,'(2A)') '---> WARNING: No solid contact ',
     &                      'references given'
          WRITE (6,'(2A)')  '---> WARNING: No solid contact ',
     &                      'references given'
        endif
      endif
C
      IF(IFINDKEY(1,'NUMBER_OF_SOLID_DOMAINS').NE. 0) THEN
         READ (1, *) ndsds
         WRITE (16,'(A)') '*NUMBER_OF_SOLID_DOMAINS'
         write (16,'(I2)') ndsds
      ELSE
         if(indsol.ne.0) then
            ndsds = 1
            WRITE (16,'(A)') '*NUMBER_OF_SOLID_DOMAINS'
            write (16,'(I2)') ndsds
         else
            ndsds = 0
         endif
      endif
C
      IF(IFINDKEY(1,'SOLID_DOMAINS_PROPERTIES').NE. 0) THEN
        WRITE (16,'(2A)') '*SOLID_DOMAINS_PROPERTIES ',
     &                    '(Domain/Young/Poisson/Density)'
        do 320 i=1,ndsds
          read(1,*) isds(i),youngs(i),poiss(i),denss(i)
          write(16,'(I2,2x,E9.3,2x,F4.2,2x,E9.3)') isds(i),youngs(i),
     &      poiss(i),denss(i)
 320    continue
      ELSE
        if(indsol.ne.0) then
          WRITE (16,'(2A)') '*SOLID_DOMAINS_PROPERTIES ',
     &                      '(Domain/Young/Poisson/Density)'
          WRITE (16,'(2A)') '---> WARNING: No solid domains ',
     &                      'properties given'
          WRITE (6,'(2A)')  '---> WARNING: No solid domains ',
     &                      'properties given'
          WRITE (16,'(2A)') '---> WARNING: No taking in account ',
     &                      'solid domains'
          WRITE (6,'(2A)')  '---> WARNING: No taking in account ',
     &                      'solid domains'
          indsol = 0
        endif
        ndsds = 0
      ENDIF
      endif
C
C
C
C
      IF (indflu.ne.0) then
C
      IF (IFINDKEY (1, 'FLUID_MESH') .NE. 0) THEN
         READ (1, '(A80)') mallaf
         WRITE (16,'(/,A)') '*FLUID_MESH'
         write (16,'(A80)') mallaf
      ELSE
         if(indflu.ne.0) mallaf = 'fluido.rt'
      endif
C
      IF(IFINDKEY(1,'NUMBER_OF_FLUID_CONTACT_REFERENCES').NE. 0) THEN
         READ (1, *) nrcof
         WRITE (16,'(A)') '*NUMBER_OF_FLUID_CONTACT_REFERENCES'
         write (16,'(I2)') nrcof
      ELSE
         nrcof = 0
      endif
C
      if(nrcof.gt.0)then
        IF(IFINDKEY(1,'FLUID_CONTACT_REFERENCES').NE. 0) THEN
          READ (1, *) (irefcf(i),i=1,nrcof)
          WRITE (16,'(A)') '*FLUID_CONTACT_REFERENCES'
          write (16,'(10(I3,1x))') (irefcf(i),i=1,nrcof)
        ELSE
          nrcof = 0
          WRITE (16,'(A)') '*FLUID_CONTACT_REFERENCES'
          WRITE (16,'(2A)') '---> WARNING: No fluid contact ',
     &                      'references given'
          WRITE (6,'(2A)')  '---> WARNING: No fluid contact ',
     &                      'references given'
        endif
      endif
C
      IF(IFINDKEY(1,'NUMBER_OF_FLUID_ROBIN_REFERENCES').NE. 0) THEN
         READ (1, *) nrrf
         WRITE (16,'(A)') '*NUMBER_OF_FLUID_ROBIN_REFERENCES'
         write (16,'(I2)') nrrf
      ELSE
         nrrf = 0
      endif
C
      if(nrrf.gt.0)then
         IF(IFINDKEY(1,'FLUID_ROBIN_REFERENCES').NE. 0) THEN
            WRITE(16,'(A)')'*FLUID_ROBIN_REFERENCES (Reference/RK/RM)'
            do 330 i=1,nrrf
               READ (1, *) irefrf(i),rk(i),rm(i)
               write (16,'(I2,2(2x,E9.3))') irefrf(i),rk(i),rm(i)
 330        continue
         ELSE
            nrrf = 0
            WRITE(16,'(A)')'*FLUID_ROBIN_REFERENCES (Reference/RK/RM)'
            WRITE (16,'(2A)') '---> WARNING: No fluid robin ',
     &                        'references given'
            WRITE (6,'(2A)')  '---> WARNING: No fluid robin ',
     &                        'references given'
         endif
      endif
C
      IF(IFINDKEY(1,'NUMBER_OF_FLUID_DOMAINS').NE. 0) THEN
         READ (1, *) ndsdf
         WRITE (16,'(A)') '*NUMBER_OF_FLUID_DOMAINS'
         write (16,'(I2)') ndsdf
      ELSE
         if(indflu.ne.0) then
            ndsdf = 1
            WRITE (16,'(A)') '*NUMBER_OF_FLUID_DOMAINS'
            write (16,'(I2)') ndsdf
         else
            ndsdf = 0
         endif
      endif
C
      IF(IFINDKEY(1,'FLUID_DOMAINS_PROPERTIES').NE. 0) THEN
        WRITE (16,'(2A)') '*FLUID_DOMAINS_PROPERTIES ',
     &                    '(Domain/Density/Speed of Sound)'
        do 331 i=1,ndsdf
          read(1,*) isdf(i),densf(i),velsq(i)
          write(16,'(I2,2(2x,E9.3))') isdf(i),densf(i),velsq(i)
 331    continue
      ELSE
        if(indflu.ne.0) then
          WRITE (16,'(2A)') '*FLUID_DOMAINS_PROPERTIES ',
     &                      '(Domain/Density/Speed of Sound)'
          WRITE (16,'(2A)') '---> WARNING: No fluid domains ',
     &                      'properties given'
          WRITE ( 6,'(2A)')  '---> WARNING: No fluid domains ',
     &                      'properties given'
          WRITE (16,'(2A)') '---> WARNING: No taking in account ',
     &                      'fluid domains'
          WRITE ( 6,'(2A)') '---> WARNING: No taking in account ',
     &                      'fluid domains'
          indflu = 0
        endif
        ndsdf = 0
      ENDIF
C      
      endif
C
C
C
      IF (indflu.ne.0 .and. indsol.ne.0) then
      IF(IFINDKEY(1,'WRITE SOLID/FLUID INTERFACE').NE. 0) THEN
         w_interfaz_fs=1
      ELSE
         w_interfaz_fs=0
      ENDIF
C
      IF(IFINDKEY(1,'READ SOLID/FLUID INTERFACE').NE. 0) THEN
        if(w_interfaz_fs.eq.1)then
          r_interfaz_fs=0
          WRITE(16,'(/,2A)')'---> WARNING: WRITE and READ SOLID/FLUID ',
     &                      'INTERFACE command given'
          WRITE (6,'(2A)')  '---> WARNING: WRITE and READ SOLID/FLUID ',
     &                      'INTERFACE command given'
          WRITE (16,'(A)')  '---> WARNING: Computing new interface'
          WRITE (6,'(A)')   '---> WARNING: Computing new interface'
        else
          r_interfaz_fs=1
          WRITE(16,'(/,A)')'---> WARNING: Reading from interface_fs.dat'
          WRITE (6,'(A)')  '---> WARNING: Reading from interface_fs.dat'
        endif
      ELSE
        r_interfaz_fs=0
        if(w_interfaz_fs.eq.1)then
          WRITE (16,'(/,A)') '---> WARNING: Writing in interface_fs.dat'
          WRITE (6,'(A)')  '---> WARNING: Writing in interface_fs.dat'
        endif
      ENDIF
C
      IF(IFINDKEY(1,'SOLID/FLUID_PROJECTION_TYPE').NE. 0) THEN
        READ (1, *) proj_sf
        WRITE(16,'(2A)')'*SOLID/FLUID_PROJECTION_TYPE (0 = Coplanar',
     &                    ' / 1 = solid-->fluid / 2 = fluid-->solid)'
        WRITE (16,'(I1)') proj_sf
      ELSE
        proj_sf = 0
        WRITE(16,'(2A)')'*SOLID/FLUID_PROJECTION_TYPE (0 = Coplanar',
     &                    ' / 1 = solid-->fluid / 2 = fluid-->solid)'
        WRITE (16,'(2A)') '---> WARNING: No solid/fluid projection ',
     &                    'type given'
        WRITE ( 6,'(2A)') '---> WARNING: No solid/fluid projection ',
     &                    'type given'
        WRITE (16,'(2A)') '---> WARNING: Assuming coplanar ',
     &                    'projection type'
        WRITE ( 6,'(2A)') '---> WARNING: Assuming coplanar ',
     &                    'projection type'
      endif
      endif
C
C
C
      IF (indflu.ne.0 .and. indpla.ne.0) then
      IF(IFINDKEY(1,'WRITE PLATE/FLUID INTERFACE').NE. 0) THEN
         w_interfaz_fp=1
      ELSE
         w_interfaz_fp=0
      ENDIF
C
      IF(IFINDKEY(1,'READ PLATE/FLUID INTERFACE').NE. 0) THEN
        if(w_interfaz_fp.eq.1)then
          r_interfaz_fp=0
          WRITE(16,'(/,2A)')'---> WARNING: WRITE and READ PLATE/FLUID ',
     &                      'INTERFACE command given'
          WRITE (6,'(2A)')  '---> WARNING: WRITE and READ PLATE/FLUID ',
     &                      'INTERFACE command given'
          WRITE (16,'(A)')  '---> WARNING: Computing new interface'
          WRITE (6,'(A)')   '---> WARNING: Computing new interface'
        else
          r_interfaz_fp=1
          WRITE(16,'(/,A)')'---> WARNING: Reading from interface_fp.dat'
          WRITE(6,'(A)')   '---> WARNING: Reading from interface_fp.dat'
        endif
      ELSE
        r_interfaz_fp=0
        if(w_interfaz_fp.eq.1)then
          WRITE (16,'(/,A)') '---> WARNING: Writing in interface_fp.dat'
          WRITE (6,'(A)')  '---> WARNING: Writing in interface_fp.dat'
        endif
      ENDIF
C
      IF(IFINDKEY(1,'PLATE/FLUID_PROJECTION_TYPE').NE. 0) THEN
        READ (1, *) proj_pf
        WRITE(16,'(2A)')'*PLATE/FLUID_PROJECTION_TYPE (0 = Coplanar',
     &                    ' / 1 = plate-->fluid / 2 = fluid-->plate)'
        WRITE (16,'(I1)') proj_pf
      ELSE
        proj_pf = 0
        WRITE(16,'(2A)')'*PLATE/FLUID_PROJECTION_TYPE (0 = Coplanar',
     &                    ' / 1 = plate-->fluid / 2 = fluid-->plate)'
        WRITE (16,'(2A)') '---> WARNING: No plate/fluid projection ',
     &                    'type given'
        WRITE ( 6,'(2A)') '---> WARNING: No plate/fluid projection ',
     &                    'type given'
        WRITE (16,'(2A)') '---> WARNING: Assuming coplanar ',
     &                    'projection type'
        WRITE ( 6,'(2A)') '---> WARNING: Assuming coplanar ',
     &                    'projection type'
      endif
      endif
C
C
C
      IF (indpla.ne.0 .and. indsol.ne.0) then
      IF(IFINDKEY(1,'WRITE PLATE/SOLID INTERFACE').NE. 0) THEN
         w_interfaz_sp=1
      ELSE
         w_interfaz_sp=0
      ENDIF
C
      IF(IFINDKEY(1,'READ PLATE/SOLID INTERFACE').NE. 0) THEN
        if(w_interfaz_sp.eq.1)then
          r_interfaz_sp=0
          WRITE(16,'(/,2A)')'---> WARNING: WRITE and READ PLATE/SOLID ',
     &                      'INTERFACE command given'
          WRITE( 6,'(2A)')  '---> WARNING: WRITE and READ PLATE/SOLID ',
     &                      'INTERFACE command given'
          WRITE (16,'(A)')  '---> WARNING: Computing new interface'
          WRITE (6,'(A)')   '---> WARNING: Computing new interface'
        else
          r_interfaz_sp=1
          WRITE(16,'(/,A)')'---> WARNING: Reading from interface_sp.dat'
          WRITE( 6,'(A)')  '---> WARNING: Reading from interface_sp.dat'
        endif
      ELSE
        r_interfaz_sp=0
        if(w_interfaz_sp.eq.1)then
          WRITE (16,'(/,A)') '---> WARNING: Writing in interface_sp.dat'
          WRITE (6,'(A)')  '---> WARNING: Writing in interface_sp.dat'
        endif
      ENDIF
C
      IF(IFINDKEY(1,'PLATE/SOLID_PROJECTION_TYPE').NE. 0) THEN
        READ (1, *) proj_sp
        WRITE(16,'(2A)')'*PLATE/SOLID_PROJECTION_TYPE (0 = Coplanar',
     &                    ' / 1 = plate-->solid / 2 = solid-->plate)'
        WRITE (16,'(I1)') proj_sp
      ELSE
        proj_sp = 0
        WRITE(16,'(2A)')'*PLATE/SOLID_PROJECTION_TYPE (0 = Coplanar',
     &                    ' / 1 = plate-->solid / 2 = solid-->plate)'
        WRITE (16,'(2A)') '---> WARNING: No plate/solid projection ',
     &                    'type given'
        WRITE ( 6,'(2A)') '---> WARNING: No plate/solid projection ',
     &                    'type given'
        WRITE (16,'(2A)') '---> WARNING: Assuming coplanar ',
     &                    'projection type'
        WRITE ( 6,'(2A)') '---> WARNING: Assuming coplanar ',
     &                    'projection type'
      endif
      endif
C
C
C
      IF (IFINDKEY (1, 'DOMAIN_DIVISION_PERMUTATION') .NE. 0) THEN
        READ (1, *) div_perm
      ELSE
        div_perm = 0
        ndiv_perm = 1 
      endif
      WRITE (16,'(/,A)') '*DOMAIN_DIVISION_PERMUTATION'
      WRITE (16,'(I1)') div_perm
C
      if(div_perm.eq.1)then
        metis = 0
        IF (IFINDKEY (1, 'NUMBER_OF_DIVISIONS') .NE. 0) THEN
          READ (1, *) ndiv_perm
        ELSE
          ndiv_perm = 1
        endif
        WRITE (16,'(A)') '*NUMBER_OF_DIVISIONS'
        WRITE (16,'(I4)') ndiv_perm
      else
        metis = 1   !Default
      endif
C
C
C
      if(IFINDKEY (1,'PERM_METIS').ne.0)then
        read(1,*) metis
        if(metis.eq.1 .and. div_perm.eq.1)then
          WRITE(16,'(/,A)')'---> WARNING: DOMAIN_DIVISION_'// 
     &                     'PERMUTATION and PERM_METIS commands given.'
          WRITE( 6,'(A)')  '---> WARNING: DOMAIN_DIVISION_'// 
     &                     'PERMUTATION and PERM_METIS commands given.'
          WRITE (16,'(A)') '---> WARNING: Using columns permutation'//
     &                     ' with METIS'
          WRITE ( 6,'(A)') '---> WARNING: Using columns permutation'//
     &                     ' with METIS'
          div_perm = 0
          ndiv_perm = 1
        endif
        WRITE (16,'(/,A)') '*PERM_METIS'
        WRITE (16,'(I1)') metis
      elseif(metis.eq.1)then
        WRITE (16,'(/,A)') '*PERM_METIS'
        WRITE (16,'(I1)') metis
      endif
C
      r_mgrafo = 0
      w_mgrafo = 0
      if(metis.eq.1)then
        if(IFINDKEY (1,'READ_METIS_GRAPH') .NE. 0) then
          read(1,*) r_mgrafo
          WRITE (16,'(A)') '*READ_METIS_GRAPH'
          WRITE (16,'(I1)') r_mgrafo
        endif
        if(IFINDKEY (1,'WRITE_METIS_GRAPH') .NE. 0) then
          read(1,*) w_mgrafo
          WRITE (16,'(A)') '*WRITE_METIS_GRAPH'
          WRITE (16,'(I1)') w_mgrafo
        endif
      endif
C
C
C
      r_ggrafo=0
      w_ggrafo=0
      if(IFINDKEY (1,'READ_GLOBAL_GRAPH') .NE. 0) then
        read(1,*) r_ggrafo
        WRITE (16,'(/,A)') '*READ_GLOBAL_GRAPH'
        WRITE (16,'(I1)') r_ggrafo
      endif
      if(IFINDKEY (1,'WRITE_GLOBAL_GRAPH') .NE. 0) then
        read(1,*) w_ggrafo
        WRITE (16,'(A)') '*WRITE_GLOBAL_GRAPH'
        WRITE (16,'(I1)') w_ggrafo
      endif
C
C
C
      IF (IFINDKEY (1, 'NUMBER_OF_EIGENVALUES') .NE. 0) THEN
         READ (1, *) nev
      ELSE
         nev = 1
      endif
      WRITE (16,'(/,A)') '*NUMBER_OF_EIGENVALUES'
      WRITE (16,'(I3)') nev
C
      IF (IFINDKEY (1, 'EIGENVALUES_SHIFT') .NE. 0) THEN
         READ (1,*) shift
      ELSE
         shift = 0
      endif
      WRITE (16,'(A)') '*EIGENVALUES_SHIFT'
      WRITE (16,'(E9.3)') shift
C
      IF (IFINDKEY (1, 'MAXIMUM_ITERATIONS') .NE. 0) THEN
         READ (1,*) maxit
      ELSE
         maxit = 1000
      endif
      WRITE (16,'(A)') '*MAXIMUM_ITERATIONS'
      WRITE (16,'(I7)') maxit
C
      IF (IFINDKEY (1, 'RELATIVE_ERROR_TOLERANCE') .NE. 0) THEN
         READ (1,*) tolerr
      ELSE
         tolerr = 1.d-6
      endif
      WRITE (16,'(A)') '*RELATIVE_ERROR_TOLERANCE'
      WRITE (16,'(E9.3)') tolerr
C
      IF (IFINDKEY (1, 'RESIDUAL_TOLERANCE') .NE. 0) THEN
         READ (1,*) tolres
      ELSE
         tolres = 1.d-6
      endif
      WRITE (16,'(A)') '*RESIDUAL_TOLERANCE'
      WRITE (16,'(E9.3)') tolres
C
      WRITE (16,'(/,A)') '*END'
C
      CLOSE (1)
      close(16)
