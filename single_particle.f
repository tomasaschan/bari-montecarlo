      subroutine onepart(e0, tfin, dt, Nbins, Ntimes, bins, eI)
        implicit none
          ! The file handle to write output to
          !integer fhandle

          ! How many particles we allocate space for
          integer Nspace, Nbins, Ntimes
          parameter(Nspace=100)
          
          ! Some parameters for simulation
          integer i, head, colls_dt, bins(Nbins, Ntimes), idx, tidx
          real*8 e0, t, tfin, dt, dtp, eI
          real*8 es(Nspace), tcs(Nspace)
         
C         Initialize simulation
          t = 0
          tidx = 1
          head = 1
          call init_arrays(Nspace, es, e0, tcs)
          
C         Run simulation of one electron
          do while (t .LE. tfin)
            colls_dt = 0
            
            do i=1, head
              dtp = dt
              do while(tcs(i) .LE. dtp .AND. tcs(i) .GT. 0)
                dtp = dtp - tcs(i)
                colls_dt = colls_dt + 1
                call handle_collision(Nspace,es,tcs,i,head+colls_dt,eI)
              enddo
              
              tcs(i) = tcs(i) - dtp
            enddo
          
            head = head + colls_dt
            t = t+dt

            do i=1, head
              idx = ceiling(es(i)*(Nbins-1)/e0)
              bins(idx, tidx) = bins(idx, tidx) + 1 
            enddo
            tidx = tidx + 1

          enddo
 
          do i=1, head
            idx = int(es(i)*e0/float(Nbins))
            bins(idx, Ntimes) = bins(idx, Ntimes) + 1
          enddo
      end

      subroutine init_arrays(N, es, e0, tcs)
        implicit none
        
        integer N,i
        real*8 es(N), tcs(N), e0, collision_time
        
        es(1) = e0
        tcs(1) = collision_time(e0)
        do i=2, N-1
          es(i) = 0
          tcs(i) = 0
        enddo
      end

