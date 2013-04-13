      subroutine onepart(e0, tfin, dt, Nbins, bins, eI)
        implicit none
          ! The file handle to write output to
          !integer fhandle

          ! How many particles we allocate space for
          integer Nspace, Nbins
          parameter(Nspace=10000)
          
          ! Some parameters for simulation
          integer i, head, colls_dt, bins(Nbins)
          real*8 e0, t, tfin, dt, dtp, eI
          real*8 es(Nspace), tcs(Nspace)
         
C         Initialize simulation
          t = 0
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
          enddo
 
          do i=1, head
            bins(int(es(i)*e0/float(Nbins))) = bins(int(es(i))) + 1
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

