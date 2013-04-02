      subroutine onepart(E0, tfin, dt, bins)
        implicit none
          ! The file handle to write output to
          !integer fhandle

          ! How many particles we allocate space for
          integer Nspace
          parameter(Nspace=100)
          
          ! Some parameters for simulation
          integer i, head, colls_dt, bins(300)
          real*8 E0, t, tfin, dt, dtp
          real*8 es(Nspace), tcs(Nspace)
         
          t = 0
          head = 1
          
          call init_arrays(Nspace, es, E0, tcs)
          
          do while (t .LE. tfin)
            colls_dt = 0
            
            do i=1, head
              dtp = dt
              
              do while(tcs(i) .LE. dtp 
     +           .AND.tcs(i).GT.0.AND.(es(i)-13.4).GT.0)
                dtp = dtp - tcs(i)
                colls_dt = colls_dt + 1
                
                call handle_collision(Nspace, es, tcs, i, head+colls_dt)
              enddo
              
              tcs(i) = tcs(i) - dtp
            enddo
          
            head = head + colls_dt

            t = t+dt
          enddo
 
          do i=1, head
            bins(int(es(i))) = bins(int(es(i))) + 1
          enddo
      end

      subroutine init_arrays(N, es, E0, tcs)
        implicit none
        
        integer N,i
        real*8 es(N), tcs(N), E0, collision_time
        
        es(1) = E0
        tcs(1) = collision_time(E0)
        do i=2, N-1
          es(i) = 0
          tcs(i) = 0
        enddo
      end

