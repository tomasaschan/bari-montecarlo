      program histogram
        implicit none

        ! How many particles we allocate space for
        integer Nspace
        parameter(Nspace=10)
        
        ! Auxiliary functions
        real*8 collision_time
        
        ! Some parameters for simulation
        integer i, head, colls_dt
        real*8 E0, t, tfin, dt, dtp
        real*8 es(Nspace), tcs(Nspace)
        
        call srand(0)
        
        E0 = 300        ! eV
        tfin = 1e-9     ! s
        dt = 1e-11      ! s
        t = 0
        head = 1
        
        call init_arrays(Nspace, es, E0, tcs)
        
        do while (t .LT. tfin)
          colls_dt = 0
          
          do i=1, head
            dtp = dt
            
            do while (tcs(i) .LT. dtp)
              dtp = dtp - tcs(i)
              colls_dt = colls_dt + 1
              call handle_collision(Nspace, es, tcs, i, head+colls_dt)
            enddo
            
            tcs(i) = tcs(i) - dtp
          enddo
        
          head = head + colls_dt
          write (*,910) t*1E9, es(1:Nspace), tcs(1)*1E9
910     format((F6.2),10(2X,(F5.1)),2X(F10.4))          
          t = t+dt
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

      subroutine handle_collision(N, es, tcs, idx1, idx2)
        implicit none
          integer N, idx1, idx2
          real*8 es(N), tcs(N), collision_time
          
          es(idx1) = es(idx1) / 2
          es(idx2) = es(idx1)
          tcs(idx1) = collision_time(es(idx1))
          tcs(idx2) = collision_time(es(idx2))
      end
      
