      function onepart(E0, tfin, dt, fhandle)
        implicit none
          integer onepart
    
          ! The file handle to write output to
          integer fhandle

          ! How many particles we allocate space for
          integer Nspace
          parameter(Nspace=1000)
          
          ! Some parameters for simulation
          integer i, head, colls_dt
          real*8 E0, t, tfin, dt, dtp
          real*8 es(Nspace), tcs(Nspace)
          
          call srand(0)

          t = 0
          head = 1
          
          call init_arrays(Nspace, es, E0, tcs)
          
          do while (t .LT. tfin)
            colls_dt = 0
            
            do i=1, head
              dtp = dt
              
              do while (tcs(i) .LT. dtp .AND. tcs(i) .GT. 0)
                dtp = dtp - tcs(i)
                colls_dt = colls_dt + 1
                
                call handle_collision(Nspace, es, tcs, i, head+colls_dt)
              enddo
              
              tcs(i) = tcs(i) - dtp
            enddo
          
            head = head + colls_dt
            write (fhandle,910) t*1E9, es(1:head)!tcs(1:head)*1E9, 
  910     format((F6.2),100(2X,(F7.3)))          
            t = t+dt
          enddo
          
          onepart = head
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
      
