      subroutine simulate_distribution(nn, N, initial_energy, tfin, dt)
        implicit none
          ! Input parameters
          real*8 nn, initial_energy, tfin, dt, t
          integer N

          ! Helper functions
          real*8 collision_time, velocity!, energy
          
          ! Variables for simulation
          integer i, head, colls_this_step
          real*8 tcs(2*N), dtp
          real*8 Es(2*N), xs(6*N), vs(6*N)!, v0
          
          ! Initialize simulation variables
          t = 0
          head = N
          call init_arrays(N, xs, vs, Es, tcs, initial_energy, nn)

          ! Simulate from t = 0 to t = tfin
          do while (t .LT. tfin)
            colls_this_step = 0
            do i = 1, head
              dtp = dt

              ! Handle all collisions during this timestep
              do while (tcs(i) .lt. dtp)
                ! Propagate to collision, i.e. for tc(i)
                call propagate(xs(i:i+3), vs(i:i+3), tcs(i))
                
                ! TODO: Generate new e-
                !       Give both particles appropriate v, E
                
                !       Update number of added particles this time step
                colls_this_step = colls_this_step + 1

                es(i) = es(i)/2
                es(head+colls_this_step) = es(i)
                vs(i) = velocity(es(i))
                vs(head+colls_this_step) = velocity(es(i))
                
                ! Update time step
                dtp = dtp - tcs(i)
                ! Calculate new time to next collision
                tcs(i) = collision_time(nn, Es(i))
                tcs(head+colls_this_step) = 
     +                 collision_time(nn, Es(head+colls_this_step))
              enddo

              ! No more collisions for this particle - finish time step
              
              ! Propagate to end of timestep, i.e. for dtp
              call propagate(xs(i:i+3), vs(i:i+3), dtp)
              
              ! Update time to next collision
              tcs(i) = tcs(i) - dtp
            enddo
            
            ! update number of particles after this timestep
            head = head + colls_this_step
            
910     format(F5.4,2(3(2X,EN16.8)),2X,F6.2)
            write(*,910) t*1E9, xs(1:3*head), vs(1:3*head), Es(1:head)
            print *, t, tcs(1:head)
            t = t + dt
          enddo
      end
      
      subroutine init_arrays(N, xs, vs, Es, tcs, E0, nn)
        implicit none
          
          integer N, i
          real*8 xs(6*N), vs(6*N), Es(2*N), tcs(2*N), E0, v0, nn
          real*8 velocity, collision_time
        
          ! Initialize N particles
          xs(1:3*N+2) = 0
          
          v0 = velocity(E0)
          vs(1:N*3) = 0
          vs(1:3:N) = v0
          
          Es(1:2*N) = E0
          do i=1, 2*N
            tcs(i) = collision_time(nn, Es(i))
          enddo

          ! Give room for N more particles - init as 0
          Es(N+1:2*N) = 0
          xs(N+1:N+3*N) = 0
          vs(N+1:N+3*N) = 0
          tcs(N:2*N) = 0
      end

      subroutine propagate(xsi, vsi, t)
        implicit none
          
          real*8 xsi(3), vsi(3), t
          
          xsi = xsi + vsi * t
          
      end
