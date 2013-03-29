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
              
              do while (tcs(i) .LE. dtp .AND. tcs(i) .GT. 0)
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

      subroutine handle_collision(N, es, tcs, idx1, idx2)
        implicit none
          integer N, idx1, idx2
          real*8 es(N), tcs(N), collision_time, velocity, energy
          real*8 v1(3), v2(3), v0, cph, sph, th, pi, R(3)
          parameter(pi=4*atan(1.0))

          ! Scattering angles for the collision
          cph = 2*rand()-1
          sph = sqrt(1-cph*cph)
          th = 2*pi*rand()

          ! Initial velocity of the incoming electron
          v0 = velocity(es(idx1))

          ! Aux values
          R = (/ cos(th)*sph, sin(th)*sph, cph /)

          ! New velocity of both electrons
          v1 = v0/2 * ((/1,0,0/) + R)
          v2 = v0/2 * ((/1,0,0/) - R)
          
          ! Set energies of both electrons
          es(idx1) = energy(v1)
          es(idx2) = energy(v2)
          
          ! Calculate new collision times
          tcs(idx1) = collision_time(es(idx1))
          tcs(idx2) = collision_time(es(idx2))
      end
      
