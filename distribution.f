      subroutine simulate_distribution(nn, N, energy, tfin, dt)
        implicit none
          ! Input parameters
          real*8 nn, energy, tfin, dt, t
          integer N

          ! Helper functions
          real*8 collision_time, velocity
          
          ! Parameters for simulation
          integer i, head, colls_this_step
          real*8 tc(2*N), dtp !velocity,, tc, dtp
          real*8 energies(2*N), positions(2*N)
          
          ! Electron mass
          real me
          parameter(me=9.11E-31)

          ! Initialize simulation
          head = N
          do i = 1, head
            energies(i) = energy
            positions(i) = 0
            tc(i) = collision_time(nn, energies(i))
          enddo
          do i = 1, head
            energies(N+i) = 0
            positions(N+i) = 0
            tc(i) = 0
          enddo
          t = 0
          
          do while (t .LT. tfin)
            colls_this_step = 0
            do i = 1, head
              dtp = dt
              
              do while (tc(i) .lt. dtp)
                ! Collision occurs
                
                ! Propagate to collision, i.e. for tc(i)
                positions(i)=positions(i)+velocity(energies(i))*tc(i)
                
                ! TODO: Generate new e-
                !       Give both particles appropriate energies
                                
                ! Update time step
                dtp = dtp - tc(i)
                ! Calculate new collision time
                tc(i) = collision_time(nn, energies(i))
              enddo
              
              ! No more collisions - finish time step
              
              ! Propagate to end of timestep, i.e. for dtp
              positions(i)=positions(i)+velocity(energies(i))*dtp
              
              ! Update collision time
              tc(i) = tc(i) - dtp
            enddo
            
            print *, t, positions(1:head), energies(1:head)
              
            t = t + dt
          enddo
      end
