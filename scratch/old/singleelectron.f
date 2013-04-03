      subroutine simulate_single_electron(ni, sigmai, energy, tfin, dt)
        implicit none
        
        real*8 ni, sigmai, energy, tfin, dt
        real*8 alpha, collision_time
        real*8 velocity, position, t, tc, dtp
        real me, mi
        parameter(me=9.11E-31, mi=1.6726E-27)
                
        integer N
        
        alpha=ni*sigmai
        N = ceiling(tfin/dt)
                
        position = 0.0
        t = 0.0
        
        do while (t .LT. tfin)
            
            dtp = dt
            
            velocity = sqrt(2*energy/me)
            tc = collision_time(ni, energy)

            do while (tc .LT. dtp)
                ! A collision will occur
                ! Handle the collision, and then prepare for next coll.
                
                ! Propagate particle up to collision moment
                position = position + velocity * tc
                
                ! Update energy
                energy = (1 - 2*me/mi)*energy
                
                ! 
                
                ! Update time variables
                dtp = dtp - tc
                tc = collision_time(ni, energy)
            enddo
            
            ! We have handled all collisions for this particle within dt
            ! Propagate particle one time step
            
            position = position + velocity * dtp
            tc = tc - dtp
            print *, t*1E9, tc
            t = t + dtp
        enddo
        
      end


      function collision_time(ni, E)
        implicit none
          real*8 collision_time, cross_section
          ! ion density, energy, logarithm of energy
          real*8 ni, E, logE, v
          ! root of electron mass, cross section parameters
          real me, k1, k2, k3
          parameter(me=9.11E-31)
          parameter(k1=-65.57843,k2=13.077650,k3=-1.4198878)
          
          ! convert energy back to eV
          v = sqrt(2*E/me)
          E = E/1.602177E-19
          logE = log(E)
          cross_section = exp(k1+k2*logE+k3*logE*logE)
          collision_time=-log(rand())/(ni*cross_section*v)
      end
