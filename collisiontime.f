      program collisiontime_vs_energy
        implicit none
          ! loop variables
          integer i, N
          ! auxiliary functions
          real*8 cross_section, velocity, nonrandom_collision_time
          ! variables
          real*8 eV, cs, alpha, nn
          parameter(nn = 3E22)
          
          N = 100
          
          do i=1, N
            eV = 300.0*(i/(N*1.0))
            
            cs = cross_section(eV)
            alpha = nn*velocity(eV)*cs
            
            write(*,*) eV, cs*1E20,nonrandom_collision_time(nn, eV)*1E9
            
          enddo
      
      end
