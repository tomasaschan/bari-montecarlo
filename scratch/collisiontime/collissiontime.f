      program collisiontime_vs_energy
        implicit none
          ! loop variables
          integer i, N
          ! auxiliary functions
          real*8 cross_section, nonrandom_collision_time
          ! variables
          real*8 eV, cs, nn, ct
          parameter(nn = 3E21)
          
          N = 100
          
          do i=1, N
            eV = 1000.0*(i/(N*1.0))
            
            cs = cross_section(eV)
            ct = nonrandom_collision_time(eV)

            write(*,*) eV, cs*1E20, ct*1E9
            
          enddo
      
      end
