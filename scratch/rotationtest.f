        program rotationtest
          implicit none
          include 'mpif.h'
          
          integer ierr
                  
          real cph, sph, th, pi
          parameter(pi=4*atan(1.0))
          
          real*8 e0, v0, v1(3), v2(3)
          real*8 energy, velocity
          
          real R(3)

          call MPI_Init(ierr)
          call seed_rand()
          
          cph = 2*rand()-1
          sph = sqrt(1-cph*cph)
          th = 2*pi*rand()
                  
          R = (/ cos(th)*sph, sin(th)*sph, cph /)

          print *, R

          e0 = 300
          v0 = velocity(e0)
          
          v1 = v0/2 * ((/1,0,0/) + R)
          v2 = v0/2 * ((/1,0,0/) - R)
          
          write(*,'(F6.2)') energy(v1)+energy(v2)
          
                  
          call MPI_Finalize(ierr)
        end
