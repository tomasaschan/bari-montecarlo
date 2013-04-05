      program colltime2
        implicit none
        
          integer i
          real*8 collision_time, ct, eV
          parameter(eV=1000.0)
          call srand(0)
         
          do i = 1, 100000
            ct = collision_time(eV)
            print *, ct
          enddo
      end
