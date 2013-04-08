      function random_real()
        ! Abstraction over rand(), in case I find a better option

        implicit none
        real*8 random_real

        random_real = rand()
      end

      function random_von_Neumann()
        ! Gets a random number from the probability distribution
        ! for secondary electron energy, through von Neumann rejection

        implicit none

        real*8 random_von_Neumann, random_real, r, E
        parameter(E=13)

        r = random_real()*300
        random_von_Neumann =  1/(1+(r/E)**2)
      end
      
      subroutine seed_rand()
        ! Use MPI to find a representation of the wall time that changes
        ! rapidly (increments every 1e-7 seconds) and use that as an
        ! argument to srand()
      
        implicit none
        include 'mpif.h'
        
        integer t, ierr
        integer*16 bt
        
        ! Get a high-precision time value from MPI, and shift it up so
        ! that all significant digits are part of the integer truncation
        bt = int(MPI_Wtime(ierr)*1e7, 16)
        ! Truncate on the /left/ end of the big number, to get something
        ! that varies quickly with time (i.e. varies as 1e-7 seconds)
        ! Convert it to integer*4 so we can seed rand() with it
        t = int(mod(bt, huge(t)), 4)
        
        ! Use the rapidly-changing number as seed for rand()
        call srand(t)
      end
      
      subroutine seed_rand_0()
        ! Call srand() with a constant seed, to be used for testing or
        ! producing reproducible simulation runs, but with a syntax
        ! similar to seed_rand() above.
        
        implicit none
        call srand(0)
      end
