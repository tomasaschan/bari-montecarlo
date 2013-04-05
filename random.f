      subroutine seed_rand()
C       Use MPI to find a representation of the wall time that changes
C       rapidly (increments every 1e-7 seconds) and use that as an
C       argument to srand()
      
        implicit none
        include 'mpif.h'
        
        integer t, ierr
        integer*16 bt
        
C       Get a high-precision time value from MPI, and shift it up so
C       that all significant digits are part of the integer truncation
        bt = int(MPI_Wtime(ierr)*1e7, 16)
C       Truncate on the /left/ end of the big number, to get something
C       that varies quickly with time (i.e. varies as 1e-7 seconds)
C       Convert it to integer*4 so we can seed rand() with it
        t = int(mod(bt, huge(t)), 4)
        
C       Use the rapidly-changing number as seed for rand()
        call srand(t)
      end
      
      subroutine seed_rand_0()
C       Call srand() with a constant seed, to be used for testing or
C       producing reproducible simulation runs, but with a syntax
C       similar to seed_rand() above.
        
        implicit none
        call srand(0)
      end

      function random_real()
C       Abstraction over rand(), in case I find a better option

        implicit none
        real*8 random_real

        random_real = rand()
      end
