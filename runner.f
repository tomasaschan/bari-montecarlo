      program runner
        implicit none
        include 'mpif.h'

        real*8 energy, tfin, dt, ni
        integer ierr, rnk, seed
        
        
        call MPI_Init(ierr)
        call MPI_Comm_rank(MPI_Comm_world, rnk, ierr)
        
        if (rnk.EQ.0) then
            ! Read the comment line
            read *,
            read *, seed, ni, energy, tfin, dt

            call srand(seed)
            call simulate_distribution(ni, energy, tfin, dt)
        endif

        call MPI_Finalize(ierr)
        
      end
