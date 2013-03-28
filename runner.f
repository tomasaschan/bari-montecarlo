      program runner
        implicit none
        include 'mpif.h'

        real*8 energy, tfin, dt, nn
        integer N, seed
        integer ierr, rnk
        real*8 time
        
        call MPI_Init(ierr)
        call MPI_Comm_rank(MPI_Comm_world, rnk, ierr)

        if (rnk.EQ.0) then
            ! Read the comment line in input file
            read *,
            ! Read parameters from stdin
            read *, seed, N, nn, energy, tfin, dt

            call srand(seed)
            time = MPI_Wtime()
            call simulate_distribution(nn, N, energy, tfin, dt)
            print *, '#Simulation took ', MPI_Wtime()-time, 's'
        endif

        call MPI_Finalize(ierr)
        
      end
