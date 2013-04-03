      program runner
        implicit none
        include 'mpif.h'

        real*8 energy, tfin, dt, nn
        integer N, seed
c~         integer ierr, rnk
c~         real*8 time
c~         
c~         call MPI_Init(ierr)
c~         call MPI_Comm_rank(MPI_Comm_world, rnk, ierr)

c~         if (rnk.EQ.0) then
            ! Read the comment line in input file
            read *,
            ! Read parameters from stdin
            read *, seed, N, nn, energy, tfin, dt

            call srand(seed)
c~             time = MPI_Wtime()
            call simulate_distribution(nn, N, energy, tfin, dt)
c~             print *, '#Simulation took ', MPI_Wtime()-time, 's'
c~         endif

c~         call MPI_Finalize(ierr)
        
      end
