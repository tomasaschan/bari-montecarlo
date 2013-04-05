      program simulation
C       This program simulates electron propagation in a collisional medium.
C       As input, it takes the following parameters from stdin: 
C         1. The number of primary electrons to simulate, Nruns
C         2. The time for which the simulation should run, tfin
C         3. The time resolution, dt
C         4. The initial energy of each primary electron, e0
C         5. The ionization energy, i.e. the energy that will be "consumed" 
C            in each collision, eI
C         6. The energy resolution (i.e. bin size) in the histogram, de

C       The program was written by Tomas Lycken, tlycken@f.kth.se

        implicit none
        include 'mpif.h'

C       ***********************
C       ** DECLARE VARIABLES **
C       ***********************

C       Number of processes, index of current process and error state variable.
C       These will all be used by MPI for parallelization.
        integer nproc, rnk, ierr

C       Input parameters.
        integer Nruns
        real*8 tfin, dt, e0, eI, de

C       Bins for the histogram
        integer Nbins
        integer, dimension(:), allocatable :: bins, binsums

C       Iteration variable
        integer i
C       Timing variables
        real*8 time_t

C       ****************************
C       ** PREPARE FOR SIMULATION **
C       ****************************

C       Initialize MPI, and find out current rank and number of processes
        call MPI_Init(ierr)
        time_t = MPI_Wtime()
        call MPI_Comm_size(MPI_Comm_world, nproc, ierr)
        call MPI_Comm_rank(MPI_Comm_world, rnk, ierr)

C       Read the input from stdin, and broadcast to all processes
        call read_and_share(rnk, Nruns, tfin, dt, e0, eI, de)

C       Seed the random number generator.
C       If reproducible results are required, use seed_rand_0()
        call seed_rand()

C       Allocate memory for histogram bins, and initialize to 0
        Nbins = ceiling(e0/de)
        allocate(bins(Nbins))            
        do i=1, Nbins
          bins(i) = i
        enddo

C       *****************
C       ** DO THE WORK **
C       *****************

C       call equivalent routine to onepart Nruns times

C       **********************************
C       ** SUMMARIZE AND OUTPUT RESULTS **
C       **********************************

        if (rnk.EQ.0) then
          allocate(binsums(Nbins))
        endif

        call MPI_Barrier(MPI_Comm_world, ierr)

        call MPI_Reduce(bins, binsums, Nbins, MPI_Integer, MPI_Sum, 
     +     0, MPI_Comm_world, ierr)

        if (rnk.EQ.0) then
          print *, sum(binsums)
        endif

C       Destruct the MPI framework.
        call MPI_Finalize(ierr)
      end