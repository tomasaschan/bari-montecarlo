      program histogram
        implicit none
        include 'mpif.h'
      
900   format(3(E15.8))
910   format('# ', a, ' took ', Es10.3, ' s')
!940   format('# Number of events: ', I0, ' ', I0, ' ', f5.2)

        ! DECLARE VARIABLES

        ! Parameters for simulation (indata)
        integer Nruns
        real*8 tfin, dt, eI, e0
        
        ! MPI related stuff
        integer rnk, nproc, ierr
      
        ! Data for collisional cross-sections
        integer Ninterp
        parameter(Ninterp=int(1e5))
        real*8 cs_data(Ninterp,2)
        character*30 fname
        real*8 x0, x1
        common /cs/ cs_data, x0, x1
        
        ! Bins for summarizing the histogram
        integer i, it, Nbins, Ntimes
        parameter(Nbins=int(1e3))
 !       integer binsize
        integer, allocatable :: bins(:,:), binsum(:,:), totsum(:)
        real*8, allocatable :: norm_factor(:)

        ! Variables for output
        real*8 t, e, p

        ! Timing of the simulation
        double precision allt, startt, endt

        ! INITIALIZE SIMULATION
        
        ! MPI framework
        call init_mpi(rnk, nproc)
        ! Read data from stdin and share to all processes
        call read_program_input(Nruns, tfin, dt, e0, eI, 
     +                          fname, x0, x1, rnk)
        ! Interpolate cross-section data, and share with all processes
        call get_interpolation(fname,Ninterp,cs_data,x0,x1,rnk)

        Ntimes = int(ceiling(tfin/dt))
        allocate(bins(Nbins, Ntimes))
        allocate(binsum(Nbins, Ntimes))

        ! Seed pseudo-random number generator
        call seed_rand()

        call MPI_Barrier(MPI_Comm_world, ierr)

        allt = MPI_Wtime()

        bins = 0
        
        ! RUN SIMULATION

        startt = MPI_Wtime()
        do i=1, Nruns/nproc
          call onepart(e0, tfin, dt, Nbins, Ntimes, bins, eI)
        enddo
        
        if (rnk.EQ.0) then
          do i = 1, Nruns-nproc*Nruns/nproc
            call onepart(e0, tfin, dt, Nbins, Ntimes, bins, eI)
          enddo
        endif

!        call MPI_Barrier(MPI_Comm_world, ierr)
        if (rnk.EQ.0) then
          endt = MPI_Wtime()
          write(*,910) 'Simulation', endt-startt
        endif

        ! REDUCE RESULTS TO HISTOGRAM

        call MPI_Reduce(bins,binsum,Nbins*Ntimes,MPI_INTEGER,MPI_SUM,
     +                0, MPI_COMM_WORLD, ierr)
     
        if (rnk.EQ.0) then
          allocate(totsum(Ntimes))
          totsum = sum(binsum,dim=1)
          norm_factor = 100/float(totsum)


          do it=1, Ntimes
            do i=1, Nbins
              t = dt*it
              e = i*e0/float(Nbins)
              p = float(binsum(i,it))*norm_factor(it)
              write(*,900) t, e, p
            enddo
            write (*,*) " "
          enddo

          deallocate(totsum)
        endif

!        call MPI_Barrier(MPI_Comm_world, ierr)
        if (rnk.EQ.0) then
          write(*,910), 'Everyting', MPI_Wtime() - allt
        endif

        deallocate(bins)
        deallocate(binsum)

        ! TERMINATE MPI
        
        call finalize_mpi()
      end 
