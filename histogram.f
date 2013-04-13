      program histogram
        implicit none
        include 'mpif.h'
      
900   format(E15.2,E15.2)
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
        integer i, Nbins
        parameter(Nbins=int(1e3))
 !       integer binsize
        integer bins(Nbins), binsum(Nbins), totsum

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

        call MPI_Barrier(MPI_Comm_world, ierr)

        ! Seed pseudo-random number generator
        call seed_rand()

        allt = MPI_Wtime()

        do i=1, Nbins
          bins(i) = 0
        enddo

        startt = MPI_Wtime()
        do i=1, Nruns/nproc
          call onepart(e0, tfin, dt, Nbins, bins, eI)
        enddo
        
        if (rnk.EQ.0) then
          do i = 1, Nruns-nproc*Nruns/nproc
            call onepart(e0, tfin, dt, Nbins, bins, eI)
          enddo
        endif
        
!        call MPI_Barrier(MPI_Comm_world, ierr)
        if (rnk.EQ.0) then
          endt = MPI_Wtime()
          write(*,910) 'Simulation', endt-startt
        endif

        call MPI_Reduce(bins, binsum, Nbins, MPI_INTEGER, MPI_SUM,
     +                0, MPI_COMM_WORLD, ierr)
     
        if (rnk.EQ.0) then
          totsum = sum(binsum)
          do i=1, Nbins
             write(*,900)
     +         i*E0/float(Nbins), float(binsum(i))/float(totsum)*100
          enddo
        endif
        
!        call MPI_Barrier(MPI_Comm_world, ierr)
        if (rnk.EQ.0) then
          write(*,910), 'Everyting', MPI_Wtime() - allt
        endif
        
        call finalize_mpi()
      end 
