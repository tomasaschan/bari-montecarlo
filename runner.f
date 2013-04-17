      program interptest
        use mpi
        use io
        use interpolation
        use random
        use single_particle
        use histogram
        use iso_fortran_env, only : REAL64

        implicit none


        ! VARIABLE DECLARATIONS
        
          ! variables for indata and counters
          integer Nruns, i
          real(REAL64) tfin, dt, e0
          real(REAL64), allocatable :: eI(:)

          ! variables for histogram bins
          integer :: Ntimes

        !

        ! INITIALIZATION

          ! initialize mpi
            call init_mpi()
            !
          !

          ! master thread: read indata and interpolate cross-sections
            if (rnk.eq.0) then
              ! get input from stdin        
              call read_program_input(Nruns, tfin, dt, e0, eI)

              ! initialize e vector of interpolation
              call init_interpolation(e0)

              ! interplolate each data series
              do i=1,NDataFiles
                call read_interpolation_data(fnames(i), raw, nrd)
                call interpolate(i, e0raw(i), e1raw(i))
              end do

              deallocate(e0raw)
              deallocate(e1raw)
              deallocate(fnames)
            end if
          !

          ! share indata and interpolations between all processes
          
            call share_data(Nruns,tfin,dt,e0,eI,nri,NDataFiles,interp,interp_min,interp_max)
            !print *, "# interp_min/max: ", interp_min, interp_max
            !
          !

          ! allocate space for histogram, initialize to zero
            Ntimes = int(ceiling(tfin/dt))
            call init_bins(Ntimes)
          !

          ! seed pseudo-random number generator
            call seed_rand_0()
            !
          !
        !

        call barrier()

        ! SIMULATION
          do i=1, Nruns/nproc
            call onepart(e0, dt, Ntimes, NDataFiles, eI)
          end do

          ! master thread: run remaining, if Nruns/nproc is not even
            if (rnk.eq.0) then
              do i=1, Nruns-nproc*Nruns/nproc
                call onepart(e0, dt, Ntimes, NDataFiles, eI)
              end do
            end if
          !
        !

        ! POST PROCESSING

        !if (rnk.eq.0) then
        !  do i=1,nri
        !    print *, interp(i,1), interp(i,2)
        !  end do
        !end if
        call calculate_totals(Ntimes, e0)

        ! CLEAN UP
        !  deallocate(bins)
          deallocate(interp)
          deallocate(eI)

          call finalize_mpi()
        !
      end program interptest