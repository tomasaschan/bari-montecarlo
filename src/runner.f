      program run_simulation
        use, intrinsic :: iso_fortran_env, only : REAL64, INT16, INT32, INT64, stdout=>output_unit

        use io, only : NDataFiles, read_program_input, clean_up_io, paramformat, padr
        use physics, only : p, init_physics, NCollProc, dt, tfin, n
        use interpolation, only : nri, init_interpolation, interpolate, clean_up_interp
        use random, only : seed_rand_0
        use single_particle, only : onepart, e0
        use eedf, only : init_eedfbins, Ntimes, calculate_totals, print_eedf
        use ratecoeffs, only : calculate_ratecoeffs_evolution, print_ratecoeffs, clean_up_ratecoeffs
        use populations, only : calculate_pops, print_pops, clean_up_pops, init_pops, Npops, neexpr

        implicit none

920   format('# ', a, F8.2, ' s')
        ! VARIABLE DECLARATIONS       
        integer(INT64) Nruns, i
        integer :: clock_start, clock_end, clock_rate
        !

        ! INITIALIZATION

          ! start timer
          call system_clock(count_rate=clock_rate)
          call system_clock(count=clock_start)

          ! get input from stdin        
          call read_program_input(Nruns, tfin, dt, e0, p, Npops, neexpr)
          
          ! initialize cross-section interpolations and population ODE's
          call init_interpolation()

          NCollProc = NDataFiles

          do i=1,NCollProc
            call interpolate(i)
          end do
          
          call init_pops()

          ! clean up allocated memory
          call clean_up_io()

          ! allocate space for histogram, initialize to zero
          Ntimes = int(ceiling(tfin/dt))
          call init_eedfbins(e0)

          ! initialize physics module
          call init_physics()

          write(stdout, paramformat) padr("# Initial energy", 20), e0, "eV   "
          write(stdout, paramformat) padr("# End time", 20), tfin*1e9, "ns   "
          write(stdout, paramformat) padr("# Time step", 20), dt*1e9, "ns   "
          write(stdout, paramformat) padr("# Gas pressure", 20) , p, "Torr "
          write(stdout, paramformat) padr("# Gas density", 20) , n*1e-6, "cm^-3"

          ! seed pseudo-random number generator
          call seed_rand_0()

        ! SIMULATION

          ! split work on all threads
          do i=1, Nruns
            call onepart()
          end do
          
        ! POST PROCESSING

          ! summarize eedf
          call calculate_totals()

          call print_eedf()

          ! calculate rate coefficients
          call calculate_ratecoeffs_evolution()
          call print_ratecoeffs()

          call calculate_pops()
          call print_pops()

          call clean_up_ratecoeffs()
          call clean_up_pops()

          ! stop timer
          call system_clock(count=clock_end)
          write(stdout,920) 'It took', (clock_end-clock_start)/real(clock_rate,REAL64)

        ! CLEAN UP
          !  deallocate(bins)g
          call clean_up_interp()
        end program run_simulation