      program indatatester
        use io
        use precision
        use physics
        use interpolation

        implicit none
        integer(lkind) Nruns, i
        real(rkind) tfin, dt, e0, Pnull, Pcoll, cftot !ct, Pnull
        real(rkind), allocatable :: css(:)

        call read_program_input(Nruns, tfin, dt, e0, p)
        call init_interpolation()
        do i=1,NDataFiles
          call interpolate(i)
        end do
        call clean_up_io()
        call init_physics()

        allocate(css(NDataFiles))

        css = (/ (cross_section(e0, int(i,ikind)), i=1,NDataFiles) /)

        print *, "# Cross sections:"
        print *, css
        

        print *, "# Total/actual collision time:"
        cftot = velocity(e0)*n*sum(css)
        print *, 1/total_collision_frequency, 1/cftot

        print *, "# Probability of null collision:"
        Pnull = 1-cftot/total_collision_frequency
        write(*,'((F6.3),(A))') Pnull*100, "%"

        print *, "# Avg number of non-collided particles at tfin:"
        Pcoll = 1-(1-exp(-total_collision_frequency*tfin))*(1-Pnull)
        write(*,'((Es15.2),(F6.2),(A))') Pcoll*Nruns, 100*Pcoll, "%"

        call clean_up_interp()

        deallocate(css)
      end program indatatester
