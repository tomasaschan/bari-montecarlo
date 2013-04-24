      program indatatester
        use io
        use precision
        use physics
        use interpolation

        implicit none
        integer(lkind) Nruns, i
        real(rkind) tfin, dt, e0, Pnull!, !ct, Pnull
        real(rkind), allocatable :: css(:)

        call read_program_input(Nruns, tfin, dt, e0, p)
        call init_interpolation()
        do i=1,NDataFiles
          call interpolate(i)
        end do
        call clean_up_io()
        call init_physics()

        allocate(css(NDataFiles))

        css = (/ (cross_section(e0, int(i,4)), i=1,NDataFiles) /)

        print *, "# Cross sections:"
        print *, css
        print *, "# Probability of null collision:"
        Pnull = 1-velocity(e0)*n*sum(css)/total_collision_frequency
        print *, Pnull
        print *, "# Total collision time:"
        print *, 1/total_collision_frequency
!         print *, "# A few sample collision times:"
!         do i=1,5
!           ct = collision_time()
!           if (ct .gt. tfin) then
!             print *, ct, "> tfin"
!           else
!             print *, ct, "< tfin"
!           end if
!         end do
        print *, "# Probability of collision before tfin:"
        print *, (1-exp(-total_collision_frequency*tfin))*(1-Pnull)


        call clean_up_interp()

        deallocate(css)
      end program indatatester
