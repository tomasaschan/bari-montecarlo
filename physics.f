      module physics
        use precision
        !use interpolation

        ! nitrogen gas pressure and number density
        real(rkind) :: p, n
        ! number of interpolation points and collision processes
        integer(lkind) :: Ncs, NCollProc
        ! cross section interpolations
        real(rkind), allocatable :: cs(:,:)
        ! boundary values for the interpolations
        ! the lower boundary is also the energy loss
        real(rkind), allocatable :: cs_min(:), cs_max(:)
        ! total collision frequency; constant through simulation
        real(rkind) total_collision_frequency

      contains

        subroutine init_physics()
          implicit none

          integer(lkind) ie, ip

          real(rkind) :: cfreqs(Ncs)
          real(rkind), parameter   :: T = 300.0
          real(rkind), parameter   :: kB = 1.3806488e-23
          real(rkind), parameter :: Torr2Pa = 101325/760
          

          ! calculate nitrogen number density from gas pressure
          n = p * Torr2Pa / (kB*T)

          ! calculate total collision frequency
          do ie = 1, Ncs
            cfreqs(ie) = velocity(cs(ie,1))*n*sum(cs(ie,2:))
            print *, cs(ie,1), cfreqs(ie)
          end do

          total_collision_frequency = maxval(cfreqs)
        end subroutine init_physics

        function collision_time()
          use random

          implicit none
          real(rkind) :: collision_time, r

          r = random_real()
          collision_time = -log(r) / total_collision_frequency
        end function collision_time

        function collision_frequency(eV, process)
          implicit none

          integer process
          real(rkind) eV, collision_frequency

          collision_frequency = velocity(eV)*n*cross_section(eV, process)
        end function collision_frequency

        function cross_section(eV, process)
          implicit none

          ! Input argument and return value
          real(rkind) eV, cross_section
          integer process

          ! Intermediates
          real(rkind) dx
          integer idx

          if (eV .lt. cs_min(process) .or. eV .gt. cs_max(process)) then
            cross_section = 0
          else
            dx = cs(2,1)-cs(1,1)
            idx = 1 + int((eV-cs(1,1))/dx)
            cross_section = cs(idx,1+process)
          endif
        end function cross_section

        subroutine handle_collision(N, es, tcs, idx1, idx2)
          ! Redistribute energy between the primary and secondary electrons,
          ! and update the collision times for both.
          
          ! This version is based on energy only - it is the simplest possible
          ! implementation of the rigid sphere approximation.

          implicit none
          integer N, idx1, idx2
          real(rkind) es(N), tcs(N)
          real(rkind) ea, esec

          ! Calculate the available energy based on ionization energy
          ea = es(idx1) - cs_min(1) ! change to (p)

          ! Give a random fraction of the available energy to the secondary
          esec = secondary_energy(ea)
          ! Make sure we conserve energy
          !do while (esec.gt.ea)
          !  esec = secondary_energy(ea)
          !enddo
          !es(idx2) = max(esec,0.0D0)
          !es(idx1) = max(ea-es(idx2),0.0D0)

          ! Calculate new collision times for both electrons
          !tcs(idx1) = max(collision_time(es(idx1)), 0.0D0)
          !tcs(idx2) = max(collision_time(es(idx2)), 0.0D0)
        end

        function secondary_energy(ea)
          use random

          implicit none
          real(rkind) r, ea, secondary_energy, E
          parameter(E=13)
          r = random_real()
          secondary_energy = E*tan(atan(ea/E)*r)
          
        end

        function secondary_energy_simple(ea)
          use random

          implicit none

          real(rkind), intent(in) :: ea
          real(rkind) secondary_energy_simple

          secondary_energy_simple = ea*random_real()
        end

        function velocity(eV)
          ! Calculates the velocity in m/s for an electron with kinetic
          ! energy eV
          
          implicit none
          real(rkind) eV, velocity
          real me, e
          parameter(me=9.11E-31,e = 1.602176E-19)

          velocity = sqrt(2*eV*e/me)
        end

        function energy(v)
          ! Calculates the kinetic energy in eV for an electron which 
          ! moves at a velocity v (in m/s)
          !
          ! v is a 3-array that represents three coordinates
          ! (the coordinate system is not important as long as
          ! |v| = sqrt(v(1)^2+v(2)^2+v(3)^2)
          
          implicit none
          real(rkind) v(3), energy
          real me, e
          parameter(me=9.11E-31,e = 1.602176E-19)
          
          energy = .5*me*(v(1)*v(1) + v(2)*v(2) + v(3)*v(3)) / e
        end function energy
      end module physics