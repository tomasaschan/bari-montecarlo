      module physics
        use precision
        private
        !use interpolation

        ! nitrogen gas pressure and number density
        real(rkind), public :: p, n
        ! number of interpolation points and collision processes
        integer(lkind), public :: Ncs, NCollProc
        ! cross section interpolations and boundary values
        ! the lower boundary is also the energy loss
        real(rkind), allocatable, public :: cs(:,:), cs_min(:), cs_max(:)
        ! number of electrons created in processes
        integer(ikind), allocatable, public :: products(:)
        ! total collision frequency; constant through simulation
        real(rkind), public :: total_collision_frequency

        real(rkind), public :: me, e
        parameter(me=9.11E-31,e = 1.602176E-19)

        public :: init_physics
        public :: cross_section
        public :: collision_time
        public :: collision_frequency
        public :: handle_collision
        public :: velocity
      contains

        subroutine init_physics()
          implicit none

          integer(lkind) ie

          real(rkind) :: cfreqs(Ncs)
          real(rkind), parameter   :: T = 300.0
          real(rkind), parameter   :: kB = 1.3806488e-23
          real(rkind), parameter :: Torr2Pa = 101325/760
          

          ! calculate nitrogen number density from gas pressure
          n = p * Torr2Pa / (kB*T)

          ! calculate total collision frequency for each energy
          cfreqs = (/ (velocity(cs(ie,1))*n*sum(cs(ie,2:)), ie=1, Ncs) /)

          ! add the null collisions, by taking the maximum
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

        subroutine handle_collision(N, es, tcs, idx, head, newp_dt)
          use random, only : random_real
          ! select a collision process, 

          implicit none
          ! in arguments
          integer N, idx, head, newp_dt
          real(rkind) es(N), tcs(N)
          ! other variables
          real(rkind) r, cfsum
          integer ip

          ! select a collision process
          r = random_real()
          cfsum = 0.0
          ip = 0
          selectproc: do 
            ip = ip+1
            if (ip .gt. NCollProc) then 
              ! null collision!
              ! just give the particle a new collision time and exit
              !print *, "# null collision at process ", ip
              tcs(idx) = collision_time()
              return
            end if

            cfsum = cfsum + collision_frequency(es(idx), ip) / total_collision_frequency

            if (cfsum .gt. r) then
              ! this is the process we want
              exit selectproc
            end if
          end do selectproc

          ! test for null collision
          
          ! real collision

          ! subtract energy loss from available energy
          es(idx) = es(idx) - cs_min(ip)

          ! will a new electron be created?
          if (products(ip) .eq. 1) then
            ! create a new electron which shares the available energy
            newp_dt = newp_dt + 1
            es(head+newp_dt) = secondary_energy(es(idx))
            ! we must also subtract this energy from the primary's
            es(idx) = es(idx) - es(head+newp_dt)
            ! set collision time for the secondary
            tcs(head+newp_dt) = collision_time()
          end if

          ! update collision time
          tcs(idx) = collision_time()
        end subroutine handle_collision

        function secondary_energy(ea)
          use random

          implicit none
          real(rkind) r, ea, secondary_energy, E
          parameter(E=13)
          r = random_real()
          secondary_energy = E*tan(atan(ea/E)*r)    
        end function secondary_energy

        function velocity(eV)
          ! Calculates the velocity in m/s for an electron with kinetic
          ! energy eV
          
          implicit none
          real(rkind) eV, velocity

          velocity = sqrt(2*eV*e/me)
        end function velocity

        function energy(v)
          ! Calculates the kinetic energy in eV for an electron which 
          ! moves at a velocity v (in m/s)
          !
          ! v is a 3-array that represents three coordinates
          ! (the coordinate system is not important as long as
          ! |v| = sqrt(v(1)^2+v(2)^2+v(3)^2)
          
          implicit none
          real(rkind) v(3), energy
          
          energy = .5*me*(v(1)*v(1) + v(2)*v(2) + v(3)*v(3)) / e
        end function energy
      end module physics