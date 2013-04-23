      module physics
        use precision
        !use interpolation

        ! nitrogen gas pressure
        real(rkind) :: p
        ! number of collision processes
        integer(lkind) :: NCollProc
        ! cross section interpolations
        real(rkind), allocatable :: cs(:,:)
        ! boundary values for the interpolations
        ! the lower boundary is also the energy loss
        real(rkind), allocatable :: cs_min(:), cs_max(:)


      contains

        function collision_time(eV)
          ! This function simply uses the nonrandom version, and then adds
          ! a random factor of -log(rand()) 
          use random

          implicit none
            ! function input parameters
            real(rkind) eV
            ! return value and intermediates
            real(rkind) collision_time, r, ct

            r = random_real()
            ct = nonrandom_collision_time(eV)
            collision_time = -log(r)*ct
        end
        
        function nonrandom_collision_time(eV)
          ! Calculates the collision time for a particle of energy eV,
          ! given a particle density of the medium with which it collides
          ! of n m^(-3).
          
          implicit none
            ! function input parameters
            real(rkind) eV
            ! function return value, and helper functions
            real(rkind) nonrandom_collision_time
            ! intermediate quantities
            real(rkind) alpha, v, sigm

            ! parameter: air molecular density
            real(rkind) :: n
            real(rkind), parameter   :: T = 300.0
            real(rkind), parameter   :: kB = 1.3806488e-23
            real(rkind), parameter :: Torr2Pa = 101325/760
            n = p * Torr2Pa / (kB*T)
            
            ! TODO: introduce multiple collision processes


            v = velocity(eV)
            sigm = cross_section(eV,1)
            alpha = n*v*sigm

            nonrandom_collision_time = 1.0 / alpha
        end

        function cross_section(eV, process)
          !use interpolation, only : cs, cs_min, cs_max
          use mpi, only : rnk

          implicit none

          ! Input argument and return value
          real(rkind) eV, cross_section
          integer process

          ! Intermediates
          real(rkind) dx
          integer idx

          !print *, "# rnk/cs_min/cs_max", rnk, cs_min(process), cs_max(process), eV, eV .gt. cs_max(process), eV .lt. cs_min(process)

          if (eV .lt. cs_min(process) .or. eV .gt. cs_max(process)) then
            cross_section = 0
          else
            dx = cs(2,1)-cs(1,1)
            idx = 1 + int((eV-cs(1,1))/dx)
            cross_section = cs(idx,1+process)
          endif
        end
        
        function cross_section_polynomial(eV)
          ! Calculates the collision cross-section for electrons in a
          ! nitrogen gas, based on a fit to data from 
          ! www.lxcat.laplace.univ-tlse.fr
          
          implicit none
            ! Input argument and return value
            real(REAL64) eV, cross_section_polynomial
          
            ! Parameters from curve fitting in Matlab
            real(REAL64) q0, q1, q2, q3, q4, l0, l1, E0
            parameter(q0=-2.2007449E-20,q1=2.855227E-21,q2=-4.6951205E-23)
            parameter(q3=3.031989421E-25,q4=-6.335964E-28)
            parameter(l0=1.21665367425970e-20, l1=1.86036008409668e-18)
            parameter(E0=70)

            if (eV.LT.E0) then
                cross_section_polynomial=q0+q1*eV+q2*eV*eV+q3*eV*eV*eV+q4*eV*eV*eV*eV
            else 
                cross_section_polynomial=l0+l1/eV
            endif
        end

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
            es(idx2) = max(esec,0.0D0)
            es(idx1) = max(ea-es(idx2),0.0D0)

            ! Calculate new collision times for both electrons
            tcs(idx1) = max(collision_time(es(idx1)), 0.0D0)
            tcs(idx2) = max(collision_time(es(idx2)), 0.0D0)
        end
        
        function secondary_energy(ea)
          use random

          implicit none

          real(rkind), intent(in) :: ea
          real(rkind) secondary_energy
          real(rkind) r, E
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