      module physics
        use iso_fortran_env, only : REAL64
        use interpolation

      contains

        function collision_time(eV, np)
          ! This function simply uses the nonrandom version, and then adds
          ! a random factor of -log(rand()) 
          use random

          implicit none
            ! function input parameters
            real(REAL64) eV
            integer np
            ! parameter: air molecular density
            real(REAL64) n
            parameter(n=3e21)
            ! return value and intermediates
            real(REAL64) collision_time, r, ct

            r = random_real()
            ct = nonrandom_collision_time(eV, np)
            collision_time = -log(r)*ct
        end
        
        function nonrandom_collision_time(eV, np)
          ! Calculates the collision time for a particle of energy eV,
          ! given a particle density of the medium with which it collides
          ! of n m^(-3).
          
          implicit none
            ! function input parameters
            real(REAL64) n, eV
            integer np
            parameter(n=3e21)
            ! function return value, and helper functions
            real(REAL64) nonrandom_collision_time
            ! intermediate quantities
            real(REAL64) alpha, v, cs

            np=np

            v = velocity(eV)
            cs = cross_section(eV,1)
            alpha = n*velocity(eV)*cross_section(eV, 1)
            nonrandom_collision_time = 1.0 / alpha
        end

        function cross_section(eV, process)
          !use interpolation, only : interp, interp_min, interp_max
          use mpi, only : rnk

          implicit none

          ! Input argument and return value
          real(REAL64) eV, cross_section
          integer process

          ! Intermediates
          real(REAL64) dx
          integer idx

          !print *, "# rnk/interp_min/interp_max", rnk, interp_min(process), interp_max(process), eV, eV .gt. interp_max(process), eV .lt. interp_min(process)


          if (eV .lt. interp_min(process) .or. eV .gt. interp_max(process)) then
            cross_section = 0
          else
            dx = interp(2,1)-interp(1,1)
            idx = 1 + int((eV-interp(1,1))/dx)
            cross_section = interp(idx,1+process)
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

        subroutine handle_collision(N, es, tcs, idx1, idx2, np, eI)
          ! Redistribute energy between the primary and secondary electrons,
          ! and update the collision times for both.
          
          ! This version is based on energy only - it is the simplest possible
          ! implementation of the rigid sphere approximation.

          implicit none
            integer N, idx1, idx2, np
            real(REAL64) es(N), tcs(N)
            real(REAL64) eI(np), ea, esec

            ! Calculate the available energy based on ionization energy
            ea = es(idx1) - eI(1)

            ! Give a random fraction of the available energy to the secondary
            esec = secondary_energy(ea)
            ! Make sure we conserve energy
            !do while (esec.gt.ea)
            !  esec = secondary_energy(ea)
            !enddo
            es(idx2) = esec
            es(idx1) = ea-es(idx2)

            ! Calculate new collision times for both electrons
            tcs(idx1) = max(collision_time(es(idx1), np), 0.0)
            tcs(idx2) = max(collision_time(es(idx2), np), 0.0)
        end
        
        function secondary_energy(ea)
          use random

          implicit none

          real(REAL64), intent(in) :: ea
          real(REAL64) secondary_energy
          real(REAL64) r, E
          parameter(E=13)

          r = random_real()
          secondary_energy = E*tan(atan(ea/E)*r)
          
        end

        function secondary_energy_simple(ea)
          use random

          implicit none

          real(REAL64), intent(in) :: ea
          real(REAL64) secondary_energy_simple

          secondary_energy_simple = ea*random_real()
        end
    
        function velocity(eV)
          ! Calculates the velocity in m/s for an electron with kinetic
          ! energy eV
          
          implicit none
            real(REAL64) eV, velocity
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
            real(REAL64) v(3), energy
            real me, e
            parameter(me=9.11E-31,e = 1.602176E-19)
            
            energy = .5*me*(v(1)*v(1) + v(2)*v(2) + v(3)*v(3)) / e
        end

      end module physics