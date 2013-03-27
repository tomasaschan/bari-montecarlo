      subroutine simulate_single_electron(ni, energy, tfin, dt)
        implicit none

          ! Input parameters
          real*8 ni, energy, tfin, dt, t

          ! Function to calculate collision time
          real*8 collision_time
          
          ! Parameters for simulation
          real*8 velocity, pos, tc, dtp

          ! Electron mass
          real me
          parameter(me=9.11E-31)

          do while (t .LT. tfin)
              t = t + dt
              tc = collision_time(ni, energy)
              print *, t, energy, tc
          enddo
      end

      function collision_time(ni, eV)
        implicit none
          ! function input parameters and energy in Joules
          real*8 ni, eV, J, logeV
          ! function return value, and intermediates cs, v and alpha
          real*8 collision_time, cross_section, v, alpha

          ! elementary charge, for conversions Joule <-> eV
          real e
          parameter(e = 1.602176E-19)
          ! Electron mass
          real me
          parameter(me=9.11E-31)
          ! Parameters from curve fitting
          real k1, k2, k3
          parameter(k1=-65.5784259,k2=13.077650,k3=-1.41988776)
          
          logeV = log(eV)
          J = eV*e
          v = sqrt(2*J/me)
          
          cross_section = exp(k1 + k2*logeV + k3*logeV*logeV)
          alpha = ni*v*cross_section
          
          collision_time = -log(rand()) / alpha
      end
