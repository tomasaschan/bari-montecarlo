      function collision_time(n, eV)
        implicit none
          ! function input parameters and log(energy)
          real*8 n, eV, logeV
          ! function return value, and intermediates cs, v and alpha
          real*8 collision_time, cross_section, alpha, velocity

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
                    
          cross_section = exp(k1 + k2*logeV + k3*logeV*logeV)*1E-4
          alpha = n*velocity(eV)*cross_section
          
          collision_time = -log(rand()) / alpha
c~           print *, collision_time
      end

      function velocity(eV)
          real*8 eV, velocity
          real me, e
          parameter(me=9.11E-31,e = 1.602176E-19)

          velocity = sqrt(2*eV*e/me)
      end
