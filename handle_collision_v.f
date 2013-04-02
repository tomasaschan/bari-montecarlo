      subroutine handle_collision(N, es, tcs, idx1, idx2)
        implicit none
          integer N, idx1, idx2
          real*8 es(N), tcs(N), collision_time, velocity, energy
          real*8 v1(3), v2(3), v0, cph, sph, th, pi, R(3)
          parameter(pi=4*atan(1.0))

          ! Scattering angles for the collision
          cph = 2*rand()-1
          sph = sqrt(1-cph*cph)
          th = 2*pi*rand()

          ! Initial velocity of the incoming electron
          ! with the ionization energy removed
          v0 = velocity(es(idx1) - 13.4)

          ! Aux values
          R = (/ cos(th)*sph, sin(th)*sph, cph /)

          ! New velocity of both electrons
          v1 = v0/2 * ((/1,0,0/) + R)
          v2 = v0/2 * ((/1,0,0/) - R)
          
          ! Set energies of both electrons
          es(idx1) = energy(v1)
          es(idx2) = energy(v2)
          
          ! Calculate new collision times
          tcs(idx1) = collision_time(es(idx1))
          tcs(idx2) = collision_time(es(idx2))
      end
      