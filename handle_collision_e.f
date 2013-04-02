      subroutine handle_collision(N, es, tcs, idx1, idx2, I)
        implicit none
          integer N, idx1, idx2
          real*8 es(N), tcs(N)
          real*8 I, ea, random_real, r, collision_time

          ea = es(idx1)-I

          if (ea.LT.0) then
            return
          endif

          r = random_real()
          es(idx1) = ea*r
          es(idx2) = ea-es(idx1)

          tcs(idx1) = collision_time(es(idx1))
          tcs(idx2) = collision_time(es(idx2))
C           real*8 es(N), tcs(N), collision_time, velocity, energy
C           real*8 v1(3), v2(3), v0, cph, sph, th, pi, R(3)
C           parameter(pi=4*atan(1.0))

C           ! Scattering angles for the collision
C           cph = 2*rand()-1
C           sph = sqrt(1-cph*cph)
C           th = 2*pi*rand()

C           ! Initial velocity of the incoming electron
C           v0 = velocity(es(idx1))

C           ! Aux values
C           R = (/ cos(th)*sph, sin(th)*sph, cph /)

C           ! New velocity of both electrons
C           v1 = v0/2 * ((/1,0,0/) + R)
C           v2 = v0/2 * ((/1,0,0/) - R)
          
C           ! Set energies of both electrons
C           es(idx1) = energy(v1)
C           es(idx2) = energy(v2)
          
C           ! Calculate new collision times
C           tcs(idx1) = collision_time(es(idx1))
C           tcs(idx2) = collision_time(es(idx2))


      end
      