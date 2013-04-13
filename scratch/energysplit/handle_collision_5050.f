      subroutine handle_collision(N, es, tcs, idx1, idx2, eI)
C       Redistribute energy between the primary and secondary electrons,
C       and update the collision times for both.
        
C       This version is based on energy only - it is the simplest possible
C       implementation of the rigid sphere approximation.

        implicit none
          integer N, idx1, idx2
          real*8 es(N), tcs(N)
          real*8 eI, ea, random_real, r, collision_time

C         Calculate the available energy based on ionization energy
          ea = es(idx1) - eI

          if (ea .lt. 0) then
              return
          endif
C         Give a random fraction of the available energy to the secondary
          r = 0.5
          es(idx1) = ea*r
          es(idx2) = ea-es(idx1)

C           if (ea + eI .GT. 295) then
C             print *, es(idx1), es(idx2), es(idx1)+es(idx2)
C           endif

C         Calculate new collision times for both electrons
          tcs(idx1) = collision_time(es(idx1))
          tcs(idx2) = collision_time(es(idx2))

      end
      