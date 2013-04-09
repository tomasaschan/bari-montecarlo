      subroutine handle_collision(N, es, tcs, idx1, idx2, eI)
C       Redistribute energy between the primary and secondary electrons,
C       and update the collision times for both.
        
C       This version is based on energy only - it is the simplest possible
C       implementation of the rigid sphere approximation.

        implicit none
          integer N, idx1, idx2
          real*8 es(N), tcs(N)
          real*8 eI, ea, esec, collision_time, secondary_energy

C         Calculate the available energy based on ionization energy
          ea = es(idx1) - eI

C         Give a random fraction of the available energy to the secondary
          esec = secondary_energy(ea)
          ! Make sure we conserve energy
          !do while (esec.gt.ea)
          !  esec = secondary_energy(ea)
          !enddo
          es(idx2) = max(esec,ea)
          es(idx1) = ea-es(idx2)

C         Calculate new collision times for both electrons
          tcs(idx1) = max(collision_time(es(idx1)), 0.0)
          tcs(idx2) = max(collision_time(es(idx2)), 0.0)
      end
      
      function secondary_energy(ea)
        implicit none

        real*8, intent(in) :: ea
        real*8 secondary_energy
        real*8 r1, r2, E2, random_real
        parameter(E2=13*13)
        r1 = 0
        r2 = 0

        do while (r2*(E2+r1*r1).lt.E2)
          r1 = random_real()*ea
          r2 = random_real()

          secondary_energy = r1
        end do

      end