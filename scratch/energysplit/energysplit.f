      program energysplit
        implicit none

        integer seed, i, N
        parameter(N = 500000)
        real*8 xi1, xi2, z, E0
        parameter(E0 = 1e3)
        real*8 a2, random_real, pi
        parameter(a2 = 13*13, pi=4*atan(1.0))

        seed = 0
        call seed_rand(seed)

        do i = 1, N

110       xi1 = random_real()*E0
          xi2 = random_real()
          
          if (xi2*(a2+xi1*xi1).gt.a2) goto 110

          z = xi1

          write(*, '(F18.13)') z
        enddo

      end