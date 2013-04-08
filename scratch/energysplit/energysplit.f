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

      subroutine energysplit2()
        implicit none

        integer i, N, ierr
        parameter(N=1e6)
        real*8 ea, secondary_energy
        parameter(ea=2e3)

        call MPI_Init(ierr)

        do i=1, N
          print *, secondary_energy(ea)
        enddo

        call MPI_Finalize(ierr)


      end

      function secondary_energy(ea)
        implicit none

        real*8, intent(in) :: ea
        real*8 secondary_energy
        real*8 r1, r2, a2, random_real
        parameter(a2=13*13)
        r1 = 0
        r2 = 0

        do while (r2*(a2+r1*r1).lt.a2)
          r1 = random_real()*ea
          r2 = random_real()

          secondary_energy = r1
        end do

      end