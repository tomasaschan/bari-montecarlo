      module populations
        use, intrinsic :: iso_fortran_env, only : REAL64, INT16, INT32, INT64
        use eedf, only : Ntimes
        use physics, only : NCollProc, n

        implicit none
        private

        real(REAL64), parameter :: A(2) = (/ 2.74e7, 1.53e7 /)     ! (Ac,Ab), s^-1
        real(REAL64), parameter :: Q(2) = (/ 3.67e-17, 8.84e-16 /) ! (Qc, Qb), s^-1 m^3

        real(REAL64), allocatable, public :: pops(:,:)
        integer, parameter      :: Nsteps = 1000
        real(REAL64) h

        public :: calculate_pops, print_pops, clean_up_pops

      contains

        subroutine calculate_pops()
          use physics, only : tfin
          
          implicit none

          ! Runge-Kutta parameters
          real(REAL64)             :: k1(2), k2(2), k3(2), k4(2)
          ! time variables
          real(REAL64)             :: t
          integer it
          
          ! select step size
          h = tfin/real(Nsteps, REAL64)

          allocate(pops(0:Nsteps+1, 2))

          pops = 0.
          t = 0
          it = 0

          do while (t .le. tfin)

            k1 = h*f(t, pops(it,:))
            k2 = h*f(t+h/2., pops(it,:) + k1/2.)
            k3 = h*f(t+h/2., pops(it,:) + k2/2.)
            k4 = h*f(t+h, pops(it,:) + k3)

            pops(it+1,:) = pops(it,:) + 1/6. * (k1+2*k2+2*k3+k4)

            t = t+h
            it = it+1
          end do
        end subroutine calculate_pops

        subroutine print_pops()
          implicit none

          integer it

          do it=0, Nsteps
            write(*,'((A),3(E14.6))') "pops", it*h, pops(it,:)
          end do
          
        end subroutine print_pops

        function f(t,pops)
          use ratecoeffs, only : ratecoeff
          implicit none 

          real(REAL64), intent(in) :: t, pops(2)
          ! return values
          real(REAL64)             :: f(2)
          
          f = ne(t)*(/ ratecoeff(t, 2), ratecoeff(t, 3) /)*n - (Q*n+A)*pops
        end function f

        function ne(t)
          implicit none
          real(REAL64), intent(in) :: t
          real(REAL64) ne
          
          ne = 1.0
        end function ne

        subroutine clean_up_pops()
          implicit none
          deallocate(pops)
        end subroutine clean_up_pops
      end module populations
        