      module populations
        use precision
        use eedf, only : Ntimes
        use physics, only : NCollProc, n

        implicit none
        private

        real(rkind), parameter :: A(2) = (/ 2.74e7, 1.53e7 /)     ! (Ac,Ab), s^-1
        real(rkind), parameter :: Q(2) = (/ 3.67e-17, 8.84e-16 /) ! (Qc, Qb), s^-1 m^3

        real(rkind), allocatable, public :: pops(:,:)
        integer, parameter      :: Nsteps = 1000
        real(rkind) h

        public :: calculate_pops, print_pops, clean_up_pops

      contains

        subroutine calculate_pops()
          use physics, only : tfin
          
          implicit none

          ! Runge-Kutta parameters
          real(rkind)             :: k1(2), k2(2), k3(2), k4(2)
          ! time variables
          real(rkind)             :: t
          integer it
          
          ! select step size
          h = tfin/real(Nsteps, rkind)

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

          real(rkind), intent(in) :: t, pops(2)
          ! return values
          real(rkind)             :: f(2)
          
          f = ne(t)*(/ ratecoeff(t, 2), ratecoeff(t, 3) /)*n - (Q*n+A)*pops
        end function f

        function ne(t)
          implicit none
          real(rkind), intent(in) :: t
          real(rkind) ne
          
          ne = 1.0
        end function ne

        subroutine clean_up_pops()
          implicit none
          deallocate(pops)
        end subroutine clean_up_pops
      end module populations
        