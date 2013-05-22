      module populations
        use, intrinsic :: iso_fortran_env, only : REAL64, INT16, INT32, INT64
        use eedf, only : Ntimes
        use physics, only : NCollProc, n
        use fparser

        implicit none
        private

        real(REAL64), allocatable :: A(:)
        real(REAL64), allocatable :: Q(:)

        real(REAL64), allocatable, public :: pops(:,:)
        integer, parameter      :: Nsteps = 1000
        real(REAL64) h

        character(len=120), public :: neexpr

        integer, public :: Npops

        public :: init_pops, calculate_pops, print_pops, clean_up_pops

      contains

        subroutine init_pops()
          use io, only : Araw, Qraw
          implicit none
          
          allocate(A(Npops))
          allocate(Q(Npops))

          A = Araw
          Q = Qraw*1e-6 ! convert to m^3 s^-1 (was cm^3 s^-1)

          call initf(1)
          call parsef(1, neexpr, (/ 't' /))

        end subroutine init_pops

        subroutine calculate_pops()
          use physics, only : tfin
          
          implicit none

          ! Runge-Kutta parameters
          real(REAL64)             :: k1(Npops), k2(Npops), k3(Npops), k4(Npops)
          ! time variables
          real(REAL64)             :: t
          integer it
          
          ! select step size
          h = tfin/real(Nsteps, REAL64)

          allocate(pops(0:Nsteps+1, Npops))

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
          use io, only : pops_f
          implicit none

          integer it

          open(pops_f,file='pops.dat')

          do it=0, Nsteps
            write(pops_f,*) it*h, pops(it,:)
          end do

          close(pops_f)
        end subroutine print_pops

        function f(t,pops)
          use ratecoeffs, only : ratecoeff
          implicit none 

          real(REAL64), intent(in) :: t, pops(Npops)
          ! return values
          real(REAL64)             :: f(Npops)
          real(REAL64)             :: ne
          integer ip
          
          ne = evalf(1, (/ t /))

          f = ne*(/ (ratecoeff(t, ip),ip=2,Npops+1) /)*n - (Q*n+A)*pops
        end function f

        subroutine clean_up_pops()
          implicit none
          deallocate(pops,A,Q)
        end subroutine clean_up_pops
      end module populations
        