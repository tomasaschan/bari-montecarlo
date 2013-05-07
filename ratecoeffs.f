      module ratecoeffs
        use precision
        private

        real(rkind), allocatable, public :: k(:,:)

        public :: calculate_ratecoeffs_evolution
        public :: print_ratecoeffs
        public :: clean_up_ratecoeffs
      contains

        subroutine calculate_ratecoeffs_evolution()
          use eedf, only : Needfbins, Ntimes
          use physics, only : NCollProc
          implicit none

          integer it

          allocate(k(Ntimes,NCollProc))

          do it=1,Ntimes
            call calculate_ratecoeffs(it)
          end do

        end subroutine calculate_ratecoeffs_evolution


        subroutine print_ratecoeffs(dt, tfin)
          use eedf, only : Ntimes
          
          implicit none

          real(rkind), intent(in) :: dt, tfin
          real(rkind) t
          integer :: it

          do it = 1, Ntimes
            t = min(dt*it,tfin)
            write(*,'(A,4(E15.8))'), 'rate', t, k(it,:)!/k(1,:)
          end do
        end subroutine print_ratecoeffs

        subroutine calculate_ratecoeffs(it)
          use eedf, only : de, Needfbins
          use physics, only : NCollProc, cross_section, me

          implicit none

          integer, intent(in) :: it

          integer ip, ie
          real(rkind) I, fa, fb

          do ip=1,int(NCollProc)
            ! calculate rate coefficient for process i, and put it in k(i)

            I = 0
            ! integrate \int e*f(e)*cs(e)*de using the trapezoidal rule
            do ie=1, Needfbins-1
              fa = integrand(ip, it, ie)
              fb = integrand(ip, it, ie+1)

              I = I + 0.5*(fa + fb)
            end do
            

            k(it,ip) = sqrt(2/me) * (de)/2 * I

          end do
        end subroutine calculate_ratecoeffs


        function integrand(ip, it, ie)
          use eedf, only : eedfbins, de
          use physics, only : cross_section

          implicit none

          integer, intent(in) :: ie, ip, it
          real(rkind) :: integrand

          integrand = (ie*de)*eedfbins(ie,it)*cross_section(ie*de,ip)
                      ! e        f(e)               sigma(e)         
        end function integrand

        subroutine clean_up_ratecoeffs()
          implicit none
          deallocate(k)
        end subroutine clean_up_ratecoeffs

      end module ratecoeffs