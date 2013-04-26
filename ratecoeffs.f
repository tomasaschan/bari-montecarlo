      module ratecoeffs
        use precision
        private

        real(rkind), allocatable, public :: k(:,:)

        public :: calculate_ratecoeffs_evolution
        public :: print_ratecoeffs
        public :: clean_up_ratecoeffs
      contains

        subroutine calculate_ratecoeffs_evolution(e0)
          use histogram, only : Nbins, Ntimes
          use physics, only : NCollProc
          implicit none

          real(rkind), intent(in) :: e0
          integer it

          allocate(k(Ntimes,NCollProc))

          do it=1,Ntimes
            call calculate_ratecoeffs(e0,it)
          end do

        end subroutine calculate_ratecoeffs_evolution


        subroutine print_ratecoeffs(dt, tfin)
          use histogram, only : Ntimes
          use physics, only : NCollProc
          
          implicit none

          real(rkind), intent(in) :: dt, tfin
          real(rkind) t
          integer :: it

          do it = 1, Ntimes
            t = min(dt*it,tfin)
            write(*,'(A,4(E15.8))'), 'rate', t, k(it,:)!/k(it,1)
          end do
        end subroutine print_ratecoeffs

        subroutine calculate_ratecoeffs(e0, it)
          use histogram, only : bins, Nbins
          use physics, only : n, NCollProc, cross_section, me

          implicit none

          real(rkind), intent(in) :: e0
          integer, intent(in) :: it

          integer ip, ie
          real(rkind) I, de, fa, fb


          de = e0/real(Nbins,rkind)

          do ip=1,int(NCollProc)
            ! calculate rate coefficient for process i, and put it in k(i)

            I = 0
            ! integrate \int e*f(e)*cs(e)*de using the trapezoidal rule
            do ie=1, Nbins-1
              fa = ie*bins(ie,it)*cross_section(ie*de, ip)
              fb = sqrt((ie+i)*de)*bins(ie+1,it)*cross_section((ie+1)*de,ip)
              I = I + 0.5*(fa + fb)
            end do
            

            k(it,ip) = sqrt(2/me) * (de)/2 * I

          end do
        end subroutine calculate_ratecoeffs


        subroutine clean_up_ratecoeffs()
          implicit none
          deallocate(k)
        end subroutine clean_up_ratecoeffs

      end module ratecoeffs