      module ratecoeffs
        use precision
        private

        real(rkind), allocatable, public :: k(:)
      contains

        subroutine calculate_ratecoeffs(e0, Ntimes)
          use histogram, only : bins, Nbins
          use physics, only : n, NCollProc, cross_section, me

          implicit none

          integer ip, ie, it
          integer(lkind) Ntimes
          real(rkind) I, e0, de, fa, fb

          allocate(k(NCollProc))


          de = e0/real(Nbins,rkind)

          ! only at last instant in time, for now
          it = int(Ntimes)

          do ip=1,int(NCollProc)
            ! calculate rate coefficient for process i, and put it in k(i)

            I = 0
            ! integrate \int e*f(e)*cs(e)*de using the trapezoidal rule
            do ie=1, Nbins-1
              fa = ie*bins(ie,it)*cross_section(ie*de, ip)
              fb = (ie+i)*bins(ie+1,it)*cross_section((ie+1)*de,ip)
              I = I + 0.5*(fa + fb)
            end do
            

            k(ip) = sqrt(2/me) * de**2/2 * I

          end do

          print *, k

        end subroutine calculate_ratecoeffs
        

        subroutine cleanup_ratecoeffs()
          implicit none
          deallocate(k)
        end subroutine cleanup_ratecoeffs

      end module ratecoeffs