      module eedf
        use precision
        private

        integer, allocatable :: eedfsum(:,:), totsum(:)
        real(rkind), allocatable :: norm_factor(:)

        integer, parameter, public    :: Needfbins = int(1e3)
        integer, public :: Ntimes
        real(rkind), public :: de

        integer, allocatable, public  :: eedfbins(:,:)

        public :: init_eedfbins, cleanup_histogram
        public :: calculate_totals, print_eedf
        public :: idx2energy, energy2idx

      contains

        subroutine init_eedfbins(Ntimes, e0)
          implicit none

          integer Ntimes
          real(rkind) e0
          
          allocate(eedfbins(Needfbins,Ntimes))
          eedfbins = 0

          de = e0/real(Needfbins,rkind)
        end subroutine init_eedfbins

        subroutine calculate_totals()
          use mpi, only : rnk, reduce_bins
          implicit none

          ! reduce results to histogram
          ! master thread: allocate space for reduction
          if (rnk.eq.0) then
            allocate(eedfsum(Needfbins, Ntimes))
            allocate(totsum(Ntimes))
            allocate(norm_factor(Ntimes))
          end if

          ! calls mpi_reduce
          call reduce_bins(eedfbins,eedfsum,Needfbins,Ntimes)
        end subroutine calculate_totals

        subroutine print_eedf(dt, tfin)
          use mpi, only : rnk
          !use physics, only : NCollProc, cross_section
          implicit none

          integer it, i!, ip
          ! variables for output
          real(rkind) t, e, p, dt, tfin

          if (rnk.eq.0) then
            do it=1, Ntimes
              do i=1, Needfbins
                t = min(dt*it, tfin)
                e = idx2energy(i)
                p = normalize(e, eedfsum(i,it))
                write(*,'(A,3(E15.8))') 'eedf', t,e,p !, (/ (cross_section(e, ip), ip=1, int(NCollProc)) /), (/ (e*p*cross_section(e,ip), ip=1, int(NCollProc)) /)
              end do
              write (*,*) " "
            end do
          end if
        end subroutine print_eedf

        function normalize(e, N)
          implicit none

          real(rkind) e, normalize
          integer N

          normalize = sqrt(e)*de*N
        end function

        function energy2idx(e)
          implicit none
          real(rkind) e
          integer energy2idx

          energy2idx = int(e/de)
        end function energy2idx

        function idx2energy(idx)
          implicit none
          real(rkind) idx2energy
          integer idx

          idx2energy = idx*de
        end function idx2energy

        subroutine cleanup_histogram()
          use mpi, only : rnk
          implicit none
          deallocate(eedfbins)

          if (rnk.eq.0) then

            deallocate(totsum)
            deallocate(norm_factor)
            deallocate(eedfsum)
          end if
        end subroutine

      end module eedf
