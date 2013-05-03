      module eedf
        use precision
        private

        integer, allocatable :: eedfsum(:,:), totsum(:)
        real(rkind), allocatable :: norm_factor(:)

        integer, parameter, public    :: Needfbins = int(1e3)
        integer, public :: Ntimes

        integer, allocatable, public  :: eedfbins(:,:)

        public :: init_eedfbins
        public :: calculate_totals
        public :: print_eedf
        public :: cleanup_histogram

      contains

        subroutine init_eedfbins(Ntimes)
          implicit none

          integer Ntimes
          
          allocate(eedfbins(Needfbins,Ntimes))
          eedfbins = 0
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

          ! master thread: calculate normalization factor
          if (rnk.eq.0) then
            totsum = sum(eedfsum,dim=1)
            ! divide by the total number of particles to get probability in [0,1]
            ! multiply by 100 to get probability in %
            norm_factor = 1.0
          end if
        end subroutine calculate_totals

        subroutine print_eedf(dt, tfin, e0)
          use mpi, only : rnk
          !use physics, only : NCollProc, cross_section
          implicit none

          integer it, i!, ip
          ! variables for output
          real(rkind) t, e, p, dt, e0, tfin

          if (rnk.eq.0) then
            do it=1, Ntimes
              do i=1, Needfbins
                t = min(dt*it, tfin)
                e = i*e0/real(Needfbins,rkind)
                p = real(eedfsum(i,it),rkind)*norm_factor(it)
                write(*,'(A,3(E15.8))') 'eedf', t,e,p !, (/ (cross_section(e, ip), ip=1, int(NCollProc)) /), (/ (e*p*cross_section(e,ip), ip=1, int(NCollProc)) /)
              end do
              write (*,*) " "
            end do
          end if
        end subroutine print_eedf

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
