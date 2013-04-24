      module histogram
        use precision
        private

        integer, allocatable :: binsum(:,:), totsum(:)
        real(rkind), allocatable :: norm_factor(:)

        integer, parameter, public    :: Nbins = int(1e3)
        integer, public :: Ntimes

        integer, allocatable, public  :: bins(:,:)

        public :: init_bins
        public :: calculate_totals
        public :: print_eedf
        public :: cleanup_histogram

      contains

        subroutine init_bins(Ntimes)
          implicit none

          integer Ntimes
          
          allocate(bins(Nbins,Ntimes))
          bins = 0
        end subroutine init_bins

        subroutine calculate_totals()
          use mpi, only : rnk, reduce_bins
          implicit none

          ! reduce results to histogram
          ! master thread: allocate space for reduction
          if (rnk.eq.0) then
            allocate(binsum(Nbins, Ntimes))
            allocate(totsum(Ntimes))
            allocate(norm_factor(Ntimes))
          end if

          ! calls mpi_reduce
          call reduce_bins(bins,binsum,Nbins,Ntimes)

          ! master thread: calculate normalization factor
          if (rnk.eq.0) then
            totsum = sum(binsum,dim=1)
            ! divide by the total number of particles to get probability in [0,1]
            ! multiply by 100 to get probability in %
            norm_factor = 100/real(totsum,rkind)
          end if
        end subroutine calculate_totals

        subroutine print_eedf(dt, tfin, e0)
          use mpi, only : rnk
          implicit none

          integer it, i
          ! variables for output
          real(rkind) t, e, p, dt, e0, tfin

          if (rnk.eq.0) then
            do it=1, Ntimes
              do i=1, Nbins
                t = min(dt*it, tfin)
                e = i*e0/real(Nbins,rkind)
                p = real(binsum(i,it),rkind)*norm_factor(it)
                write(*,'(3(E15.8))') t,e,p
              end do
              write (*,*) " "
            end do
          end if
        end subroutine print_eedf

        subroutine cleanup_histogram()
          use mpi, only : rnk
          implicit none
          deallocate(bins)

          if (rnk.eq.0) then

            deallocate(totsum)
            deallocate(norm_factor)
            deallocate(binsum)
          end if
        end subroutine

      end module