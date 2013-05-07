      module eedf
        use precision
        private

        real(rkind), allocatable :: eedfsum(:,:), totsum(:)
        real(rkind), allocatable :: norm_factor(:)
        real(rkind), allocatable, public :: eedfbins(:,:)

        integer, parameter, public    :: Needfbins = int(1e3)
        integer, public :: Ntimes
        real(rkind), public :: de


        public :: init_eedfbins
        public :: calculate_totals
        public :: print_eedf
        public :: cleanup_histogram

      contains

        subroutine init_eedfbins(Ntimes, e0)
          implicit none

          integer Ntimes
          real(rkind) e0

          allocate(eedfbins(Needfbins,Ntimes))
          de = e0/Needfbins
          eedfbins = 0
        end subroutine init_eedfbins

        subroutine calculate_totals()
          use mpi, only : rnk, reduce_bins
          implicit none

          integer ie, it

          ! reduce results to histogram
          ! master thread: allocate space for reduction
          if (rnk.eq.0) then
            allocate(eedfsum(Needfbins, Ntimes))
            allocate(totsum(Ntimes))
            allocate(norm_factor(Ntimes))
          end if

          ! calls mpi_reduce
          call reduce_bins(eedfbins,eedfsum,Needfbins,Ntimes)

          if (rnk.eq.0) then
            do it=1,Ntimes
              do ie=1,Needfbins
                eedfsum(ie,it) = normalize(ie*de, eedfsum(ie,it))
              end do
            end do
          end if 

        end subroutine calculate_totals

        function normalize(e, N)
          use physics, only : me
          implicit none

          real(rkind) e, N, normalize

          real(rkind), parameter :: pi = 3.14159265358979323846264338327950288419716
          real(rkind), save :: K = 1/(4*pi)*(me/2)**(3/2)

          normalize = N * K * 1/(de*sqrt(e))

        end function normalize

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
                e = i*de
                p = eedfsum(i,it)
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
