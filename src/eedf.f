      module eedf
        use, intrinsic :: iso_fortran_env, only : REAL64, INT16, INT32, INT64
        private

        real(REAL64), allocatable :: eedfsum(:,:), totsum(:)
        real(REAL64), allocatable :: norm_factor(:)
        real(REAL64), allocatable, public :: eedfbins(:,:)

        integer, parameter, public    :: Needfbins = int(5e2)
        integer, public :: Ntimes
        real(REAL64), public :: de


        public :: init_eedfbins
        public :: calculate_totals
        public :: print_eedf
        public :: cleanup_histogram

      contains

        subroutine init_eedfbins(e0)
          implicit none

          real(REAL64) e0

          allocate(eedfbins(Needfbins,0:Ntimes))
          de = e0/Needfbins
          eedfbins = 0
        end subroutine init_eedfbins

        subroutine calculate_totals()
          use physics, only : dt
          implicit none

          integer ie, it
          
          do it=0,Ntimes
            ! normalize entire eedf to 1
            eedfbins(:,it) = eedfbins(:,it)/sum(eedfbins(:,it))

            ! normalize to f(e) according to paper
            do ie=1,Needfbins
              eedfbins(ie,it) = normalize(ie*de, eedfbins(ie,it))
            end do
          end do
        end subroutine calculate_totals

        function normalize(e, N)
          use physics, only : me
          implicit none

          real(REAL64) e, N, normalize

          real(REAL64), parameter :: pi = 3.14159265358979323846264338327950288419716
          real(REAL64), save :: K = 1 !/(4*pi)*(me/2)**(3/2)

          normalize = N * K * 1/(de*sqrt(e))

        end function normalize

        subroutine print_eedf()
          use physics, only : dt, tfin
          use io, only : eedf_f
          implicit none

          integer it, i
          ! variables for output
          real(REAL64) t, e, p

          open(eedf_f, FILE="eedf.dat")

          do it=0, Ntimes
            do i=1, Needfbins
              t = min(dt*it, tfin)
              e = i*de
              p = eedfbins(i,it)
              write(eedf_f,'(3(E15.8))') t, e, p ! time, energy, probability
            end do
          end do

          close(eedf_f)
        end subroutine print_eedf

        subroutine cleanup_histogram()
          implicit none
          deallocate(eedfbins)
        end subroutine

      end module eedf
