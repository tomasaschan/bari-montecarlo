      module histogram
        use iso_fortran_env, only : REAL64

        integer, parameter :: Nbins = int(1e3)
        integer, allocatable :: bins(:,:)

      contains

        subroutine init_bins(Ntimes)
          implicit none

          integer Ntimes
          
          allocate(bins(Nbins,Ntimes))

          bins = 0
        end subroutine init_bins

        subroutine calculate_totals(Ntimes, e0)
          use mpi

          implicit none

          integer Ntimes, it, i

          integer, allocatable :: binsum(:,:), totsum(:)
          real(REAL64), allocatable :: norm_factor(:)

          ! variables for output
          real(REAL64) t, e, p, dt, e0


          ! reduce results to histogram
            ! master thread: allocate space for reduction
            if (rnk.eq.0) then
              allocate(binsum(Nbins, Ntimes))
              allocate(totsum(Ntimes))
              allocate(norm_factor(Ntimes))
            end if
            call reduce_bins(bins,binsum,Nbins,Ntimes)
          !

          ! master thread: print histogram data to stdout
            if (rnk.eq.0) then
              totsum = sum(binsum,dim=1)
              norm_factor = 100/real(totsum,REAL64)

              dt = bins(2,1)-bins(1,1)

              do it=1, Ntimes
                do i=1, Nbins
                  t = dt*it
                  e = i*e0/real(Nbins,REAL64)
                  p = real(binsum(i,it),REAL64)!*norm_factor(it)
                  write(*,'(3(E15.8))') t,e,p
                end do
                write (*,*) " "
              end do

              deallocate(totsum)
              deallocate(norm_factor)
              deallocate(binsum)
            end if
          !
        end subroutine calculate_totals



      end module