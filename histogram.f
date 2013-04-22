      module histogram
        use precision

        integer, parameter :: Nbins = int(1e4)
        integer, allocatable :: bins(:,:)

      contains

        subroutine init_bins(Ntimes)
          implicit none

          integer Ntimes
          
          allocate(bins(Nbins,Ntimes))

          bins = 0
        end subroutine init_bins

        subroutine calculate_totals(Ntimes, dt, tfin, e0)
          use mpi, only : rnk, reduce_bins

          implicit none

          integer Ntimes, it, i

          integer, allocatable :: binsum(:,:), totsum(:)
          real(rkind), allocatable :: norm_factor(:)

          ! variables for output
          real(rkind) t, e, p, dt, e0, tfin


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
              ! divide by the total number of particles to get probability in [0,1]
              ! multiply by 100 to get probability in %
              norm_factor = 100/real(totsum,rkind)

              do it=1, Ntimes
                do i=1, Nbins
                  t = min(dt*it, tfin)
                  e = i*e0/real(Nbins,rkind)
                  p = real(binsum(i,it),rkind)*norm_factor(it)
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