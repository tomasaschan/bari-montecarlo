      module interpolation
        use io
        use iso_fortran_env, only : REAL64

        integer nri
        parameter(nri=int(1e5))

        real(REAL64), allocatable :: interp(:,:)
        real(REAL64), allocatable :: interp_min(:), interp_max(:)

      contains

        subroutine init_interpolation(e0)
          !use mpi, only : rnk
          
          implicit none

          integer i
          double precision, intent(in) :: e0
          double precision de, emax, emin

          allocate(interp(nri,NDataFiles+1))
          allocate(interp_min(NDataFiles))
          allocate(interp_max(NDataFiles))

          interp_min = e0raw
          interp_max = e1raw

          emax = max(maxval(interp_max),e0)
          emin = min(minval(interp_min),0.0)
          de = (emax-emin)/(float(nri)-1)

          do i=1,nri
            interp(i,1) = emin+(i-1)*de
          end do
        end subroutine init_interpolation

        subroutine interpolate(inti, e0, e1)
          implicit none

          ! pointers for both series
          integer inti, id, ii
          ! boundaries for the desired interval
          double precision e0, e1
          ! shortcuts to the interpolation and data ranges
          double precision ei(nri), ed(nrd), csd(nrd)
          ! slope of interpolation
          double precision k


          ei = interp(:,1)
          ed = raw(:,1)
          csd = raw(:,2)

          ! set both pointers to start of ranges
          id = 1
          ii = 1

          ! step data range to starting point
          if (e0 .gt. ed(2)) then
            ! desired e0 is after second data point.
            ! step the data range so x0 is in the current interval
            
            do while(ed(id) .lt. e0)
              id = id+1
            end do
            ! the loop above steps one step too far
            id = id - 1
          endif

          ! step interpolation range to starting point
          if (e0 .gt. ei(2)) then
            ! desired x0 is after second interpolation point
            ! step the interpolation range so x0 is in the first interval
            ! set cs = 0 for all e < e0

            do while (ei(ii) .lt. e0)
              ii = ii + 1
              interp(ii,inti+1) = 0
            end do
            ! the loop above steps one step too far
            ii = ii - 1
          end if

          k=0

          ! interpolate until end of data range
          do while(ed(id) .lt. e1 .and. id .lt. nrd)
            k = (csd(id+1)-csd(id))/(ed(id+1)-ed(id)) 

            do while(ei(ii) .lt. ed(id+1) .and. ii .lt. nri)
              interp(ii,inti+1) = csd(id) + k*(ei(ii)-ed(id))
              ii = ii+1
            end do

            id = id + 1
          end do

          ! if a larger range is desired, extrapolate
          if (ii .lt. nri) then
            do while (ei(ii) .le. e1 .and. ii .lt. nri)
              ! we re-use k from the last interval
              interp(ii, inti+1) = csd(id) + k*(ei(ii)-ed(id))
              ii = ii + 1
            end do
          end if

          ! if some other interpolation range is larger, pad with 0
          do while (ii .lt. nri)
            interp(ii,inti+1) = 0
            ii = ii + 1
          end do

          deallocate(raw)
        end subroutine interpolate

      end module interpolation

