      subroutine interpolate_2d_data(nrd,dta,nri,interp,x0,x1)
        implicit none

        ! row counts and pointers for both series
        integer nrd, nri, id, ii
        ! matrices holding x (first column) and y (second column)
        real*8, intent(in) :: dta(nrd,2)
        real*8 :: interp(nri,2)
        ! boundaries for the desired interval and interpolation step size
        real*8 x0, x1, dx
        ! some intermediate quantities, used in calculations
        real*8 k, xi, xd(nri), yd(nri)

        xd = dta(:,1)
        yd = dta(:,2)

        ! set both pointers to start of ranges
        id = 1
        ii = 1
        ! calculate step size
        dx = (x1-x0)/float(nri-1)

        !print *, "# dx: ", dx

        ! set first point of interpolation interval
        interp(1,1) = x0
        
        if (x0 .gt. xd(2)) then
          ! desired x0 is after second dta point.
          ! step the dta range so x0 is in the current interval
          print *, "# stepping dta range"

          do while(xd(id) .lt. x0)
            id = id+1
          enddo
          ! the loop above steps one step too far
          id = id - 1
        endif

        xi = x0 + (ii-1)*dx

        do while(xd(id) .lt. x1 .and. id .lt. nrd)
          k = (yd(id+1)-yd(id))/(xd(id+1)-xd(id))

          do while (xi .lt. xd(id+1) .and. ii .le. nri)
            interp(ii, 1) = xi
            interp(ii, 2) = yd(id) + k*(xi-xd(id))
            ii = ii+1
            xi = xi+dx
          enddo

          id = id+1
!          print *, "# ", ii, x0, dta(id-1,1), xi, dta(id,1), x1
        enddo

        ! if there is some part of the interval left, extrapolate to x1
        do while (xi .le. x1 .and. ii .le. nri)
          ! use the same k as in the last interval
          interp(ii, 1) = xi
          interp(ii, 2) = yd(id) + k*(xi-xd(id))
          xi = x0 + ii*dx
          ii = ii+1
        enddo
      end