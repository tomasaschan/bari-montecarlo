      subroutine interpolate_2d_data(nrd,data,nri,interp,x0,x1)
        implicit none

        ! row counts and pointers for both series
        integer nrd, nri, id, ii
        ! matrices holding x (first column) and y (second column)
        real*8, intent(in) :: data(nrd,2)
        real*8 :: interp(nri,2)
        ! boundaries for the desired interval and interpolation step size
        real*8 x0, x1, dx
        ! some intermediate quantities, used in calculations
        real*8 k, xi

        ! set both pointers to start of ranges
        id = 1
        ii = 1
        ! calculate step size
        dx = (x1-x0)/float(nri-1)

        print *, "# dx: ", dx

        ! set first point of interpolation interval
        interp(1,1) = x0
        
        if (x0 .lt. data(1,1)) then
          ! desired x0 is outside of range.

          ! extrapolate so we get an (x0,y0)-pair
          print *, "# TODO: extrapolate to x0"
        elseif (x0 .eq. data(1,1)) then
          ! start value is the same - we can use it directly
          interp(1,2) = data(1,2)
          ii = ii+1
          id = id+1
        else
          ! desired x0 is inside first data point.

          ! step the data range up to just before x0
          do while(data(id,1) .lt. x0)
            id = id+1
          enddo
          ! the loop above steps one step too far
          id = id - 1
        endif

        ! the x value for the first data point we'll interpolate to
        xi = x0 + (ii-1)*dx

!        print *, "# ", ii, x0, data(id-1,1), xi, data(id,1), x1

        do while(data(id,1) .lt. x1 .and. id .lt. nrd)
          !print *, "# ", ii, id

          k = (data(id+1,2)-data(id,2))/(data(id+1,1)-data(id,1))

          do while (xi .lt. data(id, 1))
            interp(ii, 1) = xi
            interp(ii, 2) = k*(xi-data(id,1)) + data(id,2)
            ii = ii+1
            xi = x0 + (ii-1)*dx
          enddo

          id = id+1
!          print *, "# ", ii, x0, data(id-1,1), xi, data(id,1), x1
        enddo

        ! if there is some part of the interval left, extrapolate to x1
        do while (xi .lt. x1)
          ! use the same k as in the last interval
          interp(ii,1) = xi
          interp(ii,2) = k*(xi-data(id,1))+data(id,2)
          xi = x0 + ii*dx
          ii = ii+1
        enddo

!        print *, "# ", ii, x0, data(id-1,1), xi, data(id,1), x1

!        print *, "# dx: ", dx, ", len: ", dx*nri

        if (x1 .gt. data(size(data,1),1)) then 
          print *, "# TODO: extrapolate to x1"
        endif



      end