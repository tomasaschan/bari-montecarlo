      module interpolation
        use precision
        use physics, only : cs, cs_min, cs_max, Ncs, NCollProc, products

        integer(lkind) nri
        parameter(nri=int(1e5))

      contains

        subroutine init_interpolation()
          use io, only : NDataFiles, nrd, e0raw, e1raw, productsraw
          !use mpi, only : rnk
          
          implicit none

          Ncs = nri
          NCollProc = NDataFiles
          allocate(cs(Ncs,NCollProc+1))
          allocate(cs_min(NCollProc))
          allocate(cs_max(NCollProc))
          allocate(products(NCollProc))
          
          cs_min = e0raw
          cs_max = e1raw
          products  = productsraw

        end subroutine init_interpolation

        subroutine interpolate(ip)
          use io, only : nrd, raw, e0raw, e1raw

          implicit none

          ! pointers for both series
          integer(lkind) ip, id, ii, i
          ! boundaries for the desired interval
          real(rkind) e0, e1
          ! shortcuts to the interpolation and data ranges
          real(rkind) ei(nri), ed(nrd(ip)), csd(nrd(ip))
          ! variables for interpolation
          real(rkind) k, de, emin, emax

          ed = raw(1:nrd(ip),1,ip)
          csd = raw(1:nrd(ip),2,ip)

          e0 = cs_min(ip)
          e1 = cs_max(ip)

          emax = max(maxval(cs_max),e0)
          emin = min(minval(cs_min),0.0)
          de = (emax-emin)/(float(nri)-1)

          do i=1,nri
            cs(i,1) = emin+(i-1)*de
          end do

          ei = cs(:,1)

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

            do while (cs(ii,1) .lt. e0)
              ii = ii + 1
              cs(ii,ip+1) = 0
            end do
            ! the loop above steps one step too far
            ii = ii - 1
          end if

          k=0

          ! interpolate until end of data range
          do while(ed(id) .lt. e1 .and. id .lt. nrd(ip))
            k = (csd(id+1)-csd(id))/(ed(id+1)-ed(id)) 
            !print *, "# k:", ip, cs(ii,1), k, ed(id), csd(id), csd(id+1)

            do while(ei(ii) .lt. ed(id+1) .and. ii .lt. nri)
              cs(ii,ip+1) = csd(id) + k*(ei(ii)-ed(id))
              ii = ii+1
            end do

            id = id + 1
          end do

          ! if a larger range is desired, extrapolate
          ! this loop syntax is funky to avoid off-by-one errors, because
          ! fortran does not guarantee execution order of if conditions
          extrapolate: do !while (ii .le. nri .and. ei(ii) .le. e1)
            if (ii .gt. nri) exit extrapolate
            if (ei(ii) .gt. e1) exit extrapolate

              ! we re-use k from the last interval
              cs(ii, ip+1) = csd(id) + k*(ei(ii)-ed(id))
              ii = ii + 1
          end do extrapolate

          ! if some other interpolation range is larger, pad with 0
          do while (ii .lt. nri)
            cs(ii+1,ip+1) = 0
            ii = ii + 1
          end do

        end subroutine interpolate

        subroutine clean_up_interp()
          implicit none

          deallocate(cs)
          deallocate(cs_min)
          deallocate(cs_max)
        end subroutine clean_up_interp

      end module interpolation

