      module single_particle
        use precision
        
        ! Some parameters for simulation
        integer head, colls_dt, tidx
        integer Nspace
        parameter(Nspace=100)
        real(rkind) es(Nspace), tcs(Nspace), e0

      contains

        subroutine onepart(dt, tfin, Ntimes)
          !use physics
        
          implicit none

          real(rkind) t, dt, tfin
         
          ! How many particles we allocate space for

          integer Ntimes

          ! Initialize simulation
          t = 0.0_rkind
          tidx = 1
          head = 1
          call init_arrays(Nspace, es, e0, tcs)

          ! Run simulation of one electron
          do while (tidx .le. Ntimes .and. t .le. tfin)
            ! reset collision counter
            colls_dt = 0            
            ! propagate for dt
            call propagate(dt, head)
            ! collect snapshot histogram
            call collect(tidx, head)
            ! update current time and index
            t = t+dt
            tidx = tidx + 1
          enddo
            
          ! if tfin/dt is not even, propagate to tfin
          if (t .lt. tfin) then
            call propagate(tfin-(t-dt), head)
            ! collect final value
            call collect(tidx-1, head)
          end if
        end subroutine onepart

        subroutine init_arrays(N, es, e0, tcs)
          use physics, only : collision_time
  
          implicit none
          
          integer N
          real(rkind) es(N), tcs(N), e0

          es = 0.0D0
          tcs = 0.0D0
          
          es(1) = e0
          tcs(1) = collision_time()
        end subroutine init_arrays

        subroutine propagate(dt, head)
          use physics, only : handle_collision

          implicit none

          real(rkind) dt, dtp
          integer i, head

          do i=1, head
            dtp = dt
            do while(tcs(i) .le. dtp .and. tcs(i) .gt. 0)
              dtp = dtp - tcs(i)
              call handle_collision(Nspace,es,tcs,i,head,colls_dt)
            enddo
            
            tcs(i) = tcs(i) - dtp
          enddo

          ! update head to include new collisions
          head = head + colls_dt

        end subroutine propagate

        subroutine collect(tidx, head)
          use eedf, only : eedfbins, Needfbins, de

          implicit none

          integer i, idx, tidx, head

          do i=1, head
            idx = int(es(i)/de)
            if (idx .eq. 0) then
              idx = 1
            end if
            eedfbins(idx, tidx) = eedfbins(idx, tidx) + 1
          enddo

        end subroutine collect

      end module single_particle

