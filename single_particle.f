      module single_particle
        use iso_fortran_env, only : REAL64

      contains

        subroutine onepart(e0, dt, Ntimes, np, eI)
          use physics
          use histogram
        
          implicit none

          ! How many particles we allocate space for
          integer Nspace, Ntimes, np
          parameter(Nspace=100)
          
          ! Some parameters for simulation
          integer i, head, colls_dt, idx, tidx
          real(REAL64) e0, t, dt, dtp, eI(np)
          real(REAL64) es(Nspace), tcs(Nspace)
         
          ! Initialize simulation
          t = 0
          tidx = 1
          head = 1
          call init_arrays(Nspace, np, es, e0, tcs)

          ! Run simulation of one electron
          do while (tidx .le. Ntimes)
            colls_dt = 0
            
            do i=1, head
              dtp = dt
              do while(tcs(i) .LE. dtp .AND. tcs(i) .GT. 0)
                dtp = dtp - tcs(i)
                colls_dt = colls_dt + 1
                call handle_collision(Nspace,es,tcs,i,head+colls_dt,np,eI(np))
              enddo
              
              tcs(i) = tcs(i) - dtp
            enddo
          
            head = head + colls_dt
            t = t+dt

            do i=1, head
              idx = ceiling(es(i)*(Nbins-1)/e0)
              bins(idx, tidx) = bins(idx, tidx) + 1
            enddo
            tidx = tidx + 1

          enddo
 
          do i=1, head
            idx = ceiling(es(i)*(Nbins-1)/e0)
            bins(idx, Ntimes) = bins(idx, Ntimes) + 1
          enddo
        end subroutine onepart

        subroutine init_arrays(N, np, es, e0, tcs)
          use physics
  
          implicit none
          
          integer N,np
          real(REAL64) es(N), tcs(N), e0

          es = 0.0
          tcs = 0.0
          
          es(1) = e0
          tcs(1) = collision_time(e0, np)
        end subroutine init_arrays
      end module single_particle

