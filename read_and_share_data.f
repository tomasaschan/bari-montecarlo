      subroutine read_and_share(rnk, Nruns, tend, dt, e0, eI)
        implicit none
        include 'mpif.h'

        integer rnk, Nruns, ierr
        real*8 tend, dt, e0, eI, NrunsAsReal, sharereals(4)

        if (rnk.EQ.0) then
          read *, NrunsAsReal, tend, dt, e0, eI
          Nruns = int(NrunsAsReal)
          sharereals = (/ tend, dt, e0, eI /)
        endif

        call MPI_Bcast(Nruns,1,MPI_Integer,0,MPI_Comm_world,ierr)
        call MPI_Bcast(sharereals,4,MPI_Real8,0,MPI_Comm_world,ierr)

      end