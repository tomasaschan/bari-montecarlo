      subroutine read_and_share(rnk, Nruns, tend, dt, e0, eI, de)
C       The master thread reads variables from stdin, and broadcasts
C       them to the rest of the MPI processes.
        
        implicit none
        include 'mpif.h'

        integer rnk, Nruns, ierr
        real*8 tend, dt, e0, eI, de, NrunsAsReal, sharereals(5)

        if (rnk.EQ.0) then
C         Nruns is read as real, to allow things like 1e6 in input  
          read *, NrunsAsReal, tend, dt, e0, eI, de
          Nruns = int(NrunsAsReal)
          sharereals = (/ tend, dt, e0, eI, de /)
        endif

C       Broadcast the input to all the threads
        call MPI_Bcast(Nruns,1,MPI_Integer,0,MPI_Comm_world,ierr)
        call MPI_Bcast(sharereals,5,MPI_Real8,0,MPI_Comm_world,ierr)

C       Put the broadcasted values in their variables for return
        tend = sharereals(1)
        dt = sharereals(2)
        e0 = sharereals(3)
        eI = sharereals(4)
        de = sharereals(5)
      end

      subroutine write_ints_from_master(rnk, N, ints)
C       Helper function to write N ints from the master process to stdout,
C       using the given format string.

        implicit none

        integer rnk, N, ints(N)

        if (rnk.EQ.0) then
          write(*,*) ints
        endif
      end

      subroutine write_reals_from_master(rnk, N, reals)
C       Helper function to write N ints from the master process to stdout,
C       using the given format string.

        implicit none

        integer rnk, N
        real*8 reals(N)

        if (rnk.EQ.0) then
          write(*,*) reals
        endif
      end
      