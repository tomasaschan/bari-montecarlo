      module mpi
        use iso_fortran_env, only : REAL64

        integer rnk, nproc, ierr, comm

      contains

        subroutine init_mpi()
          implicit none
          include 'mpif.h'

          call MPI_Init(ierr)
          comm = MPI_comm_world
          call MPI_Comm_rank(comm, rnk, ierr)
          call MPI_Comm_size(comm, nproc, ierr)

          if (rnk .eq. 0) then
            write(*,930), nproc
          end if

930   format('# Number of processes: ', I0)
        end subroutine init_mpi

        subroutine barrier()
          implicit none
          include 'mpif.h'

          call MPI_Barrier(comm, ierr)
        end subroutine barrier

        subroutine share_data(Nruns, tfin, dt, e0, eI, nri, ni, interp, interp_min, interp_max)
          implicit none
          include 'mpif.h'

          integer Nruns
          integer nri, ni
          real(REAL64) tfin, dt, e0
          real(REAL64), allocatable ::  eI(:), interp(:,:), interp_min(:), interp_max(:)

          integer mpi_struct_t, intextent
          integer offsets(2), blockcounts(2), oldtypes(2)

          ! create struct type for sending more data at once
            type struct_t
              sequence
              integer Nruns, ni
              real(REAL64) tfin, dt, e0
            end type struct_t

            type(struct_t) allvars

            call MPI_Type_extent(MPI_Integer, intextent, ierr)

            oldtypes = (/ MPI_Integer, MPI_Real8 /)
            blockcounts = (/ 2, 4 /)
            offsets = (/ 0, 2*intextent /)

            call MPI_Type_struct(2, blockcounts, offsets, oldtypes, mpi_struct_t, ierr)

            call MPI_Type_commit(mpi_struct_t, ierr)
          !

          ! master thread: assign values to struct
          if (rnk .eq. 0) then
            allvars%Nruns = Nruns
            allvars%ni = ni
            allvars%tfin = tfin
            allvars%dt = dt
            allvars%e0 = e0
          endif

          ! send struct
          call MPI_Bcast(allvars, 1, mpi_struct_t, 0, comm, ierr)

          ! non-master threads: put values just received to use
          if (rnk .ne. 0) then
            ! take values from struct in other threads
            Nruns = allvars%Nruns
            ni = allvars%ni
            tfin = allvars%tfin
            dt = allvars%dt
            e0 = allvars%e0

            ! allocate variables that depend on NDataFiles
            allocate(eI(ni))
            allocate(interp(nri,ni+1))
            allocate(interp_min(ni))
            allocate(interp_max(ni))

          end if

          ! send dynamically allocated data
          call MPI_Bcast(interp, nri*(ni+1), MPI_Real8, 0, comm, ierr)
          call MPI_Bcast(interp_min, ni, MPI_Real8, 0, comm, ierr)
          call MPI_Bcast(interp_max, ni, MPI_Real8, 0, comm, ierr)
          call MPI_Bcast(eI, ni, MPI_Real8, 0, comm, ierr)

          !print *, "# rnk/interp_min/interp_max", rnk, interp_min, interp_max

        end subroutine share_data

        subroutine reduce_bins(bins,sums,M,N)
          implicit none
          include 'mpif.h'

          integer M,N
          integer bins(M,N), sums(M,N)

          call MPI_Reduce(bins,sums,M*N,MPI_Integer,MPI_Sum,0,comm,ierr)
        end subroutine reduce_bins

        subroutine finalize_mpi()
          implicit none
          call MPI_Finalize(ierr)
        end subroutine finalize_mpi

        function wtime()
          use iso_fortran_env

          implicit none
          include 'mpif.h'

          real(REAL64) wtime

          wtime =  MPI_Wtime()
        end function wtime

      end module mpi