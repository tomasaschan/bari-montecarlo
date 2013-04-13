      subroutine init_mpi(rnk, nproc)
        implicit none
        include 'mpif.h'

        integer rnk, nproc, ierr

        call MPI_Init(ierr)
        call MPI_Comm_rank(MPI_comm_world, rnk, ierr)
        call MPI_Comm_size(MPI_comm_world, nproc, ierr)

        if (rnk .eq. 0) then
          write(*,930), nproc
930   format('# Number of processes: ', I0)
        endif

      end

      subroutine share_indata(Nruns, tfin, dt, e0, eI, rnk)
        implicit none
        include 'mpif.h'

        integer Nruns
        real*8 tfin, dt, e0, eI
        integer rnk

        integer mpi_struct_t, intextent, ierr
        integer offsets(2), blockcounts(2), oldtypes(2)

        type struct_t
          sequence
          integer Nruns
          real*8 tfin, dt, e0, eI
        end type struct_t

        type(struct_t) allvars


        call MPI_Type_extent(MPI_Integer, intextent, ierr)

        oldtypes = (/ MPI_Integer, MPI_Real8 /)
        blockcounts = (/ 1, 5 /)
        offsets = (/ 0, intextent /)

        call MPI_Type_struct(2, blockcounts, offsets, 
     1                         oldtypes, mpi_struct_t, ierr)


        call MPI_Type_commit(mpi_struct_t, ierr)

        if (rnk .eq. 0) then
          allvars%Nruns = Nruns
          allvars%tfin = tfin
          allvars%dt = dt
          allvars%e0 = e0
          allvars%eI = eI
        endif

        call MPI_Bcast(allvars, 1, mpi_struct_t,
     +      0, MPI_comm_world, ierr)

        Nruns = allvars%Nruns
        tfin = allvars%tfin
        dt = allvars%dt
        e0 = allvars%e0
        eI = allvars%eI
      end

      subroutine finalize_mpi()
        implicit none

        integer ierr

        call MPI_Finalize(ierr)
      end