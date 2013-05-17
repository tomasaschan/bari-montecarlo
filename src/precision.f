      module precision
        use, intrinsic :: iso_fortran_env, only : REAL64, INT16, INT32, INT64
        use, intrinsic :: iso_c_binding, only : C_INT128_T
        
        private
        include 'mpif.h'

        ! float kind: 64-bit floats (double precision)
        integer, parameter, public :: rkind = REAL64

        ! integer kinds for 16-bit (short), 32-bit (int) and 64-bit (long) integers
        integer, parameter, public :: skind = INT16
        integer, parameter, public :: ikind = INT32
        integer, parameter, public :: lkind = INT64

        ! MPI kinds for int, long and double
        integer, parameter, public :: mpi_rkind = MPI_Real8
        integer, parameter, public :: mpi_ikind = MPI_Integer
        integer, parameter, public :: mpi_lkind = MPI_Long

      end module precision