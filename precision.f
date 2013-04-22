      module precision
        use, intrinsic :: iso_fortran_env, only : REAL64, REAL128
        use, intrinsic :: iso_c_binding, only : C_INT128_T

        ! Choose 8/16-byte reals
        integer, parameter :: rkind = REAL64

        ! integer kinds for 16- and 32-byte integers
        integer, parameter :: ikind = selected_int_kind(16)
        integer, parameter :: lkind = selected_int_kind(32)

        ! MPI_Real8 = 15, MPI_Real16 = 16
        integer, parameter :: mpi_rkind = 15
        ! MPI_Integer = 7, MPI_Long = 41
        integer, parameter :: mpi_ikind = 7 
        integer, parameter :: mpi_lkind = 41
        
      end module precision