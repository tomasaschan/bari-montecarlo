      program iotest
        implicit none
        include 'mpif.h'
        
        integer ierr, rank, nproc

        integer bufsize, thefile, i
        parameter(bufsize=100)
        integer buf(bufsize)
        integer(kind=MPI_Offset_kind) disp
        
        call MPI_Init(ierr)
        
        call MPI_Comm_rank(MPI_Comm_world, rank, ierr)
        call MPI_Comm_size(MPI_Comm_world, nproc, ierr)
        
        do i=0, bufsize
          buf(i) = rank * bufsize + i
        enddo
        
        call MPI_File_open(MPI_Comm_world, 'iotestfile.out', 
     +                     MPI_Mode_wronly + MPI_Mode_create, 
     +                     MPI_Info_null, thefile, ierr)
        
        
        ! assume 4-bite integers
        disp = rank * bufsize * 4
        
c~         call MPI_File_set_view(thefile, disp, MPI_Integer, MPI_Integer, 
c~      +                         'native', MPI_Info_Null, ierr)

        call MPI_File_Write(thefile, buf, bufsize, MPI_Character, 
     +                       MPI_Status_ignore, ierr)
        
        call MPI_File_close(thefile, ierr)
        
        call MPI_Finalize(ierr)
        
      end
