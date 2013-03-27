      program runner
        implicit none
        include 'mpif.h'

        integer ierr       
        
        call MPI_Init(ierr)

        
       
        call MPI_Finalize(ierr)
        
      end

      
