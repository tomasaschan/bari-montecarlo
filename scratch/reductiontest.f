      program reductiontest
        implicit none
          include 'mpif.h'
        
          integer ierr, rnk, sz
          integer bins(3), binsum(3), i
          
          call MPI_Init(ierr)
          
          call MPI_Comm_size(MPI_Comm_world, sz, ierr)
          call MPI_Comm_rank(MPI_Comm_world, rnk, ierr)
          
          do i=1, 3
              bins(i) = 2*rnk+i
          enddo
          
          call MPI_Reduce(bins, binsum, 3, MPI_Integer, MPI_Sum, 0, 
     +                MPI_Comm_world, ierr)
          
          if (rnk.EQ.0) then
              do i=1, 3
                  print *, binsum(i)
              enddo
              
          endif
          
          call MPI_Finalize(ierr)
      end
