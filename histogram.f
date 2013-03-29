      program histogram
        implicit none
        include 'mpif.h'
        
        integer sz, rnk, ierr
        
        integer Nruns, onepart, npart, maxpart
        parameter(Nruns = 100)
        character*20 fname
        
        real*8 E0, tfin, dt
        parameter(E0=300, tfin=2e-9, dt=1e-11)
        
        call MPI_Init(ierr)
        
        call MPI_Comm_size(MPI_Comm_world, sz, ierr)
        call MPI_Comm_rank(MPI_Comm_world, rnk, ierr)
      
      
        write(fname,'(A10,I0,A4)') 'histogram.', rnk, '.out'
        open(unit=7+rnk, file=fname)
        
        npart = onepart(E0, tfin, dt, 7+rnk)
        
        call MPI_Reduce(npart, maxpart, 1, MPI_Integer, 
     +                 MPI_MAX, 0, MPI_Comm_world)
        
        print *, 'Thread', rnk, ' got ', npart, ' particles.'
        
        if (rnk.EQ.0) then
          print*, 'The maximum number of particles was ', maxpart
        endif 
        call MPI_Finalize(ierr)
      
      end 
