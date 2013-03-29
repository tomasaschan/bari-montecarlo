      program histogram
        implicit none
        include 'mpif.h'
      
        integer ierr, rnk, nproc
      
        integer i, Nruns
        real*8 E0, tfin, dt, start_t
        parameter(E0=300.0, dt=1e-11)
        integer bins(300), binsum(300), partsum
        
        
        call MPI_Init(ierr)
        call MPI_Comm_rank(MPI_COMM_WORLD, rnk, ierr)
        call MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)

        if (rnk.EQ.0) then
          read *, tfin, Nruns
        endif

        call MPI_Bcast(tfin, 1, MPI_Real8, 0, MPI_Comm_world, ierr)
        call MPI_Bcast(Nruns, 1, MPI_Integer, 0, MPI_Comm_world, ierr)
        
        call seed_rand()

        do i=1, 300
          bins(i) = 0
        enddo

        start_t = MPI_Wtime(ierr)
        do i=1, Nruns/nproc
          call onepart(E0, tfin, dt, bins)
        enddo
        
        if (rnk.EQ.0) then
          do i = 1, Nruns-nproc*Nruns/nproc
            call onepart(E0, tfin, dt, bins)
          enddo
        endif
        
        call MPI_Barrier(MPI_Comm_world, ierr)
        if (rnk.EQ.0) then
          write(*,910),'Simulation', MPI_Wtime()-start_t
        endif
        call MPI_Barrier(MPI_Comm_world, ierr)
        
        start_t = MPI_Wtime()
        call MPI_Reduce(bins, binsum, 300, MPI_INTEGER, MPI_SUM,
     +                0, MPI_COMM_WORLD, ierr)
        
        call MPI_Barrier(MPI_Comm_world, ierr)
        if (rnk.EQ.0) then
          write(*,910),'Reduction', MPI_Wtime()-start_t
        endif
        call MPI_Barrier(MPI_Comm_world, ierr)
        
        
        if (rnk.EQ.0) then
          start_t = MPI_Wtime()
          partsum = sum(binsum)
          write(*,910),'Summation',MPI_Wtime()-start_t
          
          start_t = MPI_Wtime()
          do i=1, 300
             print *, i, binsum(i)/(partsum*1.0)
          enddo
          write(*,910),'Output',MPI_Wtime()-start_t

        endif
        
        call MPI_Finalize(ierr)
910   format('# ', a, ' took ', E8.2, ' s')
      end 
