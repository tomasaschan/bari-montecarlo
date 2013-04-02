      program histogram
        implicit none
        include 'mpif.h'
      
        integer ierr, rnk, nproc
      
        integer i, Nruns
        real*8 E0, tfin, dt, start_t, all_t, NRunsreal
        parameter(E0=300.0, dt=1e-11)
        integer bins(300), binsum(300), partsum
        
        
        call MPI_Init(ierr)        


        if (rnk.EQ.0) then
          all_t = MPI_Wtime() - all_t
        endif

        call MPI_Comm_rank(MPI_Comm_world, rnk, ierr)
        call MPI_Comm_size(MPI_Comm_world, nproc, ierr)

        if (rnk.EQ.0) then
          read *, tfin, NRunsreal
          Nruns = int(NRunsreal)
          write(*,920), real(Nruns), tfin
          write(*,930), nproc
920       format('# Runs: ', eS9.1, ', t_end: ', Es8.1)
930       format('# Number of processes: ', I0)
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

        call MPI_Reduce(bins, binsum, 300, MPI_INTEGER, MPI_SUM,
     +                0, MPI_COMM_WORLD, ierr)
        

        if (rnk.EQ.0) then
          partsum = sum(binsum)
          do i=1, 300
             write(*,900) i, binsum(i)/(partsum*1.0) * 100
          enddo
900   format(I3,E15.2)
        endif
        
        call MPI_Barrier(MPI_Comm_world, ierr)
        if (rnk.EQ.0) then
          all_t = MPI_Wtime() - all_t
          write(*,910), 'Everyting', all_t
        endif
        
        call MPI_Finalize(ierr)
910   format('# ', a, ' took ', Es10.3, ' s')
      end 
