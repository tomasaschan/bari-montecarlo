      program histogram
        implicit none
        include 'mpif.h'
      
900   format(E15.2,E15.2)
910   format('# ', a, ' took ', Es10.3, ' s')
940   format('# Number of events: ', I0, ' ', I0, ' ', f5.2)

        integer ierr, rnk, nproc
      
        integer i, Nruns, Nbins
        real*8 tfin, dt, start_t, all_t, all_t_end, NRunsreal, eI, e0
        parameter(Nbins=1e3)
        integer binsize
        integer bins(Nbins), binsum(Nbins), totsum
        
        
        call MPI_Init(ierr)        
        call MPI_Barrier(MPI_Comm_world, ierr)

        all_t = MPI_Wtime()

        call MPI_Comm_rank(MPI_Comm_world, rnk, ierr)
        call MPI_Comm_size(MPI_Comm_world, nproc, ierr)

        if (rnk.EQ.0) then
          call read_program_input(Nruns, tfin, dt, e0, eI)
          write(*,930), nproc
930   format('# Number of processes: ', I0)
        endif

        call MPI_Bcast(e0, 1, MPI_Real8, 0, MPI_Comm_world, ierr)
        call MPI_Bcast(tfin, 1, MPI_Real8, 0, MPI_Comm_world, ierr)
        call MPI_Bcast(dt, 1, MPI_Real8, 0, MPI_Comm_world, ierr)
        call MPI_Bcast(Nruns, 1, MPI_Integer, 0, MPI_Comm_world, ierr)
        call MPI_Bcast(eI, 1, MPI_Real8, 0, MPI_Comm_world, ierr)

        
        call MPI_Barrier(MPI_Comm_world, ierr)

        call seed_rand()

        do i=1, Nbins
          bins(i) = 0
        enddo

        start_t = MPI_Wtime()
        do i=1, Nruns/nproc
          call onepart(e0, tfin, dt, Nbins, bins, eI)
        enddo
        
        if (rnk.EQ.0) then
          do i = 1, Nruns-nproc*Nruns/nproc
            call onepart(e0, tfin, dt, Nbins, bins, eI)
          enddo
        endif
        
        call MPI_Barrier(MPI_Comm_world, ierr)
        if (rnk.EQ.0) then
          write(*,910),'Simulation', MPI_Wtime()-start_t
        endif

        call MPI_Reduce(bins, binsum, Nbins, MPI_INTEGER, MPI_SUM,
     +                0, MPI_COMM_WORLD, ierr)
     
        if (rnk.EQ.0) then
          totsum = sum(binsum)
          do i=1, Nbins
             write(*,900)
     +         i*E0/float(Nbins), float(binsum(i))/float(totsum)*100
          enddo
        endif
        
        call MPI_Barrier(MPI_Comm_world, ierr)
        all_t_end = MPI_Wtime()
        if (rnk.EQ.0) then
          write(*,910), 'Everyting', all_t_end - all_t
        endif
        
        call MPI_Finalize(ierr)
      end 
