      program histogram
        implicit none
        include 'mpif.h'
      
900   format(I0,E15.2)
910   format('# ', a, ' took ', Es10.3, ' s')
920   format('# Runs: ', eS9.1, ', t_end: ', Es8.1, ' E0: ', Es8.1)
930   format('# Number of processes: ', I0)
940   format('# Number of events: ', I0, ' ', I0, ' ', f5.2)
960   format('# Ionization energy: ', F8.1)

        integer ierr, rnk, nproc
      
        integer i, Nruns, Nevents, Totevents
        real*8 E0, tfin, dt, start_t, all_t, eI
        integer Nbins

        integer allocatable bins(Nbins), binsum(Nbins), totsum
        
        
        call MPI_Init(ierr)        

        all_t = MPI_Wtime()

        call MPI_Comm_rank(MPI_Comm_world, rnk, ierr)
        call MPI_Comm_size(MPI_Comm_world, nproc, ierr)


        call read_and_share(rnk, Nruns, tfin, dt, E0, eI)

        if (rnk.EQ.0) then
          write(*,920), real(Nruns), tfin, e0
          write(*,930), nproc
          write(*,960), eI
        endif

        call MPI_Barrier(MPI_Comm_world, ierr)

        call seed_rand()

        do i=1, Nbins
          bins(i) = 0
        enddo
        Nevents = 0

        start_t = MPI_Wtime(ierr)
        do i=1, Nruns/nproc
          call onepart(E0, tfin, dt, bins, Nevents, eI)
        enddo
        
        if (rnk.EQ.0) then
          do i = 1, Nruns-nproc*Nruns/nproc
            call onepart(E0, tfin, dt, bins, Nbins, eI)
          enddo
        endif
        
        call MPI_Barrier(MPI_Comm_world, ierr)
        if (rnk.EQ.0) then
          write(*,910),'Simulation', MPI_Wtime()-start_t
        endif

        call MPI_Reduce(bins, binsum, Nbins, MPI_INTEGER, MPI_SUM,
     +                0, MPI_COMM_WORLD, ierr)
        call MPI_Reduce(Nevents, Totevents, 1, MPI_INTEGER, MPI_SUM,
     +                0, MPI_COMM_WORLD, ierr)
     
        if (rnk.EQ.0) then
          totsum = sum(binsum)
          do i=1, Nbins
             write(*,900) i, float(binsum(i))/float(totsum) * 100
          enddo
        endif
        
        call MPI_Barrier(MPI_Comm_world, ierr)
        if (rnk.EQ.0) then
          all_t = MPI_Wtime() - all_t
          write(*,910), 'Everyting', all_t
          write(*,940) Totevents, int(Nruns*E0/13.4), 
     +         Totevents/(Nruns*E0/13.4)
        endif
        
        call MPI_Finalize(ierr)
      end 
