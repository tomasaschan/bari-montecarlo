      subroutine read_program_input(Nruns, tfin, dt, e0, eI,
     +      fname, x0, x1, rnk)
        implicit none

        integer Nruns, rnk
        real*8 tfin, dt, e0, eI, NRunsreal, x0, x1
        character*30 fname

        if (rnk .eq. 0) then
          read *, NRunsreal, tfin, dt, e0, fname, x0, x1, eI
          Nruns = int(NRunsreal)

          write(*,920), float(Nruns), tfin
          write(*,960), e0, eI

        endif

920   format('# Runs: ', Es8.1E2, ', t_end: ', Es8.1)
960   format('# e0: ', F8.1, ', eI: ', F8.1)

        call share_indata(Nruns, tfin, dt, e0, eI, rnk)

      end

      subroutine get_interpolation(fname, nri, interp, x0, x1, rnk)
        implicit none
        include 'mpif.h'
930   format("# Reading cross-section data from ", A)

        ! row counts and pointers for both series
        integer nrd, nri
        ! matrix holding x (first column) and y (second column)
        real*8 :: interp(nri,2)
        ! boundaries for the desired interval and interpolation step size
        real*8 x0, x1
        
        ! Other variables
        integer f, row, col, ierr, rnk
        parameter(f=15)

        integer :: lines_in_file
        real*8, allocatable :: cs(:,:)
        character*30 fname
        

        if (rnk.eq.0) then
          write(*,930) fname

          nrd = lines_in_file(fname)
          allocate(cs(nrd,2))

          open(f, file=fname, status='old')
          read(f, *)  ((cs(row,col),col=1,2),row=1,nrd)

          call interpolate_2d_data(nrd, cs, nri, interp, x0, x1)

          deallocate(cs)

        endif

        call MPI_Bcast(interp,2*nri,MPI_Real8,0,MPI_Comm_world,ierr)

      end

      function lines_in_file(fname)
        implicit none

        integer lines_in_file, stat
        character*30 fname
        character*80 line

        open(20, file=fname)

        lines_in_file = 0
        read (20,*,iostat=stat) line

        do while(stat.eq.0)
          lines_in_file = lines_in_file+1
          read (20,*,iostat=stat) line
        enddo

        close(20)
      end

