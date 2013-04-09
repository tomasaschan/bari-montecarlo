      subroutine read_cross_section_data(fname, Nrows, Ncols, cs)
        implicit none

        ! Input parameters
        integer Nrows, Ncols
        real*8 cs(Nrows, Ncols)
        character*30 fname

        ! Other variables
        integer f, row, col, status
        parameter(f=15)

        open(f, file=fname, status='old')

        read(f, *) ((cs(row,col),col=1,Ncols),row=1,Nrows)
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
