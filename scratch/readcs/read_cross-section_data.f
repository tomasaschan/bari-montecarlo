      subroutine read_cross_section_data(fname, N, cs)
        implicit none

        ! Input parameters
        integer N
        real*8 cs(N,2)
        character*30 fname

        ! Other variables
        integer f, i, status
        parameter(f=15)

        open(f, file=fname, status='old')

        read(f, *) cs
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
