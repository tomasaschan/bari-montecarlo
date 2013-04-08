      program tester
        implicit none

        integer :: N, i, lines_in_file
        real*8, allocatable :: cs(:,:)
        character*30 fname

        read *, fname

        N = lines_in_file(fname)

        allocate(cs(N,2))

        call read_cross_section_data(fname, N, cs)

        do i=1,N
          print *, cs(i,1), cs(i,2)
        enddo

      end