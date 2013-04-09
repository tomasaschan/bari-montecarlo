      program tester
        implicit none

        integer :: Nrows, Ncols, row, col, lines_in_file
        parameter(Ncols=2)
        real*8, allocatable :: cs(:,:)

        integer :: Nrows_i, Ncols_interp
        parameter(Nrows_i=1e5, Ncols_interp=Ncols)
        real*8 :: interp(Nrows_i, Ncols_interp)
        real*8 x0, x1
        character*30 fname

        read *, fname, x0, x1

        Nrows = lines_in_file(fname)

        allocate(cs(Nrows,Ncols))

        print *, "# Reading from file ", fname

        call read_cross_section_data(fname, Nrows, Ncols, cs)

        call interpolate_2d_data(Nrows,cs,Nrows_i,interp,x0,x1)

        do row = 1, Nrows_i-1
          print *, row, (interp(row,col),col=1,Ncols)
        enddo

      end