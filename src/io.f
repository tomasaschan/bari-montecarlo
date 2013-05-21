      module io
        use, intrinsic :: iso_fortran_env, only : REAL64, INT16, INT32, INT64

        integer NDataFiles
        
        character(len=25), allocatable :: fnames(:)
        integer, allocatable :: nrd(:)
        real(REAL64), allocatable :: raw(:,:,:)
        real(REAL64), allocatable :: e0raw(:), e1raw(:)
        integer, allocatable :: productsraw(:)

        character(len=*), parameter, public :: paramformat = '((A30),(Es14.3),(A8))'

      contains

      subroutine read_program_input(Nruns, tfin, dt, e0, p)

        implicit none

        integer f, row, col, i
        parameter(f=15)
        integer(INT64) Nruns
        real(REAL64) tfin, dt, e0, p, NRunsreal

        read *, NRunsreal, tfin, dt, e0, p, NDataFiles
        
        allocate(fnames(NDataFiles))
        allocate(nrd(NDataFiles))
        allocate(e0raw(NDataFiles))
        allocate(e1raw(NDataFiles))
        allocate(productsraw(NDataFiles))

        read *, fnames
        Nruns = int(NRunsreal)

        if (NRunsreal .gt. huge(NRunsreal)) then
          print *, "# WARNING: overflow in input N - lower the number of simulated particles!"
        end if

        ! first pass over data files: count line numbers
        do i=1, NDataFiles
          nrd(i) = lines_in_file(trim(fnames(i)))-1
        end do
        ! allocate space for raw data
        allocate(raw(maxval(nrd),2,NDataFiles))
        ! second pass over data files: read data into memory
        do i=1, NDataFiles
          ! print diagnostic message about file name
          write(*,930) fnames(i)
          ! open data file
          open(f, file=trim(fnames(i)), status='old')
          ! read metadata
          read(unit=f,fmt=*) e0raw(i), e1raw(i), productsraw(i)
          ! read cross-sections
          read(unit=f,fmt=*) ((raw(row,col,i),col=1,2),row=1,nrd(i))
          ! Close the file so the handle can be reused
          close(f)
        end do

        ! Rescale the data to SI units - expecting cross-sections in cm^2
        raw(:,2,:) = raw(:,2,:)*1.0e-4 



930   format("# Reading cross-section data from ", A)
      end subroutine read_program_input

      subroutine clean_up_io()
        implicit none

        deallocate(e0raw)
        deallocate(e1raw)
        deallocate(fnames)
        deallocate(nrd)
        deallocate(productsraw)
        deallocate(raw)

      end subroutine clean_up_io

      function lines_in_file(fname)
        implicit none

        integer lines_in_file, stat
        character*(*) fname
        character*120 line

        open(20, file=fname)

        lines_in_file = 0
        read (20,*,iostat=stat) line

        do while(stat.eq.0)
          lines_in_file = lines_in_file+1
          read (20,*,iostat=stat) line
        enddo

        close(20)
      end function lines_in_file

      subroutine check_fnames()
        implicit none

        integer i, ci
        character(len=30) fname
        character c


        do i=3,NDataFiles
            fname = fnames(i)
            print *, len(fname)
          do ci=1,len(fname)
            c = fname(ci:ci)
            write(*,940) c, iachar(c)
          end do

940   format("'", A, "'  ", I0)

          !call read_interpolation_data(trim(fnames(i)), raw, nrd)
          !call interpolate(nrd, raw, nri, i, interp, e0raw(i), e1raw(i))
          !deallocate(raw)
        end do


      end subroutine check_fnames

      end module io
