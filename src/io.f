      module io
        use, intrinsic :: iso_fortran_env, only : REAL64, INT16, INT32, INT64, stdout=>output_unit, stdin=>input_unit


        integer NDataFiles
        
        character(len=25), allocatable :: fnames(:)
        integer, allocatable :: nrd(:)
        real(REAL64), allocatable :: raw(:,:,:)
        real(REAL64), allocatable :: e0raw(:), e1raw(:), Araw(:), Qraw(:)
        integer, allocatable :: productsraw(:)

        character(len=*), parameter, public :: paramformat = '((A),(Es14.3),(A8))'

        integer, parameter :: in_f = 11, cs_f = 13, eedf_f = 17, rate_f = 19, pops_f = 23

      contains

        subroutine read_program_input(Nruns, tfin, dt, e0, p, Npops, neexpr)

          implicit none

          integer row, col, i
          integer(INT64) Nruns
          real(REAL64) tfin, dt, e0, p, NRunsreal
          integer Npops
          character(len=*) neexpr

          read(stdin,*) NRunsreal, tfin, dt, e0, p

          read(stdin,*) neexpr

          read(stdin,*) NDataFiles, Npops
          
          allocate(fnames(NDataFiles))
          allocate(nrd(NDataFiles))
          allocate(e0raw(NDataFiles))
          allocate(e1raw(NDataFiles))
          allocate(productsraw(NDataFiles))

          allocate(Araw(Npops), Qraw(Npops))

          read(stdin,*) Araw, Qraw

          read(stdin,*) fnames


          Nruns = int(NRunsreal)

          if (NRunsreal .gt. huge(NRunsreal)) then
            write(stdout,*) "# WARNING: overflow in input N - lower the number of simulated particles!"
          end if

          ! first pass over data files: count line numbers
          do i=1, NDataFiles
            nrd(i) = lines_in_file(trim(fnames(i)))-1
          end do
          ! allocate space for raw data
          allocate(raw(maxval(nrd),2,NDataFiles))
          ! second pass over data files: read data into memory
          do i=1, NDataFiles
            ! open data file
            open(cs_f, file=trim(fnames(i)), status='old')
            ! read metadata
            read(unit=cs_f,fmt=*) e0raw(i), e1raw(i), productsraw(i)
            ! read cross-sections
            read(unit=cs_f,fmt=*) ((raw(row,col,i),col=1,2),row=1,nrd(i))
            ! Close the file so the handle can be reused
            close(cs_f)
          end do

          ! Rescale the data to SI units - expecting cross-sections in cm^2
          raw(:,2,:) = raw(:,2,:)*1.0e-4 
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

        function padr(string, length) result(r)
          character(len=*) :: string
          integer          :: length
          character(len=length) :: r

          r = adjustl(string)
        end function padr

      end module io
