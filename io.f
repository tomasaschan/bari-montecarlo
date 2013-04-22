      module io
        use precision

        integer nrd, NDataFiles
        
        character(len=30), allocatable :: fnames(:)
        real(rkind), allocatable :: raw(:,:)
        real(rkind), allocatable :: e0raw(:), e1raw(:)

      contains

      subroutine read_program_input(Nruns, tfin, dt, e0, p, eI)
        use mpi

        implicit none

        integer(lkind) Nruns
        real(rkind) tfin, dt, e0, p, NRunsreal
        real(rkind), allocatable :: eI(:)

        read *, NRunsreal, tfin, dt, e0, p, NDataFiles
        
        allocate(fnames(NDataFiles))
        allocate(e0raw(NDataFiles))
        allocate(e1raw(NDataFiles))
        allocate(eI(NDataFiles))

        read *, fnames
        read *, e0raw, e1raw, eI
        Nruns = int(NRunsreal)

        print *, "# ", NRunsreal, Nruns, huge(lkind), log10(real(huge(lkind),rkind))

        write(*,920), float(Nruns), tfin, dt
        write(*,960), e0, eI          

920   format('# Runs: ', Es8.1E2, ', tfin: ', Es8.1, ', dt: ', Es8.1)
960   format('# e0: ', F8.1, ', eI: ', 3(F8.4))
      end subroutine read_program_input

      subroutine read_interpolation_data(fname)
        implicit none

        integer f, row, col
        parameter(f=15)
        character*(*) fname

        ! Print diagnostic message about file name
        write(*,930) fname

        ! Count lines in file and allocate space for raw data
        nrd = lines_in_file(trim(fname))
        allocate(raw(nrd,2))

        ! Read the data into variable
        open(f, file=trim(fname), status='old')
        read(f,*) ((raw(row,col),col=1,2),row=1,nrd)

        ! Rescale the data to SI units - expecting cross-sections in cm^2
        raw(:,2) = raw(:,2)*1.0e-4 

        ! Close the file so the handle can be reused
        close(f)

930   format("# Reading cross-section data from ", A)
      end subroutine read_interpolation_data

      subroutine clean_up_io()
        implicit none

        deallocate(e0raw)
        deallocate(e1raw)
        deallocate(fnames)

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

      subroutine here()
        use mpi

        implicit none

        print *, "#", rnk, "here"
      end subroutine here

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
