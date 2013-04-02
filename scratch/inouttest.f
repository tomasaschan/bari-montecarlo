      program inouttest
        implicit none

        integer N, i 

        N = 0

        do i = 1, 100
          call addone(N)
        enddo

        print *, N

      end

      subroutine addone(N)
        implicit none

        integer N

        N = N+1

        return
      end