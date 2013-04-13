      program commons
        implicit none

        real*8 a, b
        integer i

        common /tst/ a,b,i

        a = 3.9
        b = 2.7

        i = 15

        call testing()

      end


      subroutine testing()
        implicit none

        real*8 a, b
        integer i

        common /tst/ a,b,i

        print *, a, b, i

      end