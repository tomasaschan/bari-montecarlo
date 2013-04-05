        parameter(nen=30)
        real en0(1000000)
        real eedf(nen)

        emin=15
        emax=300
        ne=10000
        de=emax/float(nen)

        do ien=1,nen
         eedf(ien)=0
        end do

        do ie=1,ne
         en0(ie)=emax
        end do

        ie=1
1       continue

        en = en0(ie)    !inizio traiettoria

2       if(en.gt.emin)then

         print *,'evento',ie,en
         ien=int(en/de)
         eedf(ien)=eedf(ien)+1
         enold = en
         en = en * rand()
         ne = ne+1
         en0(ne)=enold-en-emin

        end if

        if(en.gt.emin)goto 2

        ie = ie + 1

        print *,ie,ne

        if(ie.lt.ne)goto 1

        do ien=1,nen
         print *,float(ien)*de-de/2,eedf(ien)
        end do

        end