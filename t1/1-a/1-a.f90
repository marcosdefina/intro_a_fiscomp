program fatorial
    implicit none !nenhuma variável precisará ser interpretada.
    integer*8 n, reslt, counter0, counter1

    counter0 = 0
    counter1 = 0
    reslt = 1
    
    open(2, file = '1-a.dat', status = 'new')

    !program will fail if 1-a.dat already exists
    !TODO: prevent this error.
    
    do while(counter0 .lt. 20)
           counter0 = counter0 + 1
           do while(counter1 .lt. counter0)
                  counter1 = counter1 + 1

                  reslt = reslt*counter1
           enddo
           write(2,*)counter0,'! = ',reslt
    enddo

    STOP
    END
