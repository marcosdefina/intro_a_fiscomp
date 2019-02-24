program fatorial
    implicit none !nenhuma variável precisará ser interpretada.
    
    !defining variables
    integer*8 n, counter0, counter1
    real*8 logRes, reslt

    !setting initials values to variables
    counter0 = 1
    counter1 = 0
    reslt = 1
    
    !opening new file to print result
    open(2, file = '1-b.dat', status = 'new')

    !program will fail if 1-a.dat already exists
    !TODO: prevent this error.
    
    !enlace to create the list of fatorials
    do while(counter0 .lt. 30)
           counter0 = counter0 + 1
           !calculating fatorial
           do while(counter1 .lt. counter0)
                  counter1 = counter1 + 1

                  reslt = reslt*counter1
                  logRes = log(reslt)
           enddo
           write(2,*)counter0,logRes
    enddo

    STOP
    END
