program fatorial
    implicit none !nenhuma variável precisará ser interpretada.
    
    !defining variables
    integer useless
    real*8 S, pi,counter0, log_n_fat, error

    !setting initials values to variables
    counter0 = 1

    !search for pi truncate place at real*8    
    pi = 3.141592653589793238462643383279d0

    !opening 1-b file to compare result
    open(1, file = '../1-b/1-b.dat', status = 'old')
    open(2, file = '1-c.dat', status = 'new')
    
    !enlace to create the list of S's
    do while(counter0 .lt. 30)
        counter0 = counter0 + 1
        !calculating S
        S = counter0 * log(counter0) - counter0 + 1/2*log(2*pi*counter0)
        
        !reading 1-b.dat, first value are useless
        read(1,*)useless, log_n_fat

        !calculating error (ln(n!)-S)/ln(n!)
        error = (log_n_fat - S)/log_n_fat

        write(2,*)counter0, log_n_fat , S, error

    enddo

    close(1)
    close(2)

    STOP
    END
