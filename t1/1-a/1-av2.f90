program teste
       !Taylor(x) = sum(i->n){f^i(x-x0)(x-x0)^i/i!}     
        
        !definition of variables
        integer n, res

        !definition of funcions
        integer fatorial

        read(*,*)n

        res = fatorial(n)

        write(*,*)res
    STOP
    END

    integer function fatorial(n)
        integer n, counter0, reslt
        
        counter0 = 0
        reslt = 1
    
        do while(counter1 .lt. n)
            counter1 = counter1 + 1
    
            reslt = reslt*counter1
        enddo
        fatorial = reslt
        return
    end function