program teste
       !Taylor(x) = sum(i->n){f^i(x-x0)(x-x0)^i/i!}     
        
        !definition of variables
        integer n, res, counter

        !definition of funcions
        integer fatorial

        counter = 0

        read(*,*)n

        do while(counter < n)
            counter = counter + 1
            res = fatorial(counter)
            write(*,*)res
        end do
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