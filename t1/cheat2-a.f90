!P(x) = sum(i->n{fi(x-x0)(x-x0)^i/i!}
!err(x) = produtório(k=0 -> n){(x-xk)*(fn+1(erro))/(n+1)!}

program T12A
    implicit none !nenhuma variável precisará ser interpretada.

    !functions declaration
    integer ordem, counter, four

    !should be integer, but it raises errors while dividing a real number
    real fatorial

    !variables declaration
    real*8 x, x0, error, poliTaylor, currentError, modd, taylorReslt        
    real*8 p_elements(4)

    x0 = 0
    ordem = 0

    write(*,*)'digite o expoente do erro esperado (ex: -6 -> 10^-6)'
    read(*,*)error
    !input correction
    if(error  > 0) then
        error = -1*error
    end if

    error = 10**error

    write(*,*)'digite o ponto a ser aproximado para cos(x) em P(x)'
    read(*,*)x

    currentError = error
    write(*,*)'currentError, error: (',currentError,error,')'
    !aproximando P(x) a cos(x) até erro ser menor q error
    do while (currentError .ge. error)
        write(*,*)'entrou'
        
        !elements atribuition
        p_elements(1) = +cos(x0)*((x-x0)**(0+ordem))/fatorial(0+ordem)
        p_elements(2) = -sin(x0)*((x-x0)**(1+ordem))/fatorial(1+ordem)
        p_elements(3) = -cos(x0)*((x-x0)**(2+ordem))/fatorial(2+ordem)
        p_elements(4) = +sin(x0)*((x-x0)**(3+ordem))/fatorial(3+ordem)

        four = 4 

        !ordem = 1 => P(x) de ordem 3; ordem = 2 => P(x) de ordem 6
        counter = 0

        do while(counter .le. ordem)
            modd = mod(counter, four)
            counter = counter + 1
            !mod(counter,3) - resto da divisão do contador por 3
            !caso counter > 3, então ele repete os elementos
            taylorReslt = taylorReslt + p_elements(modd)  
        enddo

        currentError = taylorReslt - cos(x0)
        write(*,*)'currentError: ',currentError
        ordem = ordem + 1
    end do

    write(*,*)'grau do polimonio para o erro entrado - ordem:',ordem
    
    !modulo(funcao(x) - cos(x)) = erro

    STOP
    END

   real function fatorial(n)
        implicit none
        real n, counter0, fatorialReslt, counter1

        if (n .eq. 0) then
            fatorial = 1
        else if (n .ne. 0) then
        
            counter0 = 0
            fatorialReslt = 1

            do while(counter1 .lt. n)
                counter1 = counter1 + 1

                fatorialReslt = fatorialReslt*counter1
            enddo
            fatorial = fatorialReslt
        end if
        return
    end function