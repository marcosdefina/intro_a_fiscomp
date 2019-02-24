       program fatorial
       implicit none !nenhuma variável precisará ser interpretada.
       integer*16 n, reslt, counter

       counter = 0
       reslt = 1

       write(*,*)'digite um numero' !primeiro * é input de terminar, segundo é output.
       read(*,*)n
       
       do while(counter .lt. n)
              counter = counter + 1

              reslt = reslt*counter
       enddo

       write(*,*)n,'! = ',reslt
       STOP
       END
