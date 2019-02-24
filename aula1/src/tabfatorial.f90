       program fatorial
       implicit none !nenhuma variável precisará ser interpretada.
       real*16 n, reslt, counter0, counter1

       counter0 = 0
       counter1 = 0
       reslt = 1

       write(*,*)'digite um numero' !primeiro * é input de terminar, segundo é output.
       read(*,*)n
       
       do while(counter0 .lt. n)
              counter0 = counter0 + 1
              do while(counter1 .lt. counter0)
                     counter1 = counter1 + 1

                     reslt = reslt*counter1
              enddo
              write(*,*)counter0,'! = ',reslt
       enddo


       STOP
       END
