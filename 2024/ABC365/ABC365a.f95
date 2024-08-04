program abc365a
    !Y：西暦
    implicit none
    integer Y

    !入力
    read (*, *) Y

    !結果の出力
    if (mod(Y, 4) /= 0) then !Y が 4 の倍数でない年は 365 日
        write (*, *) 365
    elseif (mod(Y, 4) == 0 .and. mod(Y, 100) /= 0) then !Y が 4 の倍数で、かつ100 の倍数でない年は 366 日
        write (*, *) 366
    elseif (mod(Y, 100) == 0 .and. mod(Y, 400) /= 0) then !Y が 100 の倍数で、かつ 400 の倍数でない年は 365 日
        write (*, *) 365
    elseif (mod(Y, 400) == 0) then !Y が 400 の倍数である年は 366 日
        write (*, *) 366
    end if
end program abc365a
