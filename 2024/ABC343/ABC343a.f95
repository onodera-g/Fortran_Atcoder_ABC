program abc343a
    !A：整数A
    !B：整数B
    implicit none
    integer A, B

    !入力
    read (*, *) A, B

    !結果の出力
    if (A + B == 0) then
        write (*, *) 1
    else
        write (*, *) 0
    end if
end program abc343a
