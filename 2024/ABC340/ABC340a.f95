program abc340a
    !A：初項
    !B：末項
    !D：公差
    implicit none
    integer A, B, D, i

    !入力
    read (*, *) A, B, D

    !等差数列の出力
    i = 0
    do
        write (*, '(*(i0,1x))', advance='no') A + D*i
        i = i + 1
        if (B < A + D*i) stop
    end do

end program abc340a
