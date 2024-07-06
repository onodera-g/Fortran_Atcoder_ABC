program abc336a
    !N：整数
    implicit none
    integer N, i

    !入力
    read (*, *) N

    !結果の出力
    write (*, '(a1)', advance='no') "L"
    do i = 1, N
        write (*, '(a1)', advance='no') "o"
    end do
    write (*, '(a2)') "ng"
end program abc336a
