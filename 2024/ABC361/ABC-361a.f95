program abc361a
    !N：整数列の長さ
    !A：長さNの整数列
    !K：整数を割り込ませたい位置
    !X：割り込ませたい整数
    implicit none
    integer N, K, X, i, cnt
    integer, allocatable::A(:)

    !入力
    read (*, *) N, K, X
    allocate (A(N))
    read (*, *) A(:)
    cnt = 0

    !結果の出力
    do i = 1, N
        write (*, '(i0,1x)', advance='no') A(i)
        if (i == K) then !間にXを割り込ませる
            write (*, '(i0,1x)', advance='no') X
        end if
    end do
end program abc361a
