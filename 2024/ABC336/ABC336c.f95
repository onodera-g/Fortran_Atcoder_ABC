program abc335c
    !N：取り出したい良い整数の番号
    !Ans：N番目の良い整数
    implicit none
    integer(8) N, Ans(100)
    integer i, j, tmp

    !読み込み
    read (*, *) N
    N = N - 1; Ans = 0

    !良い整数の算定
    if (N == -1) then !N=1の場合
        write (*, '(i0)', advance='no') 0
        stop
    else !N=1より大きい
        i = 1; tmp = 5
        do
            Ans(i) = mod(n, tmp)
            N = N/5
            if (N == 0) exit
            i = i + 1
        end do
    end if

    !結果の出力
    do j = i, 1, -1
        write (*, '(i0)', advance='no') Ans(j)*2
    end do
end program abc335c
