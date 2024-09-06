program abc369c
    !N       ：正整数列Aの長さ
    !A       ：正整数列A
    !A_diff  ：正整数列Aにおける隣り合う値の差
    !sum_cnt ：A_diffにおいて差がA_diff(1)と同じ個数
    !zero_cnt：A_diffにおいて差が0の個数
    !cnt     ：成立する等差数列のパターン数
    implicit none
    integer(16) i, j
    integer(16) N, cnt, sum_cnt, zero_cnt
    integer(16), allocatable::A(:), A_diff(:)

    !入力
    read (*, *) N
    allocate (A(N), A_diff(N - 1))
    read (*, *) A

    !隣り合う差をあらかじめ計算
    sum_cnt = 1; zero_cnt = 0
    A_diff(1) = A(2) - A(1)
    if (A_diff(i) == 0) zero_cnt = zero_cnt + 1
    do i = 2, N - 1
        A_diff(i) = A(i + 1) - A(i)
        if (A_diff(1) == A_diff(i)) sum_cnt = sum_cnt + 1
        if (A_diff(i) == 0) zero_cnt = zero_cnt + 1
    end do

    !成立する等差数列のパターン数を計算
    cnt = N + (N - 1) !長さ１は常に等差数列、長さ２も等差数列
    !全部差が0もしくは全部差が同じ場合はdo loop不要
    if (sum_cnt == N - 1 .or. zero_cnt == N - 1) then
        print *, (N*(N + 1))/2
        stop
    end if
    !尺取り法で全パターンを数え上げ
    do i = 1, N - 1
        do j = i + 1, N - 1
            if (A_diff(i) == A_diff(j)) then
                !print *, i, j
                cnt = cnt + 1
            else
                exit
            end if
        end do
    end do

    !結果の出力
    print *, cnt
end program abc369c
