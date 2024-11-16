program abc380c
    ! N      : 入力文字列 S の長さ
    ! K      : 移動対象となる 1 の塊の番号
    ! cnt1   : 現在の 1 の塊の長さをカウントする変数
    ! now    : 現在の出力位置を管理する変数
    ! S      : 入力された 0 と 1 からなる長さ N の配列
    ! start1 : 各 1 の塊の開始位置を格納する配列
    ! num1   : 各 1 の塊の長さを格納する配列
    implicit none
    integer(16) i, j1, jj
    integer(16) N, K, cnt1, now
    integer(16), allocatable :: S(:), start1(:), num1(:)

    ! 入力
    read (*, *) N, K
    allocate (S(N), start1(N), num1(N))
    read (*, '(*(i1))') (S(i), i=1, N)

    ! 1 の塊の開始位置と長さを計算
    start1 = 0
    num1 = 0
    i = 1
    j1 = 0
    do
        ! 1 の場合
        if (S(i) == 1) then
            j1 = j1 + 1
            cnt1 = 1
            start1(j1) = i
            do
                i = i + 1
                if (S(i) == 1) then
                    cnt1 = cnt1 + 1
                else
                    num1(j1) = cnt1
                    exit
                end if
            end do
        else
            i = i + 1
        end if
        ! 終了判定
        if (i > N) exit
    end do

    ! K 番目の 1 の塊を移動する処理
    start1(K) = start1(K - 1) + num1(K - 1)

    ! 結果の出力
    i = 1
    now = 1
    do
        if (now == start1(i)) then
            ! 1 の出力
            do jj = 1, num1(i)
                write (*, '(i1)', advance='no') 1
            end do
            now = now + num1(i)
            i = i + 1
        else
            do jj = now, start1(i) - 1
                write (*, '(i1)', advance='no') 0
                now = now + 1
            end do
        end if
        if (i > j1) then
            do jj = now, N
                write (*, '(i1)', advance='no') 0
            end do
            exit
        end if
    end do
end program abc380c
