program abc380b
    ! N   : 文字列 S の長さ
    ! cnt : '-' の個数をカウントするための変数
    ! A   : 復元された正整数列を格納する可変長配列
    ! S   : 入力された文字列
    implicit none
    integer i, j
    integer N, cnt
    integer, allocatable :: A(:)
    character(100) S

    ! 入力
    read (*, *) S
    N = len_trim(S)
    allocate (A(N))

    ! "-" の数え上げ
    i = 1; j = 0
    do
        if (S(i:i) == "|") then
            cnt = 0
            j = j + 1
            do
                i = i + 1
                if (S(i:i) == "|") then
                    A(j) = cnt
                    exit
                else
                    cnt = cnt + 1
                end if
            end do
        end if
        ! 終了判定
        if (i >= N) then
            exit
        end if
    end do

    ! 結果の出力
    do i = 1, j
        write (*, '(i0,1x)', advance='no') A(i)
    end do
end program abc380b
