program ABC313b
    !N  ：プログラマ人数
    !M  ：強さに関する情報の総数
    !A,B：プログラマの強さに関する情報(Aの方が強い)
    !deg：各頂点の次数を格納する配列
    !ans：最強のプログラマー番号

    implicit none
    integer :: N, M
    integer, dimension(:), allocatable :: deg
    integer :: i, A, b, ans

    ! 入力
    read (*, *) N, M
    allocate (deg(N))
    deg = 0

    !負けの記録
    do i = 1, M
        read (*, *) a, b
        deg(b) = deg(b) + 1
    end do

    ans = -1
    !負けが0のプログラマの検索
    do i = 1, N
        if (deg(i) == 0) then
            if (ans /= -1) then !すでに負けが０のプログラマがいる場合(=最強を特定できない)
                print *, -1
                stop
            else
                ans = i
            end if
        end if
    end do

    ! 結果を出力
    if (ans == -1) then
        print *, -1
    else
        print *, ans
    end if

end program ABC313b
