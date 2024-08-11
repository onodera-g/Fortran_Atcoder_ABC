program abc340b
    !Q：クエリの総数
    !query：クエリ
    !A：クエリを処理した後の配列
    !R：クエリを格納するAの位置管理用
    !xk：クエリの種類、1or2
    implicit none
    integer Q, query, R, xk
    integer i
    integer, allocatable::A(:)

    !入力
    read (*, *) Q
    allocate (A(Q))
    R = Q

    !クエリの処理
    do i = 1, Q
        read (*, *) xk, query
        if (xk == 1) then !１：末尾に追加
            A(R) = query
            R = R - 1
        else !2：出力
            write (*, *) A(R + query)
        end if
    end do
end program abc340b
