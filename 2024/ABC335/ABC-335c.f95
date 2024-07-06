program abc335c
    !N    ：パーツの数
    !Q    ：処理するクエリの数
    !query：クエリの内容
    !arr  ：頭パーツの過去の位置(x,y)
    !x,y  ：頭パーツの現在の位置(x,y)
    implicit none
    integer(16) N, Q, i, cnt, tmp, x, y
    integer(16), allocatable::query_1(:), arr(:, :)
    character(100000), allocatable::query_2(:)

    !入力
    read (*, *) N, Q
    allocate (query_1(Q), query_2(Q), arr(N + Q, 2))
    do i = 1, Q
        read (*, *) query_1(i), query_2(i)
    end do

    !座標の初期化
    arr = 0; cnt = N
    do i = 1, N
        arr(i, 1) = N + 1 - i
        arr(i, 2) = 0
    end do
    x = 1; y = 0

    !クエリの処理
    do i = 1, Q
        if (query_1(i) == 1) then
            select case (query_2(i))
            case ("R")
                x = x + 1
            case ("L")
                x = x - 1
            case ("U")
                y = y + 1
            case ("D")
                y = y - 1
            end select
            cnt = cnt + 1
            arr(cnt, 1) = x; arr(cnt, 2) = y
        else
            read (query_2(i), *) tmp
            write (*, *) arr(cnt - (tmp - 1), 1), arr(cnt - (tmp - 1), 2)
        end if
    end do
end program abc335c
