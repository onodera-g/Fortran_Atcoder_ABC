program abc341c
    !H  ：縦のマス数
    !W  ：横のマス数
    !S  ：各マスの陸(.)海(#)
    !N  ：移動するマス数
    !T  ：各ターンでの移動方向
    !x  ：現在のx座標
    !y  ：現在のy座標
    !ans：最後まで移動できたパターン数
    implicit none
    integer H, W, N, y, x, ans, i, j, k
    character(1000) :: T
    character(1000), dimension(:), allocatable :: S

    !入力
    read (*, *) H, W, N
    read (*, *) T
    allocate (S(H))
    do i = 1, H
        read (*, *) S(i)
    end do

    !宇宙船のマス移動
    ans = 0
    do i = 1, H
        do j = 1, W
            y = i; x = j
            if (S(y) (x:x) == '#') cycle !スタート位置が「#」ならスキップ
            do k = 1, N
                select case (T(k:k)) !指示の種類に応じて移動
                case ('L')
                    x = x - 1
                case ('R')
                    x = x + 1
                case ('U')
                    y = y - 1
                case ('D')
                    y = y + 1
                end select
                if (y < 1 .or. y > H .or. x < 1 .or. x > W .or. S(y) (x:x) /= '.') exit !移動先が「＃」なら途中終了
            end do
            if (k > N) ans = ans + 1 !最後まで移動できているなら+1
        end do
    end do

    !結果の出力
    write (*, *) ans
end program abc341c
