program abc375b
    ! N          : 点の数
    ! a, b       : 現在の座標 (X, Y)
    ! c, d       : 次の座標 (X, Y)
    ! dist       : 総移動コスト（距離の総和）
    ! X(:), Y(:) : 各点のX座標とY座標を格納する配列
    implicit none
    integer(16) :: i, N
    real(16) :: a, b, c, d, dist
    real(16), allocatable :: X(:), Y(:)

    ! 入力
    read (*, *) N
    allocate (X(N), Y(N))
    do i = 1, N
        read (*, *) X(i), Y(i)
    end do

    ! 原点から最初の点までの移動
    a = 0; b = 0
    c = X(1); d = Y(1)
    dist = sqrt((a - c)**2 + (b - d)**2)

    ! 各点間の距離の合計を計算
    do i = 1, N - 1
        a = X(i); b = Y(i)
        c = X(i + 1); d = Y(i + 1)
        dist = dist + sqrt((a - c)**2 + (b - d)**2)
    end do

    ! 最後の点から原点への距離
    c = 0; d = 0
    a = X(N); b = Y(N)
    dist = dist + sqrt((a - c)**2 + (b - d)**2)

    ! 結果の出力
    write (*, *) dist

end program abc375b
