program abc364b
    !H      :グリットの行
    !W      :グリットの列
    !S      :グリットの各マス
    !X      :移動方向
    !len_X  ：総移動回数
    !xx     :現在マスのx座標
    !yy     :現在マスのy座標
    !move_x :x方向の移動量
    !move_y :y方向の移動量
    !move_TF:移動可能の可否
    implicit none
    integer i, j
    integer H, W, len_X, move_x, move_y, xx, yy
    logical move_TF
    character(1), allocatable::S(:, :)
    character(50) X

    !入力
    read (*, *) H, W
    read (*, *) yy, xx
    allocate (S(H, W))
    do i = 1, H !横
        read (*, '(*(a1))') (S(i, j), j=1, W)
    end do
    read (*, '(A)') X
    len_X = len_trim(X)

    !マス移動のシミュレーション
    move_TF = .true.
    do i = 1, len_X
        !進行方向の決定
        select case (X(i:i))
        case ("L")
            move_x = -1
            move_y = 0
        case ("R")
            move_x = 1
            move_y = 0
        case ("U")
            move_x = 0
            move_y = -1
        case ("D")
            move_x = 0
            move_y = 1
        end select
        !範囲外のマスに移動していないか
        if (xx + move_x < 1 .or. xx + move_x > W) move_TF = .false.
        if (yy + move_y < 1 .or. yy + move_y > H) move_TF = .false.
        !移動先が空きであるか
        if (move_TF .eqv. .true. .and. S(yy + move_y, xx + move_x) /= ".") move_TF = .false.
        !マスの移動
        if (move_TF .eqv. .true.) then
            xx = xx + move_x
            yy = yy + move_y
        end if
        move_TF = .true.
    end do

    !結果の出力
    write (*, '(i0,1x,i0)') yy, xx
end program abc364b

