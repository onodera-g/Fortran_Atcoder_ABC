module global_variable
    !H,W：グリッドの横、縦のマス数
    !grid：グリッドの色(黒or白)を管理
    integer H, W
    character(1), allocatable::grid(:, :)
end module
program abc339b
    !muki:移動する方向,１：上、２：右、３：下、４：左
    use global_variable
    implicit none
    integer N
    integer i, j, k, muki

    !入力
    read (*, *) H, W, N
    allocate (grid(H, W))
    grid = "."

    !1回目の移動
    j = 1; k = 1
    muki = 1
    grid(1, 1) = "#"

    !2~N回目の移動
    do i = 1, N - 1
        if (grid(j, k) == "#") then
            muki = muki + 1
            call next_grid(j, k, muki)
        else
            muki = muki - 1
            call next_grid(j, k, muki)
        end if
    end do

    !結果の出力
    do i = 1, H
        write (*, *) (grid(i, k), k=1, W)
    end do
contains
    subroutine next_grid(y, x, muki)
        integer x, y, muki

        !向きの管理は1~4なので、一周したら1,4に戻す
        if (muki == 5) muki = 1
        if (muki == 0) muki = 4

        !移動先の座標の決定
        select case (muki)
        case (1) !上
            y = y - 1
        case (2) !右
            x = x + 1
        case (3) !下
            y = y + 1
        case (4) !左
            x = x - 1
        end select

        !次のマスが配列外になっていればループさせる
        if (x > W) x = 1
        if (y > H) y = 1
        if (x < 1) x = W
        if (y < 1) y = H

        !移動先の色を塗る
        if (grid(y, x) == "#") then
            grid(y, x) = "."
        else
            grid(y, x) = "#"
        end if
    end subroutine
end program abc339b
