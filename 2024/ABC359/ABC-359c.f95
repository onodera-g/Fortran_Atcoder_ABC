program abc359c
    !Sx ：スタート位置のX座標
    !Sy ：スタート位置のY座標
    !Tx ：ゴール位置のX座標
    !Ty ：ゴール位置のX座標
    !X  ：X軸方向の移動量
    !Y  ：Y軸方向の移動量
    !ans：合計の移動量
    implicit none
    integer, parameter :: int64 = selected_int_kind(18)
    integer(kind=int64) :: Sx, Sy, Tx, Ty
    integer(kind=int64) :: x, y, ans

    !入力
    read (*, *) Sx, Sy
    read (*, *) Tx, Ty

    !左によっている分を右に寄せる
    if (mod(Sx + Sy, int(2, int64)) == int(1, int64)) Sx = Sx - int(1, int64)
    if (mod(Tx + Ty, int(2, int64)) == int(1, int64)) Tx = Tx - int(1, int64)

    !x,y軸方向の移動量計算
    x = abs(Sx - Tx)
    y = abs(Sy - Ty)

    !合計の移動量の計算
    if (x < y) then
        ans = y
    else
        ans = (x + y)/2
    end if

    !結果の出力
    write (*, '(i0)') ans
end program abc359c
