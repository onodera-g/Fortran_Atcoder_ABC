program abc374d
    ! 変数の説明
    ! N: 線分の数
    ! S: 非照射時の移動速度
    ! T: 照射時の印字速度
    ! i: ループカウンタ
    ! x1, y1: 各線分の一方の端点の座標 (x座標とy座標)
    ! x2, y2: 各線分のもう一方の端点の座標 (x座標とy座標)
    ! used: 各線分が印字されたかどうかを記録するフラグ
    ! min_time: 印字にかかる最小時間を記録する変数
    implicit none
    integer :: N, S, T, i
    real(8) :: x1(6), y1(6), x2(6), y2(6)
    logical :: used(6)
    real(8) :: min_time

    ! 入力
    read (*, *) N, S, T
    do i = 1, N
        read (*, *) x1(i), y1(i), x2(i), y2(i)
    end do
    used = .false.

    ! 最小時間の初期化と探索の開始
    min_time = 1.0d20
    call Search(N, S, T, x1, y1, x2, y2, used, 0.0d0, 0.0d0, 0.0d0, 0, min_time)

    ! 結果の出力
    print '(F20.10)', min_time

contains

    ! 線分の順序と方向を再帰的に探索し、最小時間を求めるサブルーチン
    recursive subroutine Search(N, S, T, x1, y1, x2, y2, used, acc, cx, cy, count, min_time)
        ! N: 線分の数
        ! S: 非照射時の移動速度
        ! T: 照射時の印字速度
        ! x1, y1, x2, y2: 各線分の両端の座標
        ! used: 各線分が印字済みかどうかのフラグ
        ! acc: 現在までの累積時間
        ! cx, cy: 現在のレーザ照射位置
        ! count: 印字済みの線分数
        ! min_time: 見つかった最小時間
        integer, intent(in) :: N, S, T
        real(8), intent(in) :: x1(N), y1(N), x2(N), y2(N)
        logical, intent(inout) :: used(N)
        real(8), intent(in) :: acc, cx, cy
        integer, intent(in) :: count
        real(8), intent(inout) :: min_time
        integer :: i, ch
        real(8) :: tx, ty, ex, ey, move_dist, move_time, print_dist, print_time, total_time

        ! 全ての線分を印字した場合の処理
        if (count == N) then
            if (acc < min_time) min_time = acc
            return
        end if

        ! 未使用の線分を探索
        do i = 1, N
            if (.not. used(i)) then
                used(i) = .true.

                ! 線分の始点から終点、またはその逆の2通りの印字方向を探索
                do ch = 0, 1
                    if (ch == 0) then
                        tx = x1(i); ty = y1(i)
                        ex = x2(i); ey = y2(i)
                    else
                        tx = x2(i); ty = y2(i)
                        ex = x1(i); ey = y1(i)
                    end if

                    ! 移動距離と時間を計算
                    move_dist = sqrt((tx - cx)**2 + (ty - cy)**2)
                    move_time = move_dist/S

                    ! 線分の印字距離と時間を計算
                    print_dist = sqrt((ex - tx)**2 + (ey - ty)**2)
                    print_time = print_dist/T

                    ! 累積時間を計算
                    total_time = acc + move_time + print_time

                    ! 最小時間の更新
                    if (total_time < min_time) then
                        call Search(N, S, T, x1, y1, x2, y2, used, total_time, ex, ey, count + 1, min_time)
                    end if
                end do

                ! 探索終了後に線分を未使用状態に戻す
                used(i) = .false.
            end if
        end do
    end subroutine Search

end program abc374d
