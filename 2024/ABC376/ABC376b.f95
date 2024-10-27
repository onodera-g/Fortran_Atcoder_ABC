program abc376b
    ! N: リングのパーツ数 (3 ≤ N)
    ! Q: 指示の回数 (1 ≤ Q)
    ! i: ループカウンタ、各指示を処理するために使用
    ! T: 指定されたパーツ番号
    ! left: 現在の左手が握っているパーツの番号
    ! right: 現在の右手が握っているパーツの番号
    ! total: 合計の移動回数を記録
    ! cw: 時計回りで目的のパーツに到達するための移動回数
    ! ccw: 反時計回りで目的のパーツに到達するための移動回数
    ! pos: 時計回りおよび反時計回りで現在の位置を追跡するために使用
    ! hand: 指示で指定された手 ('L' か 'R')
    implicit none
    integer :: N, Q, i, T, left, right, total, cw, ccw, pos
    character(len=1) :: hand

    total = 0
    left = 1
    right = 2
    read (*, *) N, Q
    do i = 1, Q
        read (*, *) hand, T
        if (hand == 'L') then
            ! 時計回りの距離計算
            cw = 0
            pos = left
            do while (pos /= T)
                pos = mod(pos, N) + 1
                cw = cw + 1
                if (pos == right) exit
            end do
            if (pos /= T) then
                ! 反時計回りの距離計算
                ccw = 0
                pos = left
                do while (pos /= T)
                    pos = mod(pos - 2 + N, N) + 1
                    ccw = ccw + 1
                    if (pos == right) exit
                end do
                cw = N + 1
            else
                ccw = N + 1
            end if
            total = total + min(cw, ccw)
            left = T
        else
            ! 時計回りの距離計算
            cw = 0
            pos = right
            do while (pos /= T)
                pos = mod(pos, N) + 1
                cw = cw + 1
                if (pos == left) exit
            end do
            if (pos /= T) then
                ! 反時計回りの距離計算
                ccw = 0
                pos = right
                do while (pos /= T)
                    pos = mod(pos - 2 + N, N) + 1
                    ccw = ccw + 1
                    if (pos == left) exit
                end do
                cw = N + 1
            else
                ccw = N + 1
            end if
            total = total + min(cw, ccw)
            right = T
        end if
    end do

    ! 結果の出力
    print *, total
end program abc376b
