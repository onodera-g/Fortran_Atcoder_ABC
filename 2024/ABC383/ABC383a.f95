program abc383a
    ! N           : 水を追加する回数
    ! T(i)        : i回目に水を追加する時刻
    ! V(i)        : i回目に追加する水の量（リットル）
    ! water       : 現在加湿器に入っている水の量（リットル）
    ! current_time: 現在の時刻
    implicit none
    integer :: i, N, water, current_time
    integer, allocatable :: T(:), V(:)
    ! 入力
    read (*, *) N
    allocate (T(N), V(N))
    do i = 1, N
        read (*, *) T(i), V(i)
    end do

    ! 初期化
    water = 0
    current_time = 0

    ! i=1からNまで、i回目の水追加処理を行う
    do i = 1, N
        ! (T(i) - current_time)分の時間経過で水が減少
        water = max(0, water - (T(i) - current_time))
        ! i回目の追加分を加える
        water = water + V(i)
        ! 現在時刻を更新
        current_time = T(i)
    end do

    ! 結果の出力
    print *, water
end program abc383a
