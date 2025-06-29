program ABC412a
    ! N            : 日数
    ! i            : ループ用インデックス
    ! goal         : その日の目標タスク数 A_i
    ! done         : その日の実績タスク数 B_i
    ! count_over   : 目標より多く完了した日数を数えるカウンタ

    implicit none
    integer :: N, i, goal, done, count_over

    ! 入力: N 日間
    read (*, *) N

    count_over = 0
    do i = 1, N
        ! 各日ごとに目標と実績を読み込む
        read (*, *) goal, done
        if (done > goal) then
            count_over = count_over + 1
        end if
    end do

    ! 出力: 目標超過日数
    print *, count_over

end program ABC412a
