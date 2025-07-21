program abc414a
    ! N            : リスナーの人数
    ! L, R         : 高橋君の配信開始時刻と終了時刻
    ! X(100), Y(100) : 各リスナーの視聴可能時間の開始と終了
    ! i            : ループ変数
    ! count        : 高橋君の配信を全て見られるリスナーの人数
    implicit none
    integer :: N, L, R
    integer :: X(100), Y(100)
    integer :: i, count

    ! 入力
    read (*, *) N, L, R
    do i = 1, N
        read (*, *) X(i), Y(i)
    end do

    count = 0

    ! 条件を満たすリスナーを数える
    do i = 1, N
        if (X(i) <= L .and. R <= Y(i)) then
            count = count + 1
        end if
    end do

    ! 結果の出力
    print *, count

end program abc414a
