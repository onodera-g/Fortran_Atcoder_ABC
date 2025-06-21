program ABC411b
    ! N        : 駅の数
    ! D(i)     : 駅 i と駅 i+1 間の距離 (i=1…N-1)
    ! cum(i)   : 駅 1 から駅 i までの累積距離 (cum(1)=0)
    ! i, j     : ループ用インデックス
    implicit none
    integer :: N, i, j
    integer, allocatable :: D(:), cum(:)

    ! 入力
    read (*, *) N
    allocate (D(N - 1), cum(N))
    read (*, *) (D(i), i=1, N - 1)

    ! 駅 1 から各駅までの累積距離を計算
    cum = 0
    do i = 2, N
        cum(i) = cum(i - 1) + D(i - 1)
    end do

    ! 出力: i 行目に駅 i と駅 i+j の距離を出力
    do i = 1, N - 1
        write (*, '(100(I0,1X))') (cum(i + j) - cum(i), j=1, N - i)
    end do

end program ABC411b
