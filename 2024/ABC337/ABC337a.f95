program abc336a
    !N    ：試合回数 N
    !X    ：チーム高橋の点数
    !Y    ：チーム青木の点数
    !Sum_X：チーム高橋の合計点数
    !Sum_Y：チーム青木の合計点数
    implicit none
    integer N, i, Sum_X, Sum_Y
    integer, allocatable::X(:), Y(:)

    !入力
    read (*, *) N
    allocate (X(N), Y(N))
    do i = 1, N
        read (*, *) X(i), Y(i)
    end do

    !得点集計
    Sum_X = sum(X)
    Sum_Y = sum(Y)

    !結果の出力
    if (Sum_X > Sum_Y) then
        write (*, *) "Takahashi"
    else if (Sum_X < Sum_Y) then
        write (*, *) "Aoki"
    else
        write (*, *) "Draw"
    end if
end program abc336a
