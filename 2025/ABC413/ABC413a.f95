program abc413a
    ! N             : 品物の個数
    ! M             : カバンの大きさ
    ! A(100)        : 品物の大きさの配列
    ! i             : ループ変数
    ! total         : 品物の合計の大きさ

    implicit none
    integer :: N, M
    integer :: A(100)
    integer :: i, total

    ! 入力
    read (*, *) N, M
    read (*, *) (A(i), i=1, N)

    total = 0

    ! 合計を計算
    do i = 1, N
        total = total + A(i)
    end do

    if (total <= M) then
        print *, 'Yes'
    else
        print *, 'No'
    end if

end program abc413a
