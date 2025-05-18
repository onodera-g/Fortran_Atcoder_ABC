program abc387b
    ! X                : 入力として与えられる整数 (1～81)
    ! total_sum        : 9×9の掛け算表における全81個の整数の総和
    ! count_x          : X と等しい値が書かれたマスの個数
    ! i, j             : 掛け算表を走査するループ変数 (1～9)
    implicit none
    integer :: X
    integer :: total_sum
    integer :: count_x
    integer :: i, j

    ! 入力
    read (*, *) X

    ! 掛け算表の和と X の出現回数を数える
    total_sum = 0
    count_x = 0
    do i = 1, 9
        do j = 1, 9
            total_sum = total_sum + i*j
            if (i*j == X) then
                count_x = count_x + 1
            end if
        end do
    end do

    ! 結果の出力
    print *, total_sum - count_x*X
end program abc387b
