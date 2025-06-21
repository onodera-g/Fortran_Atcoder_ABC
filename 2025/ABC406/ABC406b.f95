program ABC406b
    ! n   : 操作の回数
    ! k   : 電卓が表示できる桁数
    ! a(i): i 回目の操作で掛ける正の整数 A_i
    ! x   : 現在電卓に表示されている数
    ! y   : 電卓が桁あふれせず表示できる最大値 (10^k − 1)

    use, intrinsic :: iso_fortran_env, only: int32, int64
    implicit none
    integer(int32) :: n, k
    integer(int64), allocatable :: a(:)
    integer(int64) :: x, y
    integer(int32) :: i

    ! 入力
    read (*, *) n, k
    allocate (a(n))
    read (*, *) (a(i), i=1, n)

    ! 初期化
    x = 1_int64
    y = 10_int64**k - 1_int64

    ! 各操作をシミュレート
    do i = 1, n
        if (x > y/a(i)) then
            x = 1_int64
        else
            x = x*a(i)
        end if
    end do

    ! 結果の出力
    print '(I0)', x

end program ABC406b
