program ABC405c
    ! N       : 整数列 A の要素数
    ! A       : 整数列（長さ N）
    ! i       : ループ用インデックス
    ! prefix  : これまでの A(1)+...+A(i-1) の累積和
    ! ans     : Σ_{1 ≤ j ≤ N} A(j) * (A(1)+...+A(j-1)) を蓄積する

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    integer :: N
    integer :: i
    integer(int64) :: ans
    integer(int64) :: prefix
    integer(int64), allocatable :: A(:)

    ! 入力
    read (*, *) N
    allocate (A(N))
    read (*, *) (A(i), i=1, N)

    ! 計算
    ans = 0_int64
    prefix = 0_int64
    do i = 1, N
        ans = ans + A(i)*prefix
        prefix = prefix + A(i)
    end do

    ! 結果の出力
    print *, ans

end program ABC405c
