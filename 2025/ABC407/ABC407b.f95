program ABC407b
    ! X          : 2つのサイコロの出目の和の閾値 (2 ≤ X ≤ 13)
    ! Y          : 2つのサイコロの出目の差の絶対値の閾値 (0 ≤ Y ≤ 6)
    ! i, j       : 各サイコロの出目を走査するループ変数 (1～6)
    ! favorable  : 条件を満たす出目の組み合わせ数
    ! total      : サイコロの全組み合わせ数 (6×6 = 36)
    ! prob       : 求める確率
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none

    integer(int32), parameter :: total = 36
    integer(int32) :: X, Y
    integer(int32) :: i, j, favorable
    real(real64)   :: prob

    ! 入力
    read (*, *) X, Y

    ! 条件を満たす組み合わせをカウント
    favorable = 0
    do i = 1, 6
        do j = 1, 6
            if ((i + j >= X) .or. (abs(i - j) >= Y)) then
                favorable = favorable + 1
            end if
        end do
    end do

    ! 確率を計算
    prob = real(favorable, real64)/real(total, real64)

    ! 出力
    print *, prob

end program ABC407b
