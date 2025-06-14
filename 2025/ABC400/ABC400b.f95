program ABC400b
    ! N        : 底となる正整数
    ! M        : 級数の上限（非負整数）
    ! term     : 現在の項 N^i を保持する変数（64ビット整数）
    ! total    : これまでの項の総和 X を保持する変数（64ビット整数）
    ! overflow : 総和が 10^9 を超えたかどうかのフラグ
    ! i        : 繰り返し用ループカウンタ

    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    integer(int64) :: N
    integer       :: M, i
    integer(int64) :: term, total
    logical        :: overflow

    ! 入力
    read (*, *) N, M
    term = 1_int64
    total = 1_int64
    overflow = .false.

    ! i = 1 から M まで各項を足し合わせる
    do i = 1, M
        term = term*N
        total = total + term
        if (total > 1000000000_int64) then
            overflow = .true.
            exit
        end if
    end do

    ! 結果の出力
    if (overflow) then
        write (*, '(A)') 'inf'
    else
        write (*, '(I0)') total
    end if

end program ABC400b
